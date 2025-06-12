using System.Diagnostics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    public class InsertVariableDisplayRefactoringProcessor : AbstractRefactoringProcessor
    {
        // From refactoring args
        private Position _insertAt;
        private bool _insertBeforeStatement;
        private Selection _workingStorageSectionSelection;
        private Selection _localStorageSectionSelection;
        private Selection _linkageSectionSelection;

        // Generated using user and timestamp
        private string _hash;

        // Computed when checking target program
        private Node _location;

        public override TextDocumentIdentifier PrepareRefactoring(object[] arguments)
        {
            // Get TextDocumentPosition (contains TextDocumentIdentifier and insertion position)
            var textDocumentPosition = Expect<TextDocumentPosition>(arguments, 0, true);
            _insertAt = textDocumentPosition.position;

            // Get insert before/after flag
            _insertBeforeStatement = Expect<bool>(arguments, 1, true);

            // Get Selection objects
            _workingStorageSectionSelection = GetSelection(2);
            _localStorageSectionSelection = GetSelection(3);
            _linkageSectionSelection = GetSelection(4);

            // Generate new hash for this refactoring
            _hash = Tools.Hash.CreateCOBOLNameHash($"{EnvironmentVariableProvider.UserName}@{EnvironmentVariableProvider.Now:yyyy/MM/dd HH:mm:ss.fff}");

            return textDocumentPosition.textDocument;

            Selection GetSelection(int index)
            {
                var jsonSelection = Expect<JsonSelection>(arguments, index, false);
                return jsonSelection?.Convert();
            }
        }

        public override void CheckTarget(CompilationUnit compilationUnit)
        {
            // Require full AST
            if (compilationUnit.ProgramClassDocumentSnapshot == null)
            {
                throw new InvalidOperationException($"Could not get AST for program '{compilationUnit.TextSourceInfo.Name}'.");
            }

            // Check valid insertion location
            _location = CodeElementLocator.FindCodeElementAt(compilationUnit, _insertAt).Node;
            if (_location == null)
            {
                throw new InvalidOperationException("Unable to locate DISPLAY insertion location.");
            }
        }

        public override (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit)
        {
            // Get program DATA DIVISION
            var program = _location.GetProgramNode();
            var dataDivision = program?.Children.OfType<DataDivision>().SingleOrDefault();
            var textEdits = new List<TextEdit>();
            if (dataDivision != null)
            {
                // Generate DISPLAY statements for each data section. Required indices are tracked by IndexGenerator, shared between generations.
                var indexGenerator = new IndexGenerator(_hash);
                var statementsForWorkingStorageVariables = GenerateDisplayStatements(dataDivision.WorkingStorageSection, _workingStorageSectionSelection, indexGenerator);
                var statementsForLocalStorageVariables = GenerateDisplayStatements(dataDivision.LocalStorageSection, _localStorageSectionSelection, indexGenerator);
                var statementsForLinkageVariables = GenerateDisplayStatements(dataDivision.LinkageSection, _linkageSectionSelection, indexGenerator);

                // The statements and the indices are surrounded by standardized comment
                string openingComment = $"<DBG>{nameof(InsertVariableDisplay)} {EnvironmentVariableProvider.Now:yyyy/MM/dd HH:mm} {EnvironmentVariableProvider.UserName}";
                const string closingComment = "</DBG>";

                // Get COBOL code for required indices
                var cobolStringBuilder = new CobolStringBuilder(true);
                cobolStringBuilder.AppendCommentSingleLine(openingComment);
                indexGenerator.WriteCobolCode(cobolStringBuilder);
                cobolStringBuilder.AppendCommentSingleLine(closingComment);
                bool hasCodeForIndices = indexGenerator.HasContent;
                string cobolStringForIndices = cobolStringBuilder.ToString();

                // Reset builder and get COBOL code for DISPLAY statements (from all 3 sections)
                cobolStringBuilder.Clear();
                cobolStringBuilder.AppendCommentSingleLine(openingComment);
                statementsForWorkingStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLocalStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLinkageVariables.WriteCobolCode(cobolStringBuilder);
                cobolStringBuilder.AppendCommentSingleLine(closingComment);
                bool hasCodeForStatements = statementsForWorkingStorageVariables.HasContent || statementsForLocalStorageVariables.HasContent || statementsForLinkageVariables.HasContent;
                string cobolStringForStatements = cobolStringBuilder.ToString();

                var codeLines = compilationUnit.ProgramClassDocumentSnapshot.PreviousStepSnapshot.Lines;

                if (hasCodeForIndices)
                {
                    // Insert COBOL code for indices
                    var targetSection = (DataSection)dataDivision.WorkingStorageSection ?? dataDivision.LocalStorageSection;
                    if (targetSection == null)
                    {
                        // TODO Either move this into CheckTarget and turn it into a global refactoring requirement or generate a WORKING STORAGE ourselves ?
                        return ("Unable to generate indices: neither WORKING-STORAGE SECTION nor LOCAL-STORAGE SECTION could be found to add generated indices.", new List<TextEdit>());
                    }

                    textEdits.Add(InsertAtEnd(codeLines, targetSection, cobolStringForIndices));
                }

                if (hasCodeForStatements)
                {
                    // Insert COBOL code for statements
                    if (_insertBeforeStatement && _location is not ProcedureDivision) // Avoid inserting outside PROCEDURE DIVISION
                    {
                        textEdits.Add(InsertBefore(codeLines, _location, cobolStringForStatements));
                    }
                    else
                    {
                        textEdits.Add(InsertAfter(codeLines, _location, cobolStringForStatements));
                    }
                }
            }

            return (textEdits.Count > 0 ? "Debug instructions successfully generated." : "No modification.", textEdits);
        }

        private static GeneratedRoot GenerateDisplayStatements(DataSection dataSection, Selection rootSelection, IndexGenerator indexGenerator)
        {
            if (dataSection == null || rootSelection == null)
            {
                // Nothing to generate
                return new GeneratedRoot();
            }

            // Create new visitor and visit the whole data section using the corresponding selection
            var visitor = new DataDefinitionToDisplayVisitor(rootSelection, indexGenerator);
            visitor.Visit(dataSection);
            return visitor.GeneratedStatements;
        }

        private static TextEdit InsertBefore(ISearchableReadOnlyList<ICodeElementsLine> codeLines, Node node, string code)
        {
            Debug.Assert(node.CodeElement != null);
            int line = node.CodeElement.Line; // On same line and inserted text ends with a line break
            string newText = code + Environment.NewLine;
            int character = 0; // At beginning of the line, except when CodeElement is not the first

            var insertionToken = node.CodeElement.ConsumedTokens.FirstOrDefault();
            if (insertionToken != null)
            {
                // Is the first token of the CodeElement also the first token on the line ?
                var tokensLine = insertionToken.TokensLine;
                if (insertionToken != tokensLine.SourceTokens.First())
                {
                    // Insertion point is right before CodeElement
                    character = node.CodeElement.StartIndex;
                    // Start a new line and align text located beyond insertion point on its current column
                    newText = $"{Environment.NewLine}{newText}{BeginLine(tokensLine.IndicatorChar, character)}";
                }
                else
                {
                    // Nothing before insertion point, check for comments preceding the insertion line
                    int lineIndex = line - 2; // Start with the index of the line before insertion line (line is 1-based whereas lineIndex is 0-based)
                    while (lineIndex >= 0)
                    {
                        var codeLine = codeLines[lineIndex];
                        if (codeLine.Type is CobolTextLineType.Comment or CobolTextLineType.MultiFormalizedComment)
                        {
                            // Insert before this comment/debug line
                            line = lineIndex + 1;
                            character = 0;
                            lineIndex--;
                        }
                        else
                        {
                            // Source line: keep current insertion position
                            break;
                        }
                    }
                }
            }

            return TextEdit.Insert(new Position() { line = line, character = character }, newText);
        }

        private static TextEdit InsertAfter(ISearchableReadOnlyList<ICodeElementsLine> codeLines, Node node, string code)
        {
            // Do not insert inside statements having a body (statement with nested statements)
            // -> Reposition at the end of the whole statement (on its matching END-xxx node).
            if (node is StatementWithBody) node = node.GetLastNode();

            Debug.Assert(node != null);
            Debug.Assert(node.CodeElement != null);
            int line = node.CodeElement.LineEnd; // On same line and inserted text starts with a line break:
            string newText = Environment.NewLine + code;
            int character = node.CodeElement.StopIndex + 1; // At CodeElement end

            var insertionToken = node.CodeElement.ConsumedTokens.LastOrDefault();
            if (insertionToken != null)
            {
                // Is there anything after insertion point ?
                var tokensLine = insertionToken.TokensLine;
                if (tokensLine.Length > character)
                {
                    // Align text located beyond insertion point on its current column
                    newText += BeginLine(tokensLine.IndicatorChar, character);
                }
                else
                {
                    // Nothing after insertion point, check for comments following the insertion line
                    int lineIndex = line; // Initialize with insertion line which is 1-based, thus we start here with the index of the line after insertion line
                    while (lineIndex < codeLines.Count)
                    {
                        var codeLine = codeLines[lineIndex];
                        if (codeLine.Type is CobolTextLineType.Comment) // Do not consider MultiFormalizedComment as they are attached to the code following them
                        {
                            // Insert after this comment/debug line
                            line = ++lineIndex;
                            character = codeLine.Length;
                        }
                        else
                        {
                            // Source line: keep current insertion position
                            break;
                        }
                    }
                }
            }

            return TextEdit.Insert(new Position() { line = line, character = character }, newText);
        }

        private static string BeginLine(char indicator, int column)
            => $"{CobolStringBuilder.SequenceNumber}{indicator}{new string(' ', column - CobolStringBuilder.SequenceNumber.Length - 1)}";

        private static TextEdit InsertAtEnd(ISearchableReadOnlyList<ICodeElementsLine> codeLines, Node node, string code)
            => InsertAfter(codeLines, node.GetLastNode(), code);
    }
}
