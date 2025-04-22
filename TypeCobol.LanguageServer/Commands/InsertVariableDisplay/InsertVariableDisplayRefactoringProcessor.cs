using System.Diagnostics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
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
        private (CodeElement CodeElement, Node Node) _location;

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
            _location = CodeElementLocator.FindCodeElementAt(compilationUnit, _insertAt);
            if (_location.CodeElement == null)
            {
                throw new InvalidOperationException("Unable to locate program to modify.");
            }
        }

        public override (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit)
        {
            // Get program DATA DIVISION
            var program = _location.Node.GetProgramNode();
            var dataDivision = program.Children.OfType<DataDivision>().SingleOrDefault();
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

                if (hasCodeForIndices)
                {
                    // Insert COBOL code for indices
                    var targetSection = (DataSection)dataDivision.WorkingStorageSection ?? dataDivision.LocalStorageSection;
                    if (targetSection == null)
                    {
                        // TODO Either move this into CheckTarget and turn it into a global refactoring requirement or generate a WORKING STORAGE ourselves ?
                        return ("Unable to generate indices: neither WORKING-STORAGE SECTION nor LOCAL-STORAGE SECTION could be found to add generated indices.", new List<TextEdit>());
                    }

                    textEdits.Add(InsertAtEnd(targetSection, cobolStringForIndices));
                }

                if (hasCodeForStatements)
                {
                    // Insert COBOL code for statements
                    if (_insertBeforeStatement)
                    {
                        textEdits.Add(InsertBefore(_location.Node, cobolStringForStatements));
                    }
                    else
                    {
                        textEdits.Add(InsertAfter(_location.Node, cobolStringForStatements));
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

        private static TextEdit InsertBefore(Node node, string code)
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
            }

            return TextEdit.Insert(new Position() { line = line, character = character }, newText);
        }

        private static TextEdit InsertAfter(Node node, string code)
        {
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
            }

            return TextEdit.Insert(new Position() { line = line, character = character }, newText);
        }

        private static string BeginLine(char indicator, int column)
            => $"{CobolStringBuilder.SequenceNumber}{indicator}{new string(' ', column - CobolStringBuilder.SequenceNumber.Length - 1)}";

        private static TextEdit InsertAtEnd(Node node, string code) => InsertAfter(node.GetLastNode(), code);
    }
}
