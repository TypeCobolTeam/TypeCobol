using System.Diagnostics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class InsertVariableDisplayRefactoringProcessor : AbstractRefactoringProcessor
    {
        private string _hash;
        private Position _insertAt;
        private bool _insertBeforeStatement;
        private Selection _workingStorageSectionSelection;
        private Selection _localStorageSectionSelection;
        private Selection _linkageSectionSelection;

        private (CodeElement CodeElement, Node Node) _location;

        public override TextDocumentIdentifier PrepareRefactoring(object[] arguments)
        {
            // Generate new hash for this refactoring
            string allArgs = arguments.Select(argument => argument?.ToString()).Aggregate(string.Empty, string.Concat);
            _hash = Tools.Hash.CreateCOBOLNameHash(allArgs + EnvironmentVariableProvider.Now);

            // Get TextDocumentPosition (contains TextDocumentIdentifier and insertion position)
            var textDocumentPosition = Expect<TextDocumentPosition>(arguments, 0, true);
            _insertAt = textDocumentPosition.position;

            // Get insert before/after flag
            _insertBeforeStatement = Expect<bool>(arguments, 1, true);

            // Get Selection objects
            _workingStorageSectionSelection = GetSelection(2);
            _localStorageSectionSelection = GetSelection(3);
            _linkageSectionSelection = GetSelection(4);

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
            var program = _location.Node.GetProgramNode();
            var dataDivision = program.Children.OfType<DataDivision>().SingleOrDefault();
            var textEdits = new List<TextEdit>();
            if (dataDivision != null)
            {
                var indexGenerator = new IndexGenerator(_hash);

                var dataSections = ExtractDataSections(dataDivision);
                var statementsForWorkingStorageVariables = GenerateDisplayStatements(dataSections.WorkingStorageSection, _workingStorageSectionSelection, indexGenerator);
                var statementsForLocalStorageVariables = GenerateDisplayStatements(dataSections.LocalStorageSection, _localStorageSectionSelection, indexGenerator);
                var statementsForLinkageVariables = GenerateDisplayStatements(dataSections.LinkageSection, _linkageSectionSelection, indexGenerator);

                string openingComment = $"<DBG>{nameof(InsertVariableDisplay)} {EnvironmentVariableProvider.Now:yyyy/MM/dd HH:mm} {EnvironmentVariableProvider.UserName}";
                const string closingComment = "</DBG>";

                var cobolStringBuilder = new CobolStringBuilder(true);
                cobolStringBuilder.AppendCommentSingleLine(openingComment);
                indexGenerator.WriteCobolCode(cobolStringBuilder);
                cobolStringBuilder.AppendCommentSingleLine(closingComment);
                bool hasCodeForIndices = !indexGenerator.IsEmpty;
                string cobolStringForIndices = cobolStringBuilder.ToString();

                cobolStringBuilder.Clear();
                cobolStringBuilder.AppendCommentSingleLine(openingComment);
                statementsForWorkingStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLocalStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLinkageVariables.WriteCobolCode(cobolStringBuilder);
                cobolStringBuilder.AppendCommentSingleLine(closingComment);
                bool hasCodeForStatements = !statementsForWorkingStorageVariables.IsEmpty || !statementsForLocalStorageVariables.IsEmpty || !statementsForLinkageVariables.IsEmpty;
                string cobolStringForStatements = cobolStringBuilder.ToString();

                if (hasCodeForIndices)
                {
                    var targetSection = (DataSection)dataSections.WorkingStorageSection ?? dataSections.LocalStorageSection;
                    if (targetSection == null)
                    {
                        // TODO Either move this into CheckTarget and turn it into a refactoring requirement or generate a WORKING STORAGE ourselves ?
                        return ("Unable to generate indices", new List<TextEdit>());
                    }

                    textEdits.Add(InsertAtEnd(targetSection, cobolStringForIndices));
                }

                if (hasCodeForStatements)
                {
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

            return (textEdits.Count > 0 ? "Debug instructions successfully generated." : "No modification", textEdits);
        }

        private static (WorkingStorageSection WorkingStorageSection, LocalStorageSection LocalStorageSection, LinkageSection LinkageSection) ExtractDataSections(DataDivision dataDivision)
        {
            WorkingStorageSection workingStorageSection = null;
            LocalStorageSection localStorageSection = null;
            LinkageSection linkageSection = null;
            foreach (var dataSection in dataDivision.Children)
            {
                switch (dataSection)
                {
                    case WorkingStorageSection workingStorage:
                        workingStorageSection = workingStorage;
                        break;
                    case LocalStorageSection localStorage:
                        localStorageSection = localStorage;
                        break;
                    case LinkageSection linkage:
                        linkageSection = linkage;
                        break;
                }
            }

            return (workingStorageSection, localStorageSection, linkageSection);
        }

        private static GeneratedRoot GenerateDisplayStatements(DataSection dataSection, Selection rootSelection, IndexGenerator indexGenerator)
        {
            if (dataSection == null || rootSelection == null)
            {
                return new GeneratedRoot();
            }

            var visitor = new DataDefinitionToDisplayVisitor(rootSelection, indexGenerator);
            visitor.Visit(dataSection);
            return visitor.GeneratedStatements;
        }

        private static TextEdit InsertBefore(Node node, string code)
        {
            Debug.Assert(node.CodeElement != null);
            int line = node.CodeElement.Line;
            int character = 0;
            return TextEdit.Insert(new Position() { line = line, character = character }, code + Environment.NewLine);
        }

        private static TextEdit InsertAfter(Node node, string code)
        {
            Debug.Assert(node.CodeElement != null);
            int line = node.CodeElement.LineEnd;
            int character = node.CodeElement.StopIndex + 1;
            return TextEdit.Insert(new Position() { line = line, character = character }, Environment.NewLine + code);
        }

        private static TextEdit InsertAtEnd(Node node, string code) => InsertAfter(node.GetLastNode(), code);
    }
}
