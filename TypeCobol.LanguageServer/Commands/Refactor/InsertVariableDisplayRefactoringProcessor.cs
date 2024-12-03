using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class InsertVariableDisplayRefactoringProcessor : IRefactoringProcessor
    {
        private string _hash;
        private Position _insertAt;
        private bool _insertBeforeStatement;
        private Selection _workingStorageSectionSelection;
        private Selection _localStorageSectionSelection;
        private Selection _linkageSectionSelection;

        public TextDocumentIdentifier PrepareRefactoring(object[] arguments)
        {
            // Generate new hash for this refactoring
            string allArgs = arguments.Select(argument => argument.ToString()).Aggregate(string.Empty, string.Concat);
            _hash = Tools.Hash.CreateCOBOLNameHash(allArgs + DateTime.Now);

            // Get TextDocumentPosition (contains TextDocumentIdentifier and insertion position)
            var textDocumentPosition = IRefactoringProcessor.Expect<TextDocumentPosition>(arguments, 0, true);
            _insertAt = textDocumentPosition.position;

            // Get insert before/after flag
            _insertBeforeStatement = IRefactoringProcessor.Expect<bool>(arguments, 1, true);

            // Get Selection objects
            _workingStorageSectionSelection = GetSelection(2);
            _localStorageSectionSelection = GetSelection(3);
            _linkageSectionSelection = GetSelection(4);

            return textDocumentPosition.textDocument;

            Selection GetSelection(int index)
            {
                var jsonSelection = IRefactoringProcessor.Expect<JsonSelection>(arguments, index, false);
                return jsonSelection?.Convert();
            }
        }

        public void CheckTarget(CompilationUnit compilationUnit)
        {
            // Require full AST
            if (compilationUnit.ProgramClassDocumentSnapshot == null)
            {
                throw new InvalidOperationException($"Could not get AST for program '{compilationUnit.TextSourceInfo.Name}'.");
            }
        }

        public (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit)
        {
            var location = CodeElementLocator.FindCodeElementAt(compilationUnit, _insertAt);
            if (location.CodeElement == null)
            {
                // TODO Improve this ?
                return ("Unable to locate program", new List<TextEdit>());
            }

            var program = location.Node.GetProgramNode();
            var dataDivision = program.Children.OfType<DataDivision>().SingleOrDefault();
            if (dataDivision != null)
            {
                var indexGenerator = new IndexGenerator(_hash);

                var dataSections = ExtractDataSections(dataDivision);
                var statementsForWorkingStorageVariables = GenerateDisplayStatements(dataSections.WorkingStorageSection, _workingStorageSectionSelection, indexGenerator);
                var statementsForLocalStorageVariables = GenerateDisplayStatements(dataSections.LocalStorageSection, _localStorageSectionSelection, indexGenerator);
                var statementsForLinkageVariables = GenerateDisplayStatements(dataSections.LinkageSection, _linkageSectionSelection, indexGenerator);

                var cobolStringBuilder = new CobolStringBuilder(true);
                indexGenerator.WriteCobolCode(cobolStringBuilder);
                string cobolStringForIndices = cobolStringBuilder.ToString();

                cobolStringBuilder.Clear();
                statementsForWorkingStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLocalStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLinkageVariables.WriteCobolCode(cobolStringBuilder);
                string cobolStringForStatements = cobolStringBuilder.ToString();

                // TODO Convert generated indices to a TextEdit
                // TODO Convert generated statements to a TextEdit
            }

            // TODO Set TextEdits
            return ("Debug instructions successfully generated.", null);
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
    }
}
