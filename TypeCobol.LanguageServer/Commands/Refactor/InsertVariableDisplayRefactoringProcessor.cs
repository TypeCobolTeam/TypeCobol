using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class InsertVariableDisplayRefactoringProcessor : IRefactoringProcessor
    {
        private string _hash;
        private Position _insertAt;
        private bool _insertBeforeStatement;
        private Selection _workingStorageSectionSelection;
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
            _linkageSectionSelection = GetSelection(3);

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
            // TODO GetProgram from insert position (it may not be MainProgram if we have stacked programs)
            var program = compilationUnit.ProgramClassDocumentSnapshot.Root.MainProgram;
            var dataDivision = program.Children.OfType<DataDivision>().SingleOrDefault();
            if (dataDivision != null)
            {
                var indexGenerator = new IndexGenerator(_hash);

                var workingStorageSection = dataDivision.Children.OfType<WorkingStorageSection>().SingleOrDefault();
                GeneratedRoot statementsForWorkingStorageVariables = GenerateDisplayStatements(workingStorageSection, _workingStorageSectionSelection, indexGenerator);
                var linkageSection = dataDivision.Children.OfType<LinkageSection>().SingleOrDefault();
                GeneratedRoot statementsForLinkageVariables = GenerateDisplayStatements(linkageSection, _linkageSectionSelection, indexGenerator);

                var cobolStringBuilder = new CobolStringBuilder(true);
                indexGenerator.WriteCobolCode(cobolStringBuilder);
                string cobolStringForIndices = cobolStringBuilder.ToString();

                cobolStringBuilder.Clear();
                statementsForWorkingStorageVariables.WriteCobolCode(cobolStringBuilder);
                statementsForLinkageVariables.WriteCobolCode(cobolStringBuilder);
                string cobolStringForStatements = cobolStringBuilder.ToString();

                // TODO Convert generated indices to a TextEdit
                // TODO Convert generated statements to a TextEdit
            }

            // TODO Set TextEdits
            return ("Debug instructions successfully generated.", null);
        }

        private GeneratedRoot GenerateDisplayStatements(DataSection dataSection, Selection rootSelection, IndexGenerator indexGenerator)
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
