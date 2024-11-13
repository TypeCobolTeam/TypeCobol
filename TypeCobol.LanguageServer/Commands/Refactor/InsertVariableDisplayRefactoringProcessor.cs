using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class InsertVariableDisplayRefactoringProcessor : IRefactoringProcessor
    {
        private string _hash;
        private Position _insertAt;
        private Selection _workingStorageSectionSelection;
        private Selection _linkageSectionSelection;

        public TextDocumentIdentifier PrepareRefactoring(object[] arguments)
        {
            // TODO Extract TextDocumentIdentifier and insert position

            string allArgs = arguments.Select(argument => argument.ToString()).Aggregate(string.Empty, string.Concat);
            _hash = Tools.Hash.CreateCOBOLNameHash(allArgs + DateTime.Now);

            // TODO Build Selection objects from arguments

            return new TextDocumentIdentifier("example/uri");
        }

        public void CheckTarget(CompilationUnit compilationUnit)
        {
            // TODO Define requirements on CompilationUnit to perform refactoring
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

            // TODO content of the label ?
            return (null, null);
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
