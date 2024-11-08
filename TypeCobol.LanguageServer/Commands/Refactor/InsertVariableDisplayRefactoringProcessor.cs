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
            // TODO Extract TextDocumentIdentifier and insert position and build Selection objects from arguments
            // TODO Create hash from all args + current date ?
            return new TextDocumentIdentifier("example/uri");
        }

        public void CheckTarget(CompilationUnit compilationUnit)
        {
            // TODO
        }

        public (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit)
        {
            // TODO GetProgram from insert position (it may not be MainProgram if we have stacked programs)
            var program = compilationUnit.ProgramClassDocumentSnapshot.Root.MainProgram;
            var dataDivision = program.Children.OfType<DataDivision>().SingleOrDefault();
            if (dataDivision != null)
            {
                var workingStorageSection = dataDivision.Children.OfType<WorkingStorageSection>().SingleOrDefault();
                GeneratedRoot statementsForWorkingStorageVariables = GenerateDisplayStatements(workingStorageSection, _workingStorageSectionSelection);
                var linkageSection = dataDivision.Children.OfType<LinkageSection>().SingleOrDefault();
                GeneratedRoot statementsForLinkageVariables = GenerateDisplayStatements(linkageSection, _linkageSectionSelection);

                // TODO Gather generated indices
                
                var cobolStringWriter = new CobolStringWriter();
                statementsForWorkingStorageVariables.Write(cobolStringWriter);
                statementsForLinkageVariables.Write(cobolStringWriter);
                string cobolString = cobolStringWriter.ToString();

                // TODO Convert to TextEdits, one for indices, one for generated statements
            }

            return (null, null);
        }

        private GeneratedRoot GenerateDisplayStatements(DataSection dataSection, Selection rootSelection)
        {
            if (dataSection == null || rootSelection == null)
            {
                return new GeneratedRoot();
            }

            var visitor = new DataDefinitionToDisplayVisitor(_hash, rootSelection);
            visitor.Visit(dataSection);
            return visitor.GeneratedStatements;
        }
    }
}
