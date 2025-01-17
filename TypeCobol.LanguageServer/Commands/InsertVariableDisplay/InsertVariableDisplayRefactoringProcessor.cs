using TypeCobol.Compiler;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    public class InsertVariableDisplayRefactoringProcessor : AbstractRefactoringProcessor
    {
        public override TextDocumentIdentifier PrepareRefactoring(object[] arguments)
        {
            throw new NotImplementedException();
        }

        public override void CheckTarget(CompilationUnit compilationUnit)
        {
            throw new NotImplementedException();
        }

        public override (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit)
        {
            throw new NotImplementedException();
        }
    }
}
