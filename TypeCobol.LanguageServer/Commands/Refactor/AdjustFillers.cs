namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class AdjustFillers : AbstractSingleDocumentRefactoring<AdjustFillerRefactoringProcessor>
    {
        public static AdjustFillers Create(TypeCobolServer typeCobolServer) => new(typeCobolServer);

        public AdjustFillers(TypeCobolServer typeCobolServer)
            : base(typeCobolServer)
        {

        }
    }
}
