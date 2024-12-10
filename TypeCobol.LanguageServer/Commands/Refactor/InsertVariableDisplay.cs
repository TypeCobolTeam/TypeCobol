namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class InsertVariableDisplay : AbstractSingleDocumentRefactoring<InsertVariableDisplayRefactoringProcessor>
    {
        public static InsertVariableDisplay Create(TypeCobolServer typeCobolServer) => new(typeCobolServer);

        public InsertVariableDisplay(TypeCobolServer typeCobolServer)
            : base(typeCobolServer)
        {

        }
    }
}
