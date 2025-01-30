namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
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
