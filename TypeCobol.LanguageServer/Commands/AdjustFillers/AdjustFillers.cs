﻿namespace TypeCobol.LanguageServer.Commands.AdjustFillers
{
    internal class AdjustFillers : AbstractSingleDocumentRefactoring<AdjustFillersRefactoringProcessor>
    {
        public static AdjustFillers Create(TypeCobolServer typeCobolServer) => new(typeCobolServer);

        public AdjustFillers(TypeCobolServer typeCobolServer)
            : base(typeCobolServer)
        {

        }
    }
}
