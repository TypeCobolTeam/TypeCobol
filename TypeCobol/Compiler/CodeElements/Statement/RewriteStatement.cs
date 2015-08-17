using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{

    public class RewriteStatement : CodeElement
    {
        QualifiedDataName RecordName;
        Identifier Identifier;

        public RewriteStatement(QualifiedDataName recordname, Identifier identifier)
            : base(CodeElementType.RewriteStatement)
        {
            this.RecordName = recordname;
            this.Identifier = identifier;
        }
    }
}
