using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class WriteStatement : CodeElement
    {
        QualifiedDataName RecordName;
        Identifier Identifier;
        bool IsBeforeAdvancing;
        bool IsAfterAdvancing;
        Expression Lines;
        MnemonicForEnvironmentName Mnemonic;
        bool Page;

        public WriteStatement(QualifiedDataName recordname, Identifier identifier, bool before, bool after, Expression lines, MnemonicForEnvironmentName mnemonic, bool page)
            : base(CodeElementType.WriteStatement)
        {
            this.RecordName = recordname;
            this.Identifier = identifier;
            this.IsBeforeAdvancing = before;
            this.IsAfterAdvancing = after;
            this.Mnemonic = mnemonic;
            this.Lines = lines;
            this.Page = page;
        }
    }
}
