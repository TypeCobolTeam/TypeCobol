using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class ReadStatement : CodeElement
    {
        SymbolReference<FileName> FileName;
        SymbolReference<DataName> Key;
        Identifier Into;
        bool IsNext;
        bool IsRecord;

        public ReadStatement(SymbolReference<FileName> filename, Identifier into, SymbolReference<DataName> key, bool next, bool record)
            : base(CodeElementType.ReadStatement)
        {
            this.FileName = filename;
            this.Into = into;
            this.Key = key;
            this.IsNext = next;
            this.IsRecord = record;
        }
    }
}
