using TypeCobol.Compiler.Types;

using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Abstract class for a combination of a Symbol and Type Visitor.
    /// </summary>
    /// <typeparam name="TResult">Result type of the visitor.</typeparam>
    /// <typeparam name="TParameter">Argument type of the visitor.</typeparam>
    public abstract class AbstractSymbolAndTypeVisitor<TResult, TParameter> : Symbol.IVisitor<TResult, TParameter>, Type.IVisitor<TResult, TParameter>
    {
        public TResult Visit(Symbol s, TParameter arg) { return s.Accept(this, arg); }
        public virtual TResult VisitFunctionSymbol(FunctionSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitIndexSymbol(IndexSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitParagraphSymbol(ParagraphSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitProgramSymbol(ProgramSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitRedefinesSymbol(RedefinesSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitSectionSymbol(SectionSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitTypedefSymbol(TypedefSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitVariableSymbol(VariableSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public virtual TResult VisitTypedVariableSymbol(TypedVariableSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
        public abstract TResult VisitSymbol(Symbol s, TParameter arg);

        public TResult Visit(Type t, TParameter s) { return t.Accept(this, s); }
        public virtual TResult VisitArrayType(ArrayType t, TParameter s) { return VisitType(t, s); }
        public virtual TResult VisitGroupType(GroupType t, TParameter s) { return VisitType(t, s); }
        public virtual TResult VisitPictureType(PictureType t, TParameter s) { return VisitType(t, s); }
        public virtual TResult VisitScopeType(ScopeType t, TParameter s) { return VisitType(t, s); }
        public virtual TResult VisitTypedefType(TypedefType t, TParameter s) { return VisitType(t, s); }
        public abstract TResult VisitType(Type t, TParameter s);
    }
}
