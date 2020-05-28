using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Abstract class for a combination of a Symbol and Type Visitor.
    /// </summary>
    /// <typeparam name="TR"></typeparam>
    /// <typeparam name="TP"></typeparam>
    public abstract class AbstractSymbolAndTypeVisitor<TR, TP> : Symbol.IVisitor<TR, TP>, Types.Type.IVisitor<TR, TP>
    {
        public TR Visit(Symbol s, TP arg) { return s.Accept(this, arg); }
        public virtual TR VisitFunctionSymbol(FunctionSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitIndexSymbol(IndexSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitNamespaceSymbol(NamespaceSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitParagraphSymbol(ParagraphSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitProgramSymbol(ProgramSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitRedefinesSymbol(RedefinesSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitRenamesSymbol(RenamesSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitSectionSymbol(SectionSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitTypedefSymbol(TypedefSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitVariableSymbol(VariableSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public virtual TR VisitVariableTypeSymbol(VariableTypeSymbol s, TP arg) { return VisitSymbol(s, arg); }
        public abstract TR VisitSymbol(Symbol s, TP arg);

        public TR Visit(Types.Type t, TP s) { return t.Accept(this, s); }
        public virtual TR VisitArrayType(ArrayType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitFunctionType(FunctionType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitPictureType(PictureType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitPointerType(PointerType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitProgramType(ProgramType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitGroupType(GroupType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitRenamesType(RenamesType t, TP s) { return VisitType(t, s); }
        public virtual TR VisitTypedefType(TypedefType t, TP s) { return VisitType(t, s); }
        public abstract TR VisitType(Types.Type t, TP s);
    }
}
