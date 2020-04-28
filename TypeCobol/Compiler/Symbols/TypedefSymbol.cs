namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Typedef Symbol
    /// </summary>
    public class TypedefSymbol : VariableSymbol
    {
        /// <summary>
        /// Named constructor.
        /// </summary>
        /// <param name="name"></param>
        public TypedefSymbol(string name)
            : base(name)
        {
            base.Kind = Kinds.Typedef;
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitTypedefSymbol(this, arg);
        }
    }
}
