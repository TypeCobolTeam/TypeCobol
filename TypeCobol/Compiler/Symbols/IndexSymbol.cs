namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents an Index.
    /// The type of an index is : S9(8) comp
    /// </summary>
    public class IndexSymbol : VariableSymbol
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        /// <param name="name">The Index name</param>
        public IndexSymbol(string name)
            : base(name, Kinds.Index)
        {
            base.Type = Builtins.IndexType;
        }

        /// <summary>
        /// The Indexed Variable, this is the Owner but viewed as a VariableSymbol.
        /// </summary>
        public VariableSymbol Indexed
        {
            get
            {
                System.Diagnostics.Debug.Assert(Owner is VariableSymbol);
                return (VariableSymbol) Owner;
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitIndexSymbol(this, arg);
        }
    }
}
