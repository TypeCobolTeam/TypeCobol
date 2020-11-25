namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// The Symbol of a Function declaration
    /// </summary>
    public class FunctionSymbol : ScopeSymbol
    {
        /// <summary>
        /// Name constructor
        /// </summary>
        /// <param name="name">Function's name</param>
        /// <remarks>Do not forget to set FunctionType after calling this.</remarks>
        public FunctionSymbol(string name)
            : base(name, Kinds.Function)
        {

        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitFunctionSymbol(this, arg);
        }
    }
}
