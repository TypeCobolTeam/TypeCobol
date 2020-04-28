using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Represents a Redefines Symbol, for instance in the example Below it is the variable B.
    /// ex:
    /// 05  A PICTURE X(6).
    /// 05  B REDEFINES A.
    ///     10 B-1          PICTURE X(2).
    ///     10 B-2          PICTURE 9(4).
    /// 05  C PICTURE 99V99.
    /// </summary>
    public class RedefinesSymbol : VariableSymbol
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="name">Symbol 's name</param>
        /// <param name="redefined">The redefined Symbol</param>
        public RedefinesSymbol(string name, VariableSymbol redefined) : base(name)
        {
            base.SetFlag(Flags.Redefines, true);
            Redefined = redefined;
        }

        /// <summary>
        /// The redefined symbol
        /// </summary>
        public VariableSymbol Redefined
        {
            get;
            internal set;
        }

        /// <summary>
        /// Get the Top REDEFINES in case of REDEFINES OF REDEFINES suite.
        /// </summary>
        public VariableSymbol TopRedefined => Redefined != null && Redefined.HasFlag(Flags.Redefines) ? ((RedefinesSymbol) Redefined).TopRedefined : Redefined;

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitRedefinesSymbol(this, arg);
        }
    }
}
