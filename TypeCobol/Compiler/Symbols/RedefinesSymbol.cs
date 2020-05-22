using System.IO;

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
        public RedefinesSymbol(string name)
            : base(name)
        {
            base.SetFlag(Flags.Redefines, true);
            _redefined = null;
        }

        private VariableSymbol _redefined;
        /// <summary>
        /// Redefined symbol.
        /// </summary>
        /// <remarks>Should be set only by <see cref="Diagnostics.RedefinesChecker" /></remarks>
        public VariableSymbol Redefined
        {
            get => _redefined;
            set
            {
                if (value != null)
                {
                    _redefined = value;
                    _redefined.AddRedefines(this);
                }
            }
        }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);

            if (_redefined != null)
            {
                output.Write(indent);
                output.WriteLine($"Redefined: {Redefined.FullName}");//Write reference
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitRedefinesSymbol(this, arg);
        }
    }
}
