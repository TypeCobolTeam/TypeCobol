using System.IO;
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

        /// <summary>
        /// When a RedefinesSymbol is normalized it  Redefined Symbol must change.
        /// </summary>
        /// <param name="domain"></param>
        internal override void NormalizeExpandedSymbol(Domain<VariableSymbol> domain)
        {
            base.NormalizeExpandedSymbol(domain);
            Container<VariableSymbol>.Entry entry = domain.Lookup(Redefined.Name);
            System.Diagnostics.Debug.Assert(entry != null);
            System.Diagnostics.Debug.Assert(entry.Count == 1);
            Redefined = entry.Symbol;            
        }

        /// <summary>
        /// Dump this symbol in the given TextWriter instance
        /// </summary>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">Indentation level</param>
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(this.Level.ToString("00"));
            tw.Write(' ');
            tw.Write(Name);
            tw.Write(" REDEFINES ");
            tw.Write(this.Redefined.Name);
            tw.Write(" ");
            if (Type != null)
                this.Type.Dump(tw, 0);
            else
                tw.Write("???");
            DumpSymbolFlags(this.Flag, tw);
            tw.Write('.');
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitRedefinesSymbol(this, arg); }
    }
}
