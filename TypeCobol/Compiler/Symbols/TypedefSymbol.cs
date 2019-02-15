using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
        public TypedefSymbol(String name)
            : base(name)
        {
            base.Kind = Kinds.Typedef;
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
            tw.Write(" TYPEDEF ");
            if (Type != null)
                this.Type.Dump(tw, 0);
            else
                tw.Write("???");
            DumpSymbolFlags(this.Flag, tw);
            tw.Write('.');
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitTypedefSymbol(this, arg); }
    }
}
