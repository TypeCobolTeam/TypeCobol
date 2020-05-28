using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;
using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The Pointer Type.
    /// </summary>
    public class PointerType : Type
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public PointerType()
            : base(Tags.Pointer)
        {
        }

        /// <summary>
        /// The Type of an Element
        /// </summary>
        public Type ElementType
        {
            get;
            set;
        }

        public override Type TypeComponent => ElementType;
        public override bool MayExpand => ElementType != null && ElementType.MayExpand;
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(s);
            if (ElementType != null)
            {
                ElementType.Dump(tw, indentLevel);
            }
            else
            {
                tw.Write("???");
            }
            tw.Write(" POINTER");
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitPointerType(this, s); }
    }
}
