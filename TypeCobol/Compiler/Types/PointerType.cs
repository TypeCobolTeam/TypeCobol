using System.IO;

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

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitPointerType(this, arg);
        }
    }
}
