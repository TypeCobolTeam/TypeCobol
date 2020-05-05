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

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            if (ElementType != null)
            {
                string indent = new string(' ', 2 * indentLevel);
                output.Write(indent);
                output.WriteLine("ElementType:");
                ElementType.Dump(output, indentLevel + 1);
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitPointerType(this, arg);
        }
    }
}
