using System.IO;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Class that represents an Array Type
    /// </summary>
    public class ArrayType : Type
    {
        /// <summary>
        /// Empty Constructor
        /// </summary>
        public ArrayType()
            : base(Tags.Array)
        {
        }

        /// <summary>
        /// The Minimal Occurence number in the array.
        /// </summary>
        public long MinOccur
        {
            get;
            set;
        }

        /// <summary>
        /// The Maximal Occurence number in the array.
        /// </summary>
        public long MaxOccur
        {
            get;
            set;
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
            tw.Write(" OCCURS ");
            tw.Write(MinOccur);
            tw.Write(" TO ");
            tw.Write(MaxOccur);
            tw.Write(" TIMES ");
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitArrayType(this, arg);
        }
    }
}
