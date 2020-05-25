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
        /// The Maximal Occurence number in the array, null if the array
        /// has an unbounded number of occurrences.
        /// </summary>
        public long? MaxOccur
        {
            get;
            set;
        }

        /// <summary>
        /// Shortcut to test if the array has no max occur.
        /// </summary>
        public bool IsUnbounded => !MaxOccur.HasValue;

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

        public override bool IsEquivalentTo(Type otherType)
        {
            if(otherType==null)
            {
                return false;
            }
            if(otherType.Tag == Tags.Array)
            {
                var arrayType = (ArrayType) otherType;
                return this.MinOccur == arrayType.MinOccur && this.MaxOccur == arrayType.MaxOccur 
                    && (this.ElementType == arrayType.ElementType || (this.ElementType?.IsEquivalentTo(arrayType.ElementType) ?? false) );
            }
            return false;
        }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"MinOccur: {MinOccur}");
            output.Write(indent);
            output.WriteLine($"MaxOccur: {(MaxOccur.HasValue ? MaxOccur.Value.ToString() : "Unbounded")}");

            if (ElementType != null)
            {
                output.Write(indent);
                output.WriteLine("ElementType:");
                ElementType.Dump(output, indentLevel + 1);
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitArrayType(this, arg);
        }
    }
}
