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
        /// Path to the VariableSymbol of the DEPENDING ON clause if any.
        /// </summary>
        public string[] DependingOnPath
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

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"MinOccur: {MinOccur}");
            output.Write(indent);
            output.WriteLine($"MaxOccur: {MaxOccur}");

            if (DependingOnPath != null && DependingOnPath.Length > 0)
            {
                output.Write(indent);
                output.WriteLine($"DependingOnPath: {string.Join(", ", DependingOnPath)}");
            }

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
