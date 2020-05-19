using System.IO;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

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
        public ArrayType(Symbol owner)
            : base(Tags.Array)
        {
            Indexes = new Domain<IndexSymbol>(owner);
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

        /// <summary>
        /// All indexes of this array.
        /// </summary>
        public Domain<IndexSymbol> Indexes { get; }

        public override Type TypeComponent => ElementType;

        public override bool MayExpand => ElementType != null && ElementType.MayExpand;

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"MinOccur: {MinOccur}");
            output.Write(indent);
            output.WriteLine($"MaxOccur: {(MaxOccur.HasValue ? MaxOccur.Value.ToString() : "Unbounded")}");

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

            if (Indexes.Count > 0)
            {
                output.Write(indent);
                output.WriteLine("Indexes:");
                var level = indentLevel + 1;
                foreach (var index in Indexes)
                {
                    index.Dump(output, level);
                }
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitArrayType(this, arg);
        }
    }
}
