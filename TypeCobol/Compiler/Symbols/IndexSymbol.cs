using System.IO;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents an Index.
    /// The type of an index is : S9(8) comp
    /// </summary>
    public class IndexSymbol : VariableSymbol
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        /// <param name="name">The Index name</param>
        public IndexSymbol(string name)
            : base(name, Kinds.Index)
        {
            base.Type = Builtins.IndexType;
        }

        /// <summary>
        /// The Indexed Variable.
        /// </summary>
        public VariableSymbol Indexed { get; set; }        

        public override string IndexedName => Indexed != null && Indexed.Name .Length != 0 ? Indexed.Name + "::" + Name : Name;
        public override string IndexedOfName => Indexed != null && Indexed.Name.Length != 0 ? Name +  " OF " + Indexed.Name  : Name;
        public override string IndexedDotName => Indexed != null && Indexed.Name.Length != 0 ? Indexed.Name + '.' + Name : Name;

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            if (Indexed != null)
            {
                string indent = new string(' ', 2 * indentLevel);
                output.Write(indent);
                output.WriteLine($"Indexed: {Indexed.FullName}");//Write reference
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitIndexSymbol(this, arg);
        }
    }
}
