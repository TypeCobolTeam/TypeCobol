using System.IO;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A variable declared with a type that comes from a TypeDef.
    /// Such variable is expanded to have the expanded type from its TypeDef.
    /// </summary>
    public class TypedVariableSymbol : VariableSymbol
    {
        /// <summary>
        /// Constructor with an unresolved Type's path
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="paths">The unresolved type's path</param>
        public TypedVariableSymbol(string name, string[] paths)
            : base(name)
        {
            System.Diagnostics.Debug.Assert(paths != null);
            System.Diagnostics.Debug.Assert(paths.Length != 0);
            SetFlag(Flags.HasATypedefType, true);
            TypePaths = paths;
            Typedef = null;
        }

        /// <summary>
        /// If the underlying type is not resolved then this is the path of the type to resolve.
        /// </summary>
        public string[] TypePaths { get; }

        /// <summary>
        /// The Typedef symbol
        /// </summary>
        /// <remarks>Setter should only be called by TypeCobolLinker.</remarks>
        public TypedefSymbol Typedef { get; set; }

        /// <summary>
        /// The Type of a variable whose type comes from a typedef is only accessible after the typedef has been resolved.
        /// It is then replaced by a new Type instance during program expansion.
        /// </summary>
        public override Type Type
        {
            get
            {
                if (Typedef == null)
                {
                    //Type linking not done yet
                    return null;
                }

                if (base.Type == null)
                {
                    //Type expansion not done yet 
                    return Typedef.Type;
                }

                return base.Type;
            }
        }

        public override string TypedName => Typedef != null ? (Name + '.' + Typedef.Name) : Name;

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);

            output.Write(indent);
            output.WriteLine($"TypePaths: [{string.Join(", ", TypePaths)}]");

            if (Typedef != null)
            {
                output.Write(indent);
                output.WriteLine($"Typedef: {Typedef.FullName}");//Write reference
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitTypedVariableSymbol(this, arg);
        }
    }
}
