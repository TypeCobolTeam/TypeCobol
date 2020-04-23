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
            tw.Write(' ');
            bool bHasDot = false;
            if (Type != null)
            {
                if (Type.Tag == Type.Tags.Typedef || Type.TypeComponent?.Tag == Type.Tags.Typedef)
                {
                    tw.Write("TYPE ");
                    this.Type.Dump(tw, 0);
                }
                else
                {
                    if (Type.TypeComponent?.Tag == Type.Tags.Group)
                    {
                        tw.WriteLine(".");
                        this.Type.Dump(tw, indentLevel + 1);
                        bHasDot = true;
                    }
                    else
                    {
                        this.Type.Dump(tw, 0);
                    }
                }
            }
            else 
                tw.Write("???");
            DumpSymbolFlags(this.Flag, tw);
            if (!bHasDot)
                tw.WriteLine('.');
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitTypedVariableSymbol(this, arg);
        }
    }
}
