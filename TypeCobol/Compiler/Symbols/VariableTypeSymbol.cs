using System.IO;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A variable declared has having a type that comes from a TypeDef.
    /// Such variable is expanded to have the expanded type from a TypeDef
    /// </summary>
    public class VariableTypeSymbol : VariableSymbol
    {
        private TypedefSymbol _typedef;

        /// <summary>
        /// Constructor with an unresolved Type's path
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="paths">The unresolved type's path</param>
        public VariableTypeSymbol(string name, string[] paths)
            : base(name)
        {
            System.Diagnostics.Debug.Assert(paths != null);
            System.Diagnostics.Debug.Assert(paths.Length != 0);
            SetFlag(Flags.HasATypedefType, true);
            TypePaths = paths;
            _typedef = null;
        }

        /// <summary>
        /// If the underlying type is not resolved then this the path of the type to resolved.
        /// </summary>
        public string[] TypePaths { get; }

        /// <summary>
        /// Tries to resolve TypedefSymbol of this variable according to its TypePaths.
        /// </summary>
        private void LinkTypedef()
        {
            var declaringProgram = (ProgramSymbol) NearestKind(Kinds.Program, Kinds.Function);
            if (declaringProgram == null)
            {
                //variable has no owner yet
                return;
            }

            var root = (RootSymbolTable) TopParent(Kinds.Root);
            System.Diagnostics.Debug.Assert(root != null);

            var entry = declaringProgram.ResolveType(root, TypePaths);
            if (entry != null && entry.Count == 1)
            {
                //Successfully resolved Typedef symbol
                _typedef = entry.Symbol;

                //Update ElementType in case of Pointer or Array
                var currentType = base.Type;
                if (currentType != null)
                {
                    switch (currentType.Tag)
                    {
                        case Type.Tags.Array:
                            ArrayType arrayType = (ArrayType)currentType;
                            arrayType.ElementType = _typedef.Type;
                            break;
                        case Type.Tags.Pointer:
                            PointerType pointerType = (PointerType)currentType;
                            pointerType.ElementType = _typedef.Type;
                            break;
                    }
                }

                //--------------------------------------------------------------------------------------------
                //We don't check type accessibility here. I think that the semantic analyzer should do that.
                //This can be achieved by the following call:
                //program.IsTypeAccessible(entry.Symbol);
                //--------------------------------------------------------------------------------------------
            }
        }

        /// <summary>
        /// The Typedef symbol
        /// </summary>
        public TypedefSymbol Typedef
        {
            get
            {
                //Perform lazy typedef resolution
                if (_typedef == null) LinkTypedef();

                return _typedef;
            }
        }

        /// <summary>
        /// The Type of a variable whose type comes from a typedef is only accessible after the typedef has been resolved.
        /// It is then replaced by a new Type instance during program expansion.
        /// </summary>
        public override Types.Type Type
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

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitVariableTypeSymbol(this, arg); }
    }
}
