using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;
using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A variable declared has having a type that comes from a TypeDef.
    /// Such variable is expanded to have the expanded type from a TypeDef
    /// </summary>
    public class VariableTypeSymbol : VariableSymbol
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="tdSym">The associated TypeDef symbol</param>
        public VariableTypeSymbol(string name, TypedefSymbol tdSym) : base(name)
        {
            System.Diagnostics.Debug.Assert(tdSym != null);
            SetFlag(Flags.HasATypedefType, true);
            Typedef = tdSym;
        }

        /// <summary>
        /// Constructor with an unresolved Type's path
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="path">The unresolved type's path</param>
        public VariableTypeSymbol(string name, string[] paths) : base(name)
        {
            System.Diagnostics.Debug.Assert(paths != null);
            SetFlag(Flags.HasATypedefType, true);
            Typedef = null;
            TypePaths = paths;
        }

        /// <summary>
        /// If the undrlying type is not resolved then this the path of the type to resolved.
        /// </summary>
        internal string[] TypePaths
        {
            get;
            set;
        }

        /// <summary>
        /// The Type of a variable whose type comes from a TYPEDEF can be set later when
        /// The TYPEDEF symbol is resolved. 
        /// </summary>
        public override Types.Type Type
        {
            get
            {
                if (Typedef == null)
                {
                    //Try to complete the type.
                    if (!TypeCompleter())
                        return base.Type;
                }
                if (base.Type == null && Typedef?.Type != null)
                {
                    base.Type = Typedef.Type;
                }
                return base.Type;
            }
            set => base.Type = value;
        }

        /// <summary>
        /// The Typedef symbol
        /// </summary>
        public TypedefSymbol Typedef
        {
            get;
            internal set;
        }

        protected internal override bool TypeCompleter(RootSymbolTable root = null)
        {
            if (Typedef != null)
            {//Variable Type Symbol is already completed
                return true;
            }
            if (TypePaths == null || TypePaths.Length == 0)
            {
                return false;
            }
            ProgramSymbol program = (ProgramSymbol)NearestKind(Symbol.Kinds.Program, Symbol.Kinds.Function);
            ProgramSymbol topProgram = (ProgramSymbol)TopParent(Symbol.Kinds.Program);
            if (!topProgram.HasFlag(Flags.ProgramCompleted))
                return false;//This program is not completed yet, that is to say it is in construction.
            root  = root ?? (RootSymbolTable)TopParent(Symbol.Kinds.Root);
            System.Diagnostics.Debug.Assert(root != null);
            if (root == null)
                return false;
            Domain<TypedefSymbol>.Entry entry = program.ResolveType(root, TypePaths);
            if (entry == null || entry.Count != 1)
            {//Unknown type or ambiguous types
                return false;
            }
            //--------------------------------------------------------------------------------------------
            //We don't check type accessibility here. I think that the semantic analyzer should do that.
            //This can be achieved by the following call:
            //program.IsTypeAccessible(entry.Symbol);
            //--------------------------------------------------------------------------------------------
            Type currentType = base.Type;//The type before completion can be an ArrayType or a PointerType
            Typedef = entry.Symbol;
            TypePaths = null;//GC : :-)
            if (currentType != null)
            {
                switch (currentType.Tag)
                {
                    case Types.Type.Tags.Array:
                        {
                            ArrayType at = (ArrayType)currentType;
                            at.ElementType = Typedef?.Type;
                        }
                        break;
                    case Types.Type.Tags.Pointer:
                        {
                            PointerType at = (PointerType)currentType;
                            at.ElementType = Typedef?.Type;
                        }
                        break;
                }
            }
            return true;
        }


        //public override string TypedName => Typedef != null && Typedef.Type != null && !Typedef.Type.HasFlag(Flags.BuiltinType) ? (Name + Typedef.Name) : Name;
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
