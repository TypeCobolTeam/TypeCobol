using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Schema;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;
using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Base classe of a Cobol Symbol
    /// </summary>
    public abstract class Symbol : ISemanticData, ICloneable
    {
        /// <summary>
        /// Enumeration on the kind of symbols
        /// </summary>
        public enum Kinds
        {
            Root,//The Root Symbol table
            Namespace,
            Program,
            Function,
            Typedef,
            Variable,
            Index,
            Section,
            Paragraph,
            Sentence,
        }

        [Flags]
        public enum Flags : ulong
        {
            Public = 0x01 << 0,
            Private = 0x01 << 1,
            External = 0x01 << 2,
            Global = 0x01 << 3, //Symbol explicitly marked as global
            FILE_SECTION = 0x01 << 4,
            GLOBAL_STORAGE = 0x01 << 5,
            WORKING_STORAGE = 0x01 << 6,
            LOCAL_STORAGE = 0x01 << 7,
            LINKAGE = 0x01 << 8,
            Input = 0x01 << 9,
            Output = 0x01 << 10,
            Inout = 0x01 << 11,
            ByReference = 0x01 << 12,
            ByContent = 0x01 << 13,
            ByValue = 0x01 << 14,
            Strong = 0x01 << 15,
            Strict = 0x01 << 16,
            Weak = 0x01 << 17,
            Conditions = 0x01 << 18,
            Renames = 0x01 << 19,
            Redefines = 0x01 << 20,
            HasATypedefType = 0x01 << 21,//The symbol has a type that comes from a TypeDef.
            Parameter = 0x01 << 22,//This a parameter variable.
            Return = 0x01 << 23,//A Return variable.
            BuiltinType = 0x01 << 24,//This is a Builtin Type.
            InsideTypdef = 0x01 << 25,//Flag of any symbol inside a Typedef definition
            Declaratives = 0x01 << 26,//Flag to indicate a symbol inside Declaractives context
            ProgramExpanded = 0x01 << 27,//Flag for a program that have been already expanded.
            NeedTypeCompletion = 0x01 << 28,//For a program that need type Completion, a pure COBOL Program does not need type completion (No TYPEDEF).
            BuiltinSymbol = 0x01 << 29, //This is a builting symbol.
            ProgramCompleted = 0x01 << 30, //This Top Program has been completed

            //Etc...
        }

        /// <summary>
        /// Variable section mask.
        /// </summary>
        internal const Flags SectionMask = Flags.GLOBAL_STORAGE | Flags.WORKING_STORAGE | Flags.LINKAGE | Flags.FILE_SECTION | Flags.LOCAL_STORAGE;

        /// <summary>
        /// The Visibility mask that a symbol can take.
        /// </summary>
        public const Flags SymbolVisibilityMask = Flags.Public | Flags.Private | Flags.External | Flags.Global;

        /// <summary>
        /// Empty constructor
        /// </summary>
        protected Symbol()
        {
        }

        /// <summary>
        /// Named constructor
        /// </summary>
        protected Symbol(string name, Kinds kind)
        {
            this.Name = name??"";//It happends that a symbol can have no name
            Kind = kind;
        }

        /// <summary>
        /// Symbol Kind
        /// </summary>
        public Kinds Kind
        {
            get;
            protected set;
        }

        /// <summary>
        /// Symbol Flags.
        /// </summary>
        public Flags Flag
        {
            get;
            internal set;
        }

        /// <summary>
        /// A integer number which can be associated to this Symbol.
        /// </summary>
        public int Number
        {
            set;
            get;
        }

        /// <summary>
        /// Symbol's name
        /// </summary>
        public string Name
        {
            get;
            protected set;
        }

        private System.WeakReference _myTargetNode = null;
        /// <summary>
        /// The target AST node if any
        /// </summary>
        public Node TargetNode
        {
            get
            {
                lock (this)
                {
                    return (Node)_myTargetNode?.Target;
                }
            }
            internal set
            {
                lock (this)
                {
                    if (_myTargetNode == null)
                        _myTargetNode = new System.WeakReference(value);
                    else
                        _myTargetNode.Target = value;
                }
            }
        }

        /// <summary>
        /// A Typed name is the name followed by a type, by default is the name..
        /// </summary>
        public virtual string TypedName => Name;

        /// <summary>
        /// Name used for an Indexed Name
        /// </summary>
        public virtual string IndexedName => Name;
        public virtual string IndexedOFName => Name;
        public virtual string IndexedDotName => Name;

        /// <summary>
        /// Full qualified name of this Symbol à la TypeCobol using "::"
        /// </summary>
        public virtual string FullName
        {
            get
            {
                string root = Owner?.FullName ?? "";
                string name = IndexedName;
                return root.Length > 0 ? root + (name.Length > 0 ? ("::" + name) : name) : name;
            }
        }

        /// <summary>
        /// Full qualified name of this Symbol à la COBOL85 using OF
        /// </summary>
        public virtual string FullOfName
        {
            get
            {
                string root = Owner?.FullOfName ?? "";
                string name = IndexedOFName;
                return root.Length > 0 ? (name.Length > 0 ? (name + " OF ") : name) + root : name;
            }
        }

        /// <summary>
        /// Full dotted qualified name
        /// </summary>
        public virtual string FullDotName
        {
            get
            {
                string root = Owner?.FullDotName ?? "";
                string name = IndexedDotName;
                return root.Length > 0 ? root + (name.Length > 0 ? ('.' + name) : name) : name;
            }
        }

        /// <summary>
        /// Full typed dotted qualified name
        /// </summary>
        public virtual string FullTypedDotName
        {
            get
            {
                Stack<string> paths = new Stack<string>();
                paths.Push(IndexedDotName);
                Symbol owner = Owner;
                while (owner != null)
                {
                    string name = owner.TypedName;
                    if (name?.Length != 0)
                    {
                        paths.Push(name);
                    }
                    owner = owner.Owner;
                }

                return string.Join(".", paths.ToArray());
            }
        }

        /// <summary>
        /// Type changed event.
        /// </summary>
        public event EventHandler TypeChanged;

        Type m_Type;
        /// <summary>
        /// Symbol's type
        /// </summary>
        public virtual Type Type
        {
            get => m_Type;
            set
            {
                if (m_Type != value)
                {
                    m_Type = value;
                    TypeChanged?.Invoke(this, null);
                }
            }
        }

        /// <summary>
        /// Get the the expanded type of this type symbol.
        /// The Expanded Type is the Cobol85 Type in fact.
        /// <param name="program">The program in which the expanded Type is Computed.</param>
        /// </summary>
        public virtual Type ExpandedType(ProgramSymbol program)
        {
            return this.Type;
        }

        /// <summary>
        /// Complete the Symbol's type associated to this Symbol
        /// </summary>
        /// <param name="root">The root symbol table to be used to complete the type</param>
        /// <returns>true if the type is completed, false otherwise</returns>
        protected internal virtual bool TypeCompleter(RootSymbolTable root = null)
        {
            return true;
        }

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        /// <param name="propagate">true the flags must be propagated, false otherwise</param>
        protected internal virtual void SetFlag(Flags flag, bool value, bool propagate = false)
        {
            this.Flag = value ? (Flags)((ulong)this.Flag | (ulong)flag)
                              : (Flags)((ulong)this.Flag & ~(ulong)flag);
            if (Type != null && propagate)
            {//Propagate to types.                
                if (!Type.HasFlag(flag))
                {//We test HasFlag to avoid infinite recursion with cyclic Type.
                    Type.SetFlag(flag, value);
                }
            }
        }

        /// <summary>
        /// Determines if the given flag is set.
        /// </summary>
        /// <param name="flag">The flag to be tested</param>
        /// <returns>true if yes, false otherwise.</returns>
        public bool HasFlag(Flags flag)
        {
            return ((ulong)this.Flag & (ulong)flag) != 0;
        }

        /// <summary>
        /// The Owner of this Symbol
        /// </summary>
        public Symbol Owner
        {
            get;
            protected internal set;
        }

        public SemanticKinds SemanticKind => SemanticKinds.Symbol;

        /// <summary>
        /// Determines if this symbol cans be seen as Scope.
        /// </summary>
        public bool HasScope => Kind == Kinds.Root || Kind == Kinds.Namespace || Kind == Kinds.Program ||
                                Kind == Kinds.Function;

        public virtual object Clone()
        {
            return MemberwiseClone();
        }

        /// <summary>
        /// Propagate internal symbol owner to this symbol.
        /// </summary>
        public virtual void PropagateOwner()
        {

        }

        /// <summary>
        /// Lookup the Parent of this symbol of the given name.
        /// </summary>
        /// <param name="name">Parent name looked for</param>
        /// <returns>The Parent if any, null otherwise</returns>
        public virtual Symbol LookupParentOfName(string name)
        {
            if (Owner != null && Owner.Name.Equals(name, StringComparison.OrdinalIgnoreCase))
                return Owner;
            return Owner?.LookupParentOfName(name);
        }

        /// <summary>
        /// Determine if this symbol is matching the given path (à la COBOL qualification)
        /// </summary>
        /// <param name="path">The path to match</param>
        /// <returns>true if yes, false otherwise</returns>
        public bool IsMatchingPath(string[] path)
        {
            Symbol currentSymbol = this;
            for (int i = 0; i < path.Length; i++)
            {
                switch (i)
                {
                    case 0:
                        string name = currentSymbol.Name;
                        if (!path[i].Equals(name, StringComparison.OrdinalIgnoreCase))
                            return false;
                        break;
                    default:
                    {
                        Symbol parent = currentSymbol.LookupParentOfName(path[i]);
                        if (parent == null)
                            return false;
                        currentSymbol = parent;
                    }
                        break;
                }
            }
            return true;
        }

        /// <summary>
        /// Determine if this symbol is matching the given path strictly
        /// </summary>
        /// <param name="path">The path to match</param>
        /// <returns>true if yes, false otherwise</returns>
        public bool IsStrictlyMatchingPath(string[] path)
        {
            Symbol currentSymbol = this;
            int i = 0;
            for (i = 0; i < path.Length; i++)
            {
                string name = currentSymbol.Name;
                if (!path[i].Equals(name, StringComparison.OrdinalIgnoreCase))
                    return false;
                currentSymbol = currentSymbol.Owner;
            }
            return  i == path.Length && (currentSymbol == null || currentSymbol.Kind == Kinds.Root);
        }

        /// <summary>
        /// Check if this symbol has the given symbol as parent in the parent hierarchy
        /// </summary>
        /// <param name="parent">The parent to be tested</param>
        /// <returns>true if yes, false otherwise</returns>
        public virtual bool HasParent(Symbol parent)
        {
            if (Owner == parent)
                return true;
            if (Owner == null || parent == null)
                return false;
            return Owner.HasParent(parent);
        }

        /// <summary>
        /// Get the Top Parent of this symbol having the Given Kind.
        /// This symbol is included.
        /// </summary>
        /// <param name="kind">The Kind of the Top parent</param>
        /// <returns>The Top parent if any, null otherwise</returns>
        public virtual Symbol TopParent(Kinds kind)
        {
            Symbol top = null;
            if (this.Kind == kind)
                top = this;
            Symbol ownerTop = Owner?.TopParent(kind);
            if (ownerTop != null)
                top = ownerTop;
            return top;
        }

        /// <summary>
        /// Get the nearest symbol of one of the given Kinds, including this one.
        /// </summary>
        /// <param name="kinds">The Kinds to look for</param>
        /// <returns>The Nearest Symbol</returns>
        public Symbol NearestKind(params Symbol.Kinds[] kinds)
        {
            System.Diagnostics.Debug.Assert(kinds != null);
            System.Diagnostics.Debug.Assert(kinds.Length > 0);
            return kinds.Contains(this.Kind) ? this : Owner?.NearestKind(kinds);
        }

        /// <summary>
        /// Lookup for the parent having t he given Level
        /// </summary>
        /// <param name="level"></param>
        /// <param name="inclusive">true if this symbol must be taken in account, false otherwise</param>
        /// <returns>The parent symbol of the level if one exists, null otherwise</returns>
        public virtual Symbol LookupParentLevelSymbol(int level, bool inclusive)
        {
            return null;
        }

        /// <summary>
        /// Dum Symbol tags
        /// </summary>
        /// <param name="flag">Flags to dump</param>
        /// <param name="tw">TextWriter instance</param>
        internal static void DumpSymbolFlags(Flags flag, TextWriter tw)
        {
            if (((ulong)flag & (ulong)Flags.Global) != 0)
            {
                tw.Write(" GLOBAL ");
            }
            if (((ulong)flag & (ulong)Flags.Public) != 0)
            {
                tw.Write(" PUBLIC ");
            }
            if (((ulong)flag & (ulong)Flags.Private) != 0)
            {
                tw.Write(" PRIVATE ");
            }
        }

        public override string ToString()
        {
            StringWriter sw = new StringWriter();
            Dump(sw, 0);
            return sw.ToString();
        }

        /// <summary>
        /// Dump this symbol in the given TextWriter instance
        /// </summary>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">Indentation level</param>
        public virtual void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(Name);
            this.Type?.Dump(tw, 0);
            DumpSymbolFlags(this.Flag, tw);
        }

        public virtual TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitSymbol(this, arg); }

        public class LevelExceed : Exception
        {
            /// <summary>
            /// The symbol which level exceed.
            /// </summary>
            public VariableSymbol Symbol
            {
                get;
                private set;
            }
            public LevelExceed(VariableSymbol symbol)
            {
                this.Symbol = symbol;
            }
        }

        /// <summary>
        /// A visitor for symbols.  A visitor is used to implement operations
        /// (or relations) on symbols.  Most common operations on symbols are
        ///  binary relations of the form : Symbol x TP -> TR
        /// </summary>
        /// <typeparam name="TR">the return type of the operation implemented by this visitor.
        /// </typeparam>
        /// <typeparam name="TP">the type of the second argument (the first being the
        /// symbol itself) of the operation implemented by this visitor.
        /// </typeparam>
        public interface IVisitor<out TR, in TP>
        {
            TR VisitFunctionSymbol(FunctionSymbol s, TP arg);
            TR VisitIndexSymbol(IndexSymbol s, TP arg);
            TR VisitNamespaceSymbol(NamespaceSymbol s, TP arg);
            TR VisitParagraphSymbol(ParagraphSymbol s, TP arg);
            TR VisitProgramSymbol(ProgramSymbol s, TP arg);
            TR VisitRedefinesSymbol(RedefinesSymbol s, TP arg);
            TR VisitRenamesSymbol(RenamesSymbol s, TP arg);
            TR VisitSectionSymbol(SectionSymbol s, TP arg);
            TR VisitTypedefSymbol(TypedefSymbol s, TP arg);
            TR VisitVariableSymbol(VariableSymbol s, TP arg);
            TR VisitVariableTypeSymbol(VariableTypeSymbol s, TP arg);
            TR VisitSymbol(Symbol s, TP arg);
        }

        /// <summary>
        /// The Abstract Symbol Visitor Class
        /// </summary>
        /// <typeparam name="TR"></typeparam>
        /// <typeparam name="TP"></typeparam>
        public abstract class AbstractSymbolVisitor<TR, TP> : IVisitor<TR, TP>
        {
            public TR Visit(Symbol s, TP arg) { return s.Accept(this, arg); }
            public virtual TR VisitFunctionSymbol(FunctionSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitIndexSymbol(IndexSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitNamespaceSymbol(NamespaceSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitParagraphSymbol(ParagraphSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitProgramSymbol(ProgramSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitRedefinesSymbol(RedefinesSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitRenamesSymbol(RenamesSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitSectionSymbol(SectionSymbol s, TP arg) { return VisitSymbol(s, arg); }            
            public virtual TR VisitTypedefSymbol(TypedefSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitVariableSymbol(VariableSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public virtual TR VisitVariableTypeSymbol(VariableTypeSymbol s, TP arg) { return VisitSymbol(s, arg); }
            public abstract TR VisitSymbol(Symbol s, TP arg);
        }

    }
}
