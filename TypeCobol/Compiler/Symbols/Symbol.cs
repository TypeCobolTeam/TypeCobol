using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Base class of a Cobol Symbol
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
            Variable,//VariableSymbol and its inheritors except Index and Typedef
            Index,
            Section,
            Paragraph
        }

        [Flags]
        public enum Flags : ulong
        {
            Public = 0x01L << 0,
            Private = 0x01L << 1,
            External = 0x01L << 2,
            Global = 0x01L << 3, //Symbol set as global
            Volatile = 0x01L << 4, //Symbol explicitly marked as volatile
            FILE = 0x01L << 5,
            GLOBAL_STORAGE = 0x01L << 6,
            WORKING_STORAGE = 0x01L << 7,
            LOCAL_STORAGE = 0x01L << 8,
            LINKAGE = 0x01L << 9,
            Input = 0x01L << 10,
            Output = 0x01L << 11,
            Inout = 0x01L << 12,
            ByReference = 0x01L << 13,
            ByContent = 0x01L << 14,
            ByValue = 0x01L << 15,
            Strong = 0x01L << 16,
            Strict = 0x01L << 17,
            Weak = 0x01L << 18,

            //Some new Symbols Modifiers-----------------------
            Based = 0x01L << 19,
            AnyLength = 0x01L << 20,
            GroupUsageBit = 0x01L << 21,
            GroupUsageNational = 0x01L << 22,
            GroupUsageUTF8 = 0x01L << 23,
            //-------------------------------------------------

            //Symbols Modifiers that have Type Equality impact
            //along with PICTURE and USAGE.
            BlankWhenZero = 0x01L << 24,
            DynamicLength = 0x01L << 25,
            Justified = 0x01L << 26,
            SeparateSign = 0x01L << 27,
            LeadingSign = 0x01L << 28,
            TrailingSign = 0x01L << 29,
            Sync = 0x01L << 30, //May be SYNC LEFT or SYNC RIGHT
            //-------------------------------------------------

            Redefines = 0x01L << 31,
            HasATypedefType = 0x01L << 32,//The symbol has a type that comes from a TypeDef.
            Parameter = 0x01L << 33,//This a parameter variable.
            Returning = 0x01L << 34,//A Return variable.
            InsideTypedef = 0x01L << 35,//Flag of any symbol inside a Typedef definition.
            BuiltinSymbol = 0x01L << 36, //This is a Builtin symbol.
            
            //Etc... (Max = 0x01L << 62)
        }

        /// <summary>
        /// Named constructor
        /// </summary>
        protected Symbol(string name, Kinds kind)
        {
            this.Name = name ?? "";//It happens that a symbol can have no name
            Kind = kind;
        }

        /// <summary>
        /// Symbol Kind
        /// </summary>
        public Kinds Kind { get; }

        /// <summary>
        /// Symbol Flags.
        /// </summary>
        public Flags Flag
        {
            get;
            internal set;
        }

        /// <summary>
        /// Symbol's name
        /// </summary>
        public string Name
        {
            get;
            internal set;
        }

        /// <summary>
        /// The target AST node if any
        /// </summary>
        public Node TargetNode
        {
            get;
            internal set;
        }

        private IEnumerable<string> Path
        {
            get
            {
                if (Owner != null)
                {
                    foreach (var part in Owner.Path)
                    {
                        yield return part;
                    }
                }

                if (!string.IsNullOrEmpty(Name))
                {
                    yield return Name;
                }
            }
        }

        /// <summary>
        /// Full qualified name of this Symbol à la TypeCobol using "::"
        /// </summary>
        public virtual string FullName => string.Join("::", Path);

        /// <summary>
        /// Full qualified name of this Symbol à la COBOL85 using OF
        /// </summary>
        public virtual string FullOfName => string.Join(" OF ", Path.Reverse());

        /// <summary>
        /// Full dotted qualified name
        /// </summary>
        public virtual string FullDotName => string.Join(".", Path);

        /// <summary>
        /// Type changed event.
        /// </summary>
        public event EventHandler TypeChanged;

        Types.Type _type;
        /// <summary>
        /// Symbol's type
        /// </summary>
        public virtual Types.Type Type
        {
            get => _type;
            set
            {
                if (_type != value)
                {
                    _type = value;
                    TypeChanged?.Invoke(this, null);
                }
            }
        }

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        /// <param name="propagate">true the flags must be propagated, false otherwise</param>
        /// <remarks>Do not propagate flags before type expansion except for InsideTypedef which is allowed on initial symbol definitions.</remarks>
        internal virtual void SetFlag(Flags flag, bool value, bool propagate = false)
        {
            this.Flag = value ? (Flags)((ulong)this.Flag | (ulong)flag)
                              : (Flags)((ulong)this.Flag & ~(ulong)flag);
            if (Type != null && propagate)
            {
                //Propagate through types.
                Type.PropagateSymbolFlag(flag, value);
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
            internal set;
        }

        public SemanticKinds SemanticKind => SemanticKinds.Symbol;

        public virtual object Clone()
        {
            return MemberwiseClone();
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
        public Symbol NearestParent(params Kinds[] kinds)
        {
            System.Diagnostics.Debug.Assert(kinds != null);
            System.Diagnostics.Debug.Assert(kinds.Length > 0);
            return kinds.Contains(this.Kind) ? this : Owner?.NearestParent(kinds);
        }

        /// <summary>
        /// Lookup for the parent having the given Level
        /// </summary>
        /// <param name="level">Target level</param>
        /// <param name="inclusive">true if this symbol must be taken in account, false otherwise</param>
        /// <returns>The parent symbol of the level if one exists, null otherwise</returns>
        public virtual Symbol LookupParentLevelSymbol(int level, bool inclusive)
        {
            return null;
        }

        public override string ToString()
        {
            var output = new StringWriter();
            Dump(output, 0);
            return output.ToString();
        }

        public virtual void Dump(TextWriter output, int indentLevel)
        {
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine($"{Name} (.NET Type={GetType().Name}, Kind={Kind})");
            if (Flag != 0)
            {
                output.Write(indent);
                output.WriteLine($"Flags: [{string.Join(", ", GetFlagsLabels())}]");
            }

            if (Owner != null)
            {
                output.Write(indent);
                output.WriteLine($"Owner: {Owner.FullName}");//Write reference
            }

            if (Type != null)
            {
                output.Write(indent);
                output.WriteLine("Type:");
                Type.Dump(output, indentLevel + 1);
            }

            IEnumerable<string> GetFlagsLabels()
            {
                foreach (Flags flag in Enum.GetValues(typeof(Flags)))
                    if (Flag.HasFlag(flag))
                        yield return flag.ToString();
            }
        }

        public virtual TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitSymbol(this, arg);
        }

        /// <summary>
        /// A visitor for symbols.  A visitor is used to implement operations
        /// (or relations) on symbols.  Most common operations on symbols are
        ///  binary relations of the form : Symbol x TParameter -> TResult
        /// </summary>
        /// <typeparam name="TResult">the return type of the operation implemented by this visitor.
        /// </typeparam>
        /// <typeparam name="TParameter">the type of the second argument (the first being the
        /// symbol itself) of the operation implemented by this visitor.
        /// </typeparam>
        public interface IVisitor<out TResult, in TParameter>
        {
            TResult VisitFunctionSymbol(FunctionSymbol s, TParameter arg);
            TResult VisitIndexSymbol(IndexSymbol s, TParameter arg);
            TResult VisitParagraphSymbol(ParagraphSymbol s, TParameter arg);
            TResult VisitProgramSymbol(ProgramSymbol s, TParameter arg);
            TResult VisitRedefinesSymbol(RedefinesSymbol s, TParameter arg);
            TResult VisitSectionSymbol(SectionSymbol s, TParameter arg);
            TResult VisitTypedefSymbol(TypedefSymbol s, TParameter arg);
            TResult VisitVariableSymbol(VariableSymbol s, TParameter arg);
            TResult VisitTypedVariableSymbol(TypedVariableSymbol s, TParameter arg);
            TResult VisitSymbol(Symbol s, TParameter arg);
        }

        /// <summary>
        /// The Abstract Symbol Visitor Class
        /// </summary>
        /// <typeparam name="TResult">Return type of the visitor</typeparam>
        /// <typeparam name="TParameter">Argument type of the visitor</typeparam>
        public abstract class AbstractSymbolVisitor<TResult, TParameter> : IVisitor<TResult, TParameter>
        {
            public TResult Visit(Symbol s, TParameter arg) { return s.Accept(this, arg); }
            public virtual TResult VisitFunctionSymbol(FunctionSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitIndexSymbol(IndexSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitParagraphSymbol(ParagraphSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitProgramSymbol(ProgramSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitRedefinesSymbol(RedefinesSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitSectionSymbol(SectionSymbol s, TParameter arg) { return VisitSymbol(s, arg); }            
            public virtual TResult VisitTypedefSymbol(TypedefSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitVariableSymbol(VariableSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public virtual TResult VisitTypedVariableSymbol(TypedVariableSymbol s, TParameter arg) { return VisitSymbol(s, arg); }
            public abstract TResult VisitSymbol(Symbol s, TParameter arg);
        }
    }
}
