using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
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
            Global,//The Global Symbol table
            Namespace,
            Program,
            Function,
            Typedef,
            Variable,
            Index,
            Section,
            Paragraph
        }

        public enum Flags : ulong
        {
            Public = 0x01 << 0,
            Private = 0x01 << 1,
            External = 0x01 << 2,
            FILE_SECTION = 0x01 << 3,
            GLOBAL_STORAGE = 0x01 << 4,
            WORKING_STORAGE = 0x01 << 5,
            LOCAL_STORAGE = 0x01 << 6,
            LINKAGE = 0x01 << 7,
            Input = 0x01 << 8,
            Output = 0x01 << 9,
            Inout = 0x01 << 10,
            ByReference = 0x01 << 11,
            ByContent = 0x01 << 12,
            Strict = 0x01 << 13,
            Weak = 0x01 << 14,
            Conditions = 0x01 << 15,
            Renames = 0x01 << 16,
            Rededines = 0x01 << 17,
            HasATypedefType = 0x01 << 18,//The symbol has a type that comes from a TypeDef.
            //Etc...
        }

        /// <summary>
        /// Empty constructor
        /// </summary>
        protected Symbol()
        {
        }

        /// <summary>
        /// Named constructor
        /// </summary>
        protected Symbol(String name, Kinds kind)
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
            set;
        }

        /// <summary>
        /// Symbol's name
        /// </summary>
        public String Name
        {
            get;
            internal set;
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
            get
            {
                return m_Type;
            }
            set
            {
                if (m_Type != value)
                {
                    m_Type = value;
                    if (TypeChanged != null)
                        TypeChanged(this, null);
                }
            }
        }

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        /// <param name="propagate">true the flags must be propagated, false otherwise</param>
        public virtual void SetFlag(Flags flag, bool value, bool propagate = false)
        {
            this.Flag = value ? (Flags)((ulong)this.Flag | (ulong)flag)
                              : (Flags)((ulong)this.Flag & ~(ulong)flag);
            if (Type != null && propagate)
            {//Propagate to types.
                Type.SetFlag(flag, value);
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
        /// Check this Symbol if it is well formed, well defined.
        /// </summary>
        /// <param name="varSymbolIndex">The function used to </param>
        /// <returns>True if this symbol is valid, false otherwise.</returns>
        public virtual bool Check(Func<uint> varSymbolIndex)
        {
            if (Type != null)
            {
                Type.Expand(this, false, varSymbolIndex);
            }
            return Type != null;
        }

        /// <summary>
        /// The Owner of this Symbol
        /// </summary>
        public Symbol Owner
        {
            get;
            set;
        }

        public SemanticKinds SemanticKind
        {
            get { return SemanticKinds.Symbol; }
        }

        /// <summary>
        /// Determines if this symbol cans be seen as Scope.
        /// </summary>
        public bool HasScope
        {
            get
            {
                return Kind == Kinds.Global || Kind == Kinds.Namespace || Kind == Kinds.Program ||
                       Kind == Kinds.Function;
            }
        }

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
    }
}
