using TypeCobol.Compiler.Types;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Base classe of a Cobol Symbol
    /// </summary>
    public abstract class Symbol : ISemanticData
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

        public enum Flags
        {
            Public = 0x01 << 0,
            Private = 0x01 << 2,
            External = 0x01 << 3,
            WORKING_STORAGE = 0x01 << 4,
            LOCAL_STORAGE = 0x01 << 5,
            LINKAGE = 0x01 << 6,
            Input = 0x01 << 7,
            Output = 0x01 << 8,
            Inout = 0x01 << 9,
            ByReference = 0x01 << 10,
            ByContent = 0x01 << 11,
            Strict = 0x01 << 12,
            Weak = 0x01 << 13,
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
        protected Symbol(string name, Kinds kind)
        {
            this.Name = name;
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
        public string Name
        {
            get;
            internal set;
        }

        /// <summary>
        /// Type changed event.
        /// </summary>
        public event System.EventHandler TypeChanged;

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
    }
}
