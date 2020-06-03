using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Section symbol used by Cfg : it contains Sentences and Paragraphs in order of appearance.
        /// </summary>
        internal class CfgSectionSymbol : SectionSymbol
        {
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="name">Section's name</param>
            public CfgSectionSymbol(string name) : base(name)
            {
                SentencesParagraphs = new Domain<Symbol>(this);
            }

            /// <summary>
            /// All sentences and paragraphs in this section in the order of appearance.
            /// </summary>
            public Domain<Symbol> SentencesParagraphs
            {
                get;
                protected set;
            }

            /// <summary>
            /// Set flags
            /// </summary>
            /// <param name="flag">The flag to be set</param>
            /// <param name="value">The value to set</param>
            internal void SetFlag(Symbol.Flags flag, bool value)
            {
                base.SetFlag(flag, value, false);
            }
        }
    }
}