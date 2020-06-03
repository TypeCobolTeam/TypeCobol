using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Paragraph symbol used by a CFG, it contains Sentence symbols.
        /// </summary>
        internal class CfgParagraphSymbol : ParagraphSymbol
        {
            /// <summary>
            /// All sentences in this paragraph in the order of appearance
            /// </summary>
            public Domain<CfgSentence> Sentences
            {
                get;
                protected set;
            }

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="name">Pargarph's name</param>
            public CfgParagraphSymbol(string name) : base(name)
            {
                Sentences = new Domain<CfgSentence>(this);
            }
            /// <summary>
            /// Set flags
            /// </summary>
            /// <param name="flag">The falg to be set</param>
            /// <param name="value">The value to set</param>
            internal void SetFlag(Symbol.Flags flag, bool value)
            {
                base.SetFlag(flag, value, false);
            }
        }
    }
}