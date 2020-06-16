using System.Collections.Generic;
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
            /// <param name="name">Paragraph's name</param>
            public CfgParagraphSymbol(string name) : base(name)
            {
                Sentences = new Domain<CfgSentence>(this);
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

        /// <summary>
        /// A Paragraph used by a CFG, it contains sentences.
        /// </summary>
        private class Paragraph : Procedure
        {
            private List<Sentence> _sentences;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the paragraph.</param>
            /// <param name="name">Name of the paragraph.</param>
            public Paragraph(int number, string name)
                : base(number, name)
            {
                _sentences = null;
            }

            /// <summary>
            /// Add a sentence to this paragraph.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            public void AddSentence(Sentence sentence)
            {
                if (_sentences == null)
                {
                    _sentences = new List<Sentence>();
                }
                _sentences.Add(sentence);
            }

            public override IEnumerator<Sentence> GetEnumerator()
            {
                if (_sentences != null)
                {
                    foreach (var sentence in _sentences)
                    {
                        yield return sentence;
                    }
                }
            }
        }
    }
}
