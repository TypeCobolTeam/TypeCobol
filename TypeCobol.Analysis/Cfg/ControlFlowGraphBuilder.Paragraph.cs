using System.Collections.Generic;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Paragraph used by a CFG, it contains sentences.
        /// Also in CFG, paragraphs always have a parent section.
        /// </summary>
        private class Paragraph : Procedure
        {
            private List<Sentence> _sentences;

            /// <summary>
            /// Section in which this paragraph is declared.
            /// If the paragraph is declared directly in procedure division, its
            /// parent section is the special root section.
            /// </summary>
            public Section ParentSection { get; }

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the paragraph.</param>
            /// <param name="name">Name of the paragraph.</param>
            /// <param name="parentSection">Section in which this paragraph is declared.</param>
            /// <remarks>The paragraph is added into the supplied parent section.</remarks>
            public Paragraph(int number, string name, Section parentSection)
                : base(number, name)
            {
                _sentences = null;
                ParentSection = parentSection;
                ParentSection.AddParagraph(this);
            }

            /// <summary>
            /// Add a sentence to this paragraph.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            public override void AddSentence(Sentence sentence)
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
