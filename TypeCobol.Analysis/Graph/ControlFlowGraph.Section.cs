using System.Collections.Generic;

namespace TypeCobol.Analysis.Graph
{
	public partial class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// A Section used by CFG : it contains sentences and paragraphs in order of appearance.
        /// </summary>
        public class Section : Procedure
        {
            private List<ProcedureDivisionRegion> _regions;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the section.</param>
            /// <param name="name">Name of the section.</param>
            internal Section(int number, string name)
                : base(number, name)
            {
                _regions = null;
            }

            private void AddRegion(ProcedureDivisionRegion region)
            {
                if (_regions == null)
                {
                    _regions = new List<ProcedureDivisionRegion>();
                }
                _regions.Add(region);
            }

            /// <summary>
            /// Add a sentence to this section.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            internal override void AddSentence(Sentence sentence) => AddRegion(sentence);

            /// <summary>
            /// Add a paragraph to this section.
            /// </summary>
            /// <param name="paragraph">The paragraph to be added.</param>
            internal void AddParagraph(Paragraph paragraph) => AddRegion(paragraph);

            public override IEnumerator<Sentence> GetEnumerator()
            {
                if (_regions != null)
                {
                    //Iterate over all sentences of all regions.
                    foreach (var region in _regions)
                    {
                        foreach (var sentence in region)
                        {
                            yield return sentence;
                        }
                    }
                }
            }

            internal override void AccumulateSentencesThrough(List<Sentence> sentences, Procedure end, out Procedure last)
            {
                last = null;
                if (_regions != null)
                {
                    foreach (var region in _regions)
                    {
                        region.AccumulateSentencesThrough(sentences, end, out last);
                        if (region == end)
                        {
                            break;
                        }
                    }
                }

                if (last == null)
                {
                    //Empty section or made only of sentences
                    last = this;
                }
            }
        }
    }
}
