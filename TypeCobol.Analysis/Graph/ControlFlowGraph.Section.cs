using System.Collections.Generic;
using System.Linq;

namespace TypeCobol.Analysis.Graph
{
    public partial class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// A Section used by CFG : it contains sentences and paragraphs in order of appearance.
        /// </summary>
        public class Section : Procedure
        {
            private List<Sentence> _preambleSentences;
            private List<Paragraph> _paragraphs;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the section.</param>
            /// <param name="name">Name of the section.</param>
            internal Section(int number, string name)
                : base(number, name)
            {
                _preambleSentences = null;
                _paragraphs = null;
            }

            /// <summary>
            /// Beginning sentences of the section which are not included in a paragraph
            /// </summary>
            public IEnumerable<Sentence> PreambleSentences => _preambleSentences ?? Enumerable.Empty<Sentence>();

            /// <summary>
            /// Paragraphs defined in this section
            /// </summary>
            public IEnumerable<Paragraph> Paragraphs => _paragraphs ?? Enumerable.Empty<Paragraph>();

            /// <summary>
            /// Add a sentence to this section.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            internal override void AddSentence(Sentence sentence)
            {
                System.Diagnostics.Debug.Assert(_paragraphs == null); //Cannot add sentence if a paragraph is already 'opened'
                if (_preambleSentences == null)
                {
                    _preambleSentences = new List<Sentence>();
                }
                _preambleSentences.Add(sentence);
            }

            /// <summary>
            /// Add a paragraph to this section.
            /// </summary>
            /// <param name="paragraph">The paragraph to be added.</param>
            internal void AddParagraph(Paragraph paragraph)
            {
                if (_paragraphs == null)
                {
                    _paragraphs = new List<Paragraph>();
                }
                _paragraphs.Add(paragraph);
            }

            public override IEnumerator<Sentence> GetEnumerator()
            {
                //Iterate over preamble sentences,
                if (_preambleSentences != null)
                {
                    foreach (var preambleSentence in _preambleSentences)
                    {
                        yield return preambleSentence;
                    }
                }

                //and then over paragraphs.
                if (_paragraphs != null)
                {
                    foreach (var paragraph in _paragraphs)
                    {
                        foreach (var sentence in paragraph)
                        {
                            yield return sentence;
                        }
                    }
                }
            }

            internal override void AccumulateSentencesThrough(List<Sentence> sentences, Procedure end, out Procedure last)
            {
                foreach (var preambleSentence in PreambleSentences)
                {
                    sentences.Add(preambleSentence);
                }

                last = null;
                foreach (var paragraph in Paragraphs)
                {
                    paragraph.AccumulateSentencesThrough(sentences, end, out last);
                    if (paragraph == end)
                    {
                        break;
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
