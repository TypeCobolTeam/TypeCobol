using System.Collections.Generic;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Section used by CFG : it contains sentences and paragraphs in order of appearance.
        /// </summary>
        private class Section : Procedure
        {
            private List<ProcedureDivisionPartition> _partitions;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the section.</param>
            /// <param name="name">Name of the section.</param>
            public Section(int number, string name)
                : base(number, name)
            {
                _partitions = null;
            }

            private void AddPartition(ProcedureDivisionPartition partition)
            {
                if (_partitions == null)
                {
                    _partitions = new List<ProcedureDivisionPartition>();
                }
                _partitions.Add(partition);
            }

            /// <summary>
            /// Add a sentence to this section.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            public override void AddSentence(Sentence sentence) => AddPartition(sentence);

            /// <summary>
            /// Add a paragraph to this section.
            /// </summary>
            /// <param name="paragraph">The paragraph to be added.</param>
            public void AddParagraph(Paragraph paragraph) => AddPartition(paragraph);

            public override IEnumerator<Sentence> GetEnumerator()
            {
                if (_partitions != null)
                {
                    //Iterate over all sentences of all parts.
                    foreach (var partition in _partitions)
                    {
                        foreach (var sentence in partition)
                        {
                            yield return sentence;
                        }
                    }
                }
            }
        }
    }
}
