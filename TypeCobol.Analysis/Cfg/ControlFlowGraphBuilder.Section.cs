using System.Collections.Generic;
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
            public void AddSentence(Sentence sentence) => AddPartition(sentence);

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
