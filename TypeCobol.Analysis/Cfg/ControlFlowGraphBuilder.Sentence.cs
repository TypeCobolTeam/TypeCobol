using System.Collections.Generic;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Cobol sentence used by CFG, a sentence is made of one or several basic blocks.
        /// </summary>
        private class Sentence : ProcedureDivisionRegion
        {
            private readonly LinkedList<BasicBlockForNode> _blocks;

            /// <summary>
            /// This is the index of the first block in the global SuccessorEdges list.
            /// Null if the first block is not a successor (meaning first block is root block).
            /// </summary>
            public int? FirstBlockIndex { get; }

            /// <summary>
            /// The associated procedure
            /// </summary>
            internal readonly Procedure Procedure;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the sentence.</param>
            /// <param name="firstBlock">First block of the sentence.</param>
            /// <param name="firstBlockIndex">Index of the first block the global SuccessorEdges list.
            /// <param name="procedure">the procedure to which the sentence belongs.</param>
            /// Pass null if the first block is a root block and consequently has no index in successors list.</param>
            public Sentence(int number, BasicBlockForNode firstBlock, int? firstBlockIndex, Procedure procedure)
                : base(number)
            {
                _blocks = new LinkedList<BasicBlockForNode>();
                _blocks.AddLast(firstBlock);
                FirstBlockIndex = firstBlockIndex;
                this.Procedure = procedure;
            }

            /// <summary>
            /// First block of this sentence.
            /// </summary>
            public BasicBlockForNode FirstBlock => _blocks.First.Value;

            /// <summary>
            /// All blocks of this sentence.
            /// </summary>
            public IEnumerable<BasicBlockForNode> Blocks => _blocks;

            /// <summary>
            /// Add a block to this sentence.
            /// </summary>
            /// <param name="block">The block to be added.</param>
            public void AddBlock(BasicBlockForNode block) => _blocks.AddLast(block);

            public override IEnumerator<Sentence> GetEnumerator()
            {
                yield return this;
            }

            public override void AccumulateSentencesThrough(List<Sentence> sentences, Procedure end, out Procedure last)
            {
                sentences.Add(this);
                last = null;
            }
        }
    }
}
