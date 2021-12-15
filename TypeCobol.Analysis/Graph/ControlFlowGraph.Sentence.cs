using System.Collections.Generic;

namespace TypeCobol.Analysis.Graph
{
	public partial class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// A Cobol sentence used by CFG, a sentence is made of one or several basic blocks.
        /// </summary>
        public class Sentence : ProcedureDivisionRegion
        {
            private readonly LinkedList<BasicBlock<N, D>> _blocks;

            /// <summary>
            /// This is the index of the first block in the global SuccessorEdges list.
            /// Null if the first block is not a successor (meaning first block is root block).
            /// </summary>
            public int? FirstBlockIndex { get; }

            /// <summary>
            /// The procedure in which this sentence appears.
            /// </summary>
            public Procedure ParentProcedure { get; }

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the sentence.</param>
            /// <param name="firstBlock">First block of the sentence.</param>
            /// <param name="firstBlockIndex">Index of the first block the global SuccessorEdges list.
            /// Pass null if the first block is a root block and consequently has no index in successors list.</param>
            /// <param name="parentProcedure">Parent procedure.</param>
            internal Sentence(int number, BasicBlock<N, D> firstBlock, int? firstBlockIndex, Procedure parentProcedure)
                : base(number)
            {
	            _blocks = new LinkedList<BasicBlock<N, D>>();
                _blocks.AddLast(firstBlock);
                FirstBlockIndex = firstBlockIndex;
                ParentProcedure = parentProcedure;

                if (parentProcedure != null) //See issue #2081
                {
	                parentProcedure.AddSentence(this);

	                //Give to the first block the name of its paragraph/section as tag.
	                firstBlock.Tag = parentProcedure.Name;
                }
            }

            /// <summary>
            /// First block of this sentence.
            /// </summary>
            public BasicBlock<N, D> FirstBlock => _blocks.First.Value;

            /// <summary>
            /// All blocks of this sentence.
            /// </summary>
            public IEnumerable<BasicBlock<N, D>> Blocks => _blocks;

            /// <summary>
            /// Add a block to this sentence.
            /// </summary>
            /// <param name="block">The block to be added.</param>
            internal void AddBlock(BasicBlock<N, D> block) => _blocks.AddLast(block);

            public override IEnumerator<Sentence> GetEnumerator()
            {
                yield return this;
            }
        }
    }
}
