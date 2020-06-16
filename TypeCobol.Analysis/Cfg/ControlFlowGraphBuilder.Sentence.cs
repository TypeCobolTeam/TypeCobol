using System.Collections.Generic;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Sentence used for our builder. A Sentence is a special symbol.
        /// </summary>
        internal class CfgSentence : Symbol
        {
            /// <summary>
            /// First Block associated to this sentence.
            /// </summary>
            internal BasicBlockForNode Block
            {
                get;
                set;
            }

            /// <summary>
            /// The Block Index Associated to the Block.
            /// </summary>
            internal int BlockIndex
            {
                get;
                set;
            }

            /// <summary>
            /// All blocks in this Sentence.
            /// </summary>
            internal LinkedList<BasicBlockForNode> AllBlocks
            {
                get;
                set;
            }

            /// <summary>
            /// Add a block in this sequence.
            /// </summary>
            /// <param name="block">The block to be added</param>
            internal void AddBlock(BasicBlockForNode block)
            {
                if (AllBlocks == null)
                {
                    AllBlocks = new LinkedList<BasicBlockForNode>();
                }
                AllBlocks.AddLast(block);
            }

            /// <summary>
            /// Sentence counter
            /// </summary>
            private static int SentenceCounter = 0;
            /// <summary>
            /// ctor
            /// </summary>
            public CfgSentence() : base("<<Sentence>>(" + (SentenceCounter++) + ")", Kinds.Sentence)
            {
                BlockIndex = -1;
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
        /// A Cobol sentence used by CFG, a sentence is made of one or several basic blocks.
        /// </summary>
        private class Sentence : ProcedureDivisionPartition
        {
            private readonly LinkedList<BasicBlockForNode> _blocks;

            /// <summary>
            /// This is the index of the first block in the global SuccessorEdges list.
            /// Null if the first block is not a successor (meaning first block is root block).
            /// </summary>
            public int? FirstBlockIndex { get; }

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance of the sentence.</param>
            /// <param name="firstBlock">First block of the sentence.</param>
            /// <param name="firstBlockIndex">Index of the first block the global SuccessorEdges list.
            /// Pass null if the first block is a root block and consequently has no index in successors list.</param>
            public Sentence(int number, BasicBlockForNode firstBlock, int? firstBlockIndex)
                : base(number)
            {
                _blocks = new LinkedList<BasicBlockForNode>();
                _blocks.AddLast(firstBlock);
                FirstBlockIndex = firstBlockIndex;
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
        }
    }
}
