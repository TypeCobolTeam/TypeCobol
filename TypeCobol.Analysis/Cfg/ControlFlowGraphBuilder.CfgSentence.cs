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
    }
}