using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// A Basic Block which contains a list of BasicBlocks.
        /// Such Group is used for PERFORM Procedure instruction block,
        /// to group all BasicBlock of the taget Sentences or Paragraphs.
        /// </summary>
        public class BasicBlockForNodeGroup : BasicBlockForNode
        {
            public LinkedList<BasicBlock<Node, D>> Group
            {
                get;
                set;
            }

            public int GroupIndex
            {
                get;
                internal set;
            }

            /// <summary>
            /// Terminal blocks within this Group.
            /// </summary>
            internal List<BasicBlockForNode> TerminalBlocks
            {
                get;
                set;
            }

            /// <summary>
            /// Constructor.
            /// </summary>
            public BasicBlockForNodeGroup()
            {
                Group = new LinkedList<BasicBlock<Node, D>>();
            }

            /// <summary>
            /// Add a block to this Group.
            /// </summary>
            /// <param name="block">The block to be added.</param>
            public void AddBlock(BasicBlockForNode block)
            {
                Group.AddLast(block);
            }
        }
    }
}