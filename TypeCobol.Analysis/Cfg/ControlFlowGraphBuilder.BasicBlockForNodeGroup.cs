using System.Collections;
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
        /// to group all BasicBlock of the target Sentences or Paragraphs.
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
            /// This is the Index Entry in the Successors array, in to which this Group is stored
            /// as successor of the Predecessor block. Used for iterative PERFORM procedure.
            /// </summary>
            internal int EntryIndexInSuccessors
            {
                get;
                set;
            }

            /// <summary>
            /// Is this group for an iterative instruction.
            /// </summary>
            internal bool IsIterativeGroup
            {
                get;
                set;
            }

            /// <summary>
            /// Is this group for iterative instruction with an AFTER clause.
            /// </summary>
            internal bool IsAfterIterativeGroup
            {
                get;
                set;
            }

            /// <summary>
            /// To detect recursivity on PERFORM Procedure calls.
            /// This is a bit array of GroupIndex encountered during the workflow
            /// call of other PERFORM.
            /// </summary>
            internal BitArray RecursivityGroupSet
            {
                get;
                set;
            }

            /// <summary>
            /// To detect recursivity on PERFORM Procedure calls.
            /// This is a bit array of GroupIndex encountered during the workflow
            /// call of other PERFORM.
            /// </summary>
            internal BitArray RecursivityGroupSet
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
                EntryIndexInSuccessors = -1;
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