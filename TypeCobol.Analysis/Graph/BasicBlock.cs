using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// A Block of instruction of which the first instruction can be reached by an explicit control flow.
    /// </summary>
    /// <typeparam name="N">The Type Variable of an instruction</typeparam>
    /// <typeparam name="D">The Type Variable for any extra Data information</typeparam>
    public class BasicBlock<N, D>
    {
        /// <summary>
        /// Instructions making up this block.
        /// </summary>
        public LinkedList<N> Instructions
        {
            get;
            internal set;
        }

        /// <summary>
        /// Data Information of this block.
        /// </summary>
        public D Data
        {
            get;
            set;
        }

        /// <summary>
        /// The list of edge that leaves this block. The edges entries in the SuccessorEdges list of the ControlFlowGraph that contains this block.
        /// </summary>
        public List<int> SuccessorEdges
        {
            get;
            set;
        }

        /// <summary>
        /// Flag on a basic block.
        /// </summary>
        [Flags]
        public enum Flags : uint
        {

            Resolved = 0x01 << 0, //Flag if this basic block is totally resolved.
            Ending = 0x01 << 1, //Flag if this basic block is an ending block.
            Default = 0x01 << 2, //Flag if this basic block is  default block for instance a WhenOther block.
        }

        /// <summary>
        /// Symbol Flags.
        /// </summary>
        public Flags Flag
        {
            get;
            internal set;
        }

        /// <summary>
        /// Set a set of flags to true or false.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        internal virtual void SetFlag(Flags flag, bool value)
        {
            this.Flag = value ? (Flags)(this.Flag | flag)
                              : (Flags)(this.Flag & ~flag);
        }

        /// <summary>
        /// Determines if the given flag is set.
        /// </summary>
        /// <param name="flag">The flag to be tested</param>
        /// <returns>true if yes, false otherwise.</returns>
        public bool HasFlag(Flags flag)
        {
            return (this.Flag & flag) != 0;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public BasicBlock()
        {
            Instructions = new LinkedList<N>();
            SuccessorEdges = new List<int>(2);
        }
    }
}
