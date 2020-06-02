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
    public class BasicBlock<N, D> : ICloneable
    {
        /// <summary>
        /// BasicBlock Index
        /// </summary>
        public int Index
        {
            get;
            protected internal set;
        }
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

            Resolved = 0x01 << 0,       //Flag if this basic block is totally resolved.
            Ending = 0x01 << 1,         //Flag if this basic block is an ending block.
            Default = 0x01 << 2,        //Flag if this basic block is  default block for instance a WhenOther block.
            Declaratives = 0x01 << 3,   //Flag if this basic block is inside a declaratives section.
            Start = 0x01 << 4,           //Flag for a start node.
            End = 0x01 << 5,           //Flag for a end node.
            GroupGrafted = 0x01 << 6,           //Flag a Grafted Group.
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
        /// Determines if this block can be considered as a END block.
        /// </summary>
        public bool MaybeEndBlock
        {
            get
            {
                return HasFlag(Flags.End) || (SuccessorEdges.Count == 0 && Instructions.Count == 0);
            }
        }

        public object Clone()
        {
            return MemberwiseClone();
        }

        /// <summary>
        /// Instruction to string
        /// </summary>
        /// <param name="instruction">The instruction to get the string representation</param>
        /// <returns>The string representation of the instruction.</returns>
        protected virtual string InstructionToString(N instruction)
        {
            return instruction == null ? "<null>" : instruction.ToString();
        }
        /// <summary>
        /// String representation of a block.
        /// </summary>
        /// <returns>The string representation of a block.</returns>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("Block[" + Index + "] { ");
            foreach (N i in Instructions)
            {
                sb.AppendLine(InstructionToString(i));
            }
            sb.AppendLine("}");
            return sb.ToString();
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
