namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Generic class representing a USE Point.
    /// </summary>
    /// <typeparam name="I">Instruction generic Type</typeparam>
    /// <typeparam name="V">Used Variable generic Type</typeparam>
    public class DfaUsePoint<I, V>
    {
        /// <summary>
        /// The Instruction
        /// </summary>
        public I Instruction
        {
            get;
            internal set;
        }

        /// <summary>
        /// Instruction's index with its basic block.
        /// </summary>
        public int InstructionIndex
        {
            get;
            internal set;
        }

        /// <summary>
        /// 
        /// </summary>
        public V Variable
        {
            get;
            internal set;
        }

        /// <summary>
        /// Index of the Owner basic Block
        /// </summary>
        public int BlockIndex
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Use-Def set: set of information which tells uses of this variable 
        /// within a basic block are defined.
        /// </summary>
        public Util.BitSet UseDef
        {
            get;
            internal set;
        }
    }
}
