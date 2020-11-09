namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Generic class representing a Definition Point.
    /// </summary>
    /// <typeparam name="I">Instruction generic Type</typeparam>
    /// <typeparam name="V">Used Variable generic Type</typeparam>
    public class DfaDefPoint<I, V>
    {
        /// <summary>
        /// The Definition index.
        /// </summary>
        public int Index
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Instruction
        /// </summary>
        public I Instruction
        {
            get;
            internal set;
        }

        /// <summary>
        /// Instruction's index with its basic bloc.
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
        /// User data
        /// </summary>
        public object UserData
        {
            get;
            set;
        }
    }
}
