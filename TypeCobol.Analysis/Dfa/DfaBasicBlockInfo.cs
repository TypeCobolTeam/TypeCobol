using System.Collections.Generic;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Basic Block information for Data Flow Analysis.
    /// </summary>
    /// <typeparam name="V">Type of variables</typeparam>
    public class DfaBasicBlockInfo<V>
    {
        /// <summary>
        /// Generated definitions bit set: The set of locally available defintions in b.
        /// This is a set of data which is created to identify which data definitions are made within a basic block.
        /// </summary>
        public Util.BitSet Gen
        {
            get;
            internal set;
        }

        /// <summary>
        /// Killed definitions bit set: The Kill set represents those variables defined outside the basic block
        /// which also have definitions inside the block.
        /// </summary>
        public Util.BitSet Kill
        {
            get;
            internal set;
        }

        /// <summary>
        /// Definitions live on entry to basic block.
        /// </summary>
        public Util.BitSet In
        {
            get;
            internal set;
        }

        /// <summary>
        /// Definitions live on basic block exit
        /// </summary>
        public Util.BitSet Out
        {
            get;
            internal set;
        }

        /// <summary>
        /// The dictionary of GEN Variable within the block to their DEF List index.
        /// </summary>
        internal Dictionary<V,int> GenVariableDictionary
        {
            get;
            set;
        }

        /// <summary>
        /// The Index of the first entry in a Cfg UseList of the first use in this block.
        /// </summary>
        public int UseListFirstIndex
        {
            get;
            internal set;
        }

        /// <summary>
        /// Count of uses in this block
        /// </summary>
        public int UseCount
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Index of the first entry in a Cfg DefList of the first definition in this block.
        /// </summary>
        public int DefListFirstIndex
        {
            get;
            internal set;
        }

        /// <summary>
        /// Count of definition in this block
        /// </summary>
        public int DefCount
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public DfaBasicBlockInfo()
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cardinality">The cardinality of the dataflow domain</param>
        public DfaBasicBlockInfo(int cardinality)
        {
            Gen = new Util.BitSet(cardinality);
            Kill = new Util.BitSet(cardinality);
            In = new Util.BitSet(cardinality);
            Out = new Util.BitSet(cardinality);
        }
    }
}
