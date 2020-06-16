using System.Collections;
using System.Collections.Generic;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Base class for Procedure and Sentence.
        /// A ProcedureDivisionPartition holds statements and has a number
        /// which identifies its order of appearance in the ProcedureDivision.
        /// </summary>
        /// <remarks>As a convenience, it is also an IEnumerable of Sentence.
        /// A sentence would only return itself and a Procedure would return its own sentences</remarks>
        private abstract class ProcedureDivisionPartition : IEnumerable<Sentence>
        {
            /// <summary>
            /// Order number of appearance in the ProcedureDivision of a program or function.
            /// </summary>
            public int Number { get; }

            protected ProcedureDivisionPartition(int number)
            {
                Number = number;
            }

            public abstract IEnumerator<Sentence> GetEnumerator();

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }
        }
    }
}
