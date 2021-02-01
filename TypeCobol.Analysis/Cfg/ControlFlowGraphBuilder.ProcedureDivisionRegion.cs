using System.Collections;
using System.Collections.Generic;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Base class for Procedure and Sentence.
        /// A ProcedureDivisionRegion holds statements and has a number
        /// which identifies its order of appearance in the ProcedureDivision.
        /// </summary>
        /// <remarks>As a convenience, it is also an IEnumerable of Sentence.
        /// A sentence would only return itself and a Procedure would return its own sentences</remarks>
        private abstract class ProcedureDivisionRegion : IEnumerable<Sentence>
        {
            /// <summary>
            /// Order number of appearance in the ProcedureDivision of a program or function.
            /// </summary>
            public int Number { get; }

            protected ProcedureDivisionRegion(int number)
            {
                Number = number;
            }

            public abstract IEnumerator<Sentence> GetEnumerator();

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            /// <summary>
            /// Custom sentence enumeration mechanism used in PERFORM THRU resolution.
            /// This method accumulates sentences from this region into the given list
            /// up to a given end procedure.
            /// </summary>
            /// <param name="sentences">Accumulator list of sentences.</param>
            /// <param name="end">End procedure, sentences from end are included but iteration won't go further.</param>
            /// <param name="last">Last included procedure. For sentence it is null, for paragraph it is the paragraph
            /// itself and for sections, it is either :
            /// - the last paragraph of the section if the iteration went over the whole section
            /// - the end procedure if the iteration did not go over the whole section
            /// - the section itself if it is empty or made only of sentences.</param>
            public abstract void AccumulateSentencesThrough(List<Sentence> sentences, Procedure end, out Procedure last);
        }
    }
}
