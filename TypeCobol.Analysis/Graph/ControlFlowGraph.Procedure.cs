using System.Collections.Generic;

namespace TypeCobol.Analysis.Graph
{
    public partial class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// Base class for Paragraphs and Sections.
        /// A procedure is a target of a GOTO or PERFORM, it has a name and holds sentences.
        /// </summary>
        public abstract class Procedure : ProcedureDivisionRegion
        {
            /// <summary>
            /// Name of the procedure.
            /// </summary>
            public string Name { get; }

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="number">Order number of appearance.</param>
            /// <param name="name">Name of the procedure</param>
            internal Procedure(int number, string name)
                : base(number)
            {
                Name = name;
            }

            /// <summary>
            /// Add a sentence to this procedure.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            internal abstract void AddSentence(Sentence sentence);

            /// <summary>
            /// Custom sentence enumeration mechanism used in PERFORM THRU resolution.
            /// This method accumulates sentences from this procedure into the given list
            /// up to a given end procedure.
            /// </summary>
            /// <param name="sentences">Accumulator list of sentences.</param>
            /// <param name="end">End procedure, sentences from end are included but iteration won't go further.</param>
            /// <param name="last">Last included procedure. For paragraph it is the paragraph itself
            /// and for sections, it is either :
            /// - the last paragraph of the section if the iteration went over the whole section
            /// - the end procedure if the iteration did not go over the whole section
            /// - the section itself if it is empty or made only of sentences.</param>
            internal abstract void AccumulateSentencesThrough(List<Sentence> sentences, Procedure end, out Procedure last);
        }
    }
}
