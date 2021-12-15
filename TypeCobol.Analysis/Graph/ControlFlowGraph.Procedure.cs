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
        }
    }
}
