namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Base class for Paragraphs and Sections.
        /// A procedure is a target of a GOTO or PERFORM, it has a name and holds sentences.
        /// </summary>
        private abstract class Procedure : ProcedureDivisionRegion
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
            protected Procedure(int number, string name)
                : base(number)
            {
                Name = name;
            }

            /// <summary>
            /// Add a sentence to this procedure.
            /// </summary>
            /// <param name="sentence">The sentence to be added.</param>
            public abstract void AddSentence(Sentence sentence);
        }
    }
}
