using System.Collections.Generic;

namespace TypeCobol.Analysis.Graph
{
    public partial class ControlFlowGraph<N, D>
    {
        /// <summary>
        /// Represents the target of a jump instruction, that is either a PERFORM or a GO TO.
        /// For a PERFORM instruction:
        /// - the sentences are contiguous, it represents a single region of the Procedure Division
        /// - if a section is included in procedures, it may not be fully targeted
        ///   -> check the last procedure of the target against the last procedure of the section to see whether the section is fully covered or not
        /// For a GO TO instruction:
        /// - sentences may be non-contiguous if the GO TO has multiple targets which are non-contiguous.
        /// </summary>
        public class JumpTarget
        {
            private readonly List<Sentence> _sentences;
            private readonly List<Procedure> _procedures;

            internal JumpTarget(List<Sentence> sentences, List<Procedure> procedures)
            {
                _sentences = sentences;
                _procedures = procedures;
            }

            /// <summary>
            /// All target sentences
            /// </summary>
            public IEnumerable<Sentence> Sentences => _sentences;

            /// <summary>
            /// All target procedures
            /// </summary>
            public IEnumerable<Procedure> Procedures => _procedures;
        }
    }
}
