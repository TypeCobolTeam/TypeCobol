using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// View of a source document as a complete Cobol Program or Class after parsing
    /// </summary>
    public class ProgramClassDocument
    {
        public ProgramClassDocument(TemporarySemanticDocument previousStepSnapshot, int programClassVersion, SourceFile root, Dictionary<CodeElement, Node> nodeCodeElementLinkers)
        {
            TextSourceInfo = previousStepSnapshot.TextSourceInfo;
            PreviousStepSnapshot = previousStepSnapshot;
            CurrentVersion = programClassVersion;
            Root = root;
            NodeCodeElementLinkers = nodeCodeElementLinkers;
        }

        /// <summary>
        /// Information on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; }

        /// <summary>
        /// Snapshot of the code elements document which was used to compute the current step
        /// </summary>
        public TemporarySemanticDocument PreviousStepSnapshot { get; }

        /// <summary>
        /// Numeric version identifier for the current document
        /// </summary>
        public int CurrentVersion { get; }

        public SourceFile Root { get; }

        public Dictionary<CodeElement, Node> NodeCodeElementLinkers { get; }
    }
}
