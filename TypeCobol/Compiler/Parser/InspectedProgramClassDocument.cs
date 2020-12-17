using System.Collections.Generic;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Capture results of code analysis.
    /// </summary>
    public class InspectedProgramClassDocument
    {
        public InspectedProgramClassDocument(ProgramClassDocument programClassDocument, List<Diagnostic> violations)
        {
            PreviousStepSnapshot = programClassDocument;
            Diagnostics = violations;
        }

        /// <summary>
        /// Snapshot of the fully parsed program that was quality checked
        /// </summary>
        public ProgramClassDocument PreviousStepSnapshot { get; }

        /// <summary>
        /// List of rules violations returned by code analysis
        /// </summary>
        public List<Diagnostic> Diagnostics { get; }

        /// <summary>
        /// Checked source file, root node of the AST
        /// </summary>
        public SourceFile Root => PreviousStepSnapshot.Root;
    }
}
