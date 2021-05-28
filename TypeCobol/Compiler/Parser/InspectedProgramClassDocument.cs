using System.Collections.Generic;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Capture results of code analysis.
    /// </summary>
    public class InspectedProgramClassDocument
    {
        public InspectedProgramClassDocument(ProgramClassDocument programClassDocument, int version, List<Diagnostic> diagnostics, Dictionary<string, object> analyzerResults)
        {
            PreviousStepSnapshot = programClassDocument;
            CurrentVersion = version;
            Diagnostics = diagnostics;
            AnalyzerResults = new AnalyzerResults(analyzerResults);
        }

        /// <summary>
        /// Snapshot of the fully parsed program that was quality checked
        /// </summary>
        public ProgramClassDocument PreviousStepSnapshot { get; }

        /// <summary>
        /// Version number of the current document
        /// </summary>
        public int CurrentVersion { get; }

        /// <summary>
        /// List of rules violations returned by code analysis
        /// </summary>
        public List<Diagnostic> Diagnostics { get; }

        /// <summary>
        /// Additional results produced by analyzers
        /// </summary>
        public AnalyzerResults AnalyzerResults { get; }

        /// <summary>
        /// Checked source file, root node of the AST
        /// </summary>
        public SourceFile Root => PreviousStepSnapshot.Root;
    }
}
