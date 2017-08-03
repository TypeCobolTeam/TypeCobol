using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// View of a source document as a complete Cobol Program or Class after parsing
    /// </summary>
    public class ProgramClassDocument
    {
        public ProgramClassDocument(CodeElementsDocument previousStepSnapshot, int programClassVersion, SourceFile root, IList<ParserDiagnostic> diagnostics, Dictionary<CodeElement, Node> nodeCodeElementLinkers)
        {
            TextSourceInfo = previousStepSnapshot.TextSourceInfo;
            PreviousStepSnapshot = previousStepSnapshot;
            CurrentVersion = programClassVersion;
            Root = root;
            Diagnostics = diagnostics;
            NodeCodeElementLinkers = nodeCodeElementLinkers;
        }

        /// <summary>
        /// Informations on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// Snapshot of the code elements document which was used to compute the current step
        /// </summary>
        public IDocumentSnapshot<ICodeElementsLine> PreviousStepSnapshot { get; private set; }

        /// <summary>
        /// Numeric version identifier for the current document
        /// </summary>
        public int CurrentVersion { get; private set; }

        public SourceFile Root { get; private set; }

        public Dictionary<CodeElement, Node> NodeCodeElementLinkers { get; private set; }

        /// <summary>
        /// Errors found while parsing Program or Class
        /// </summary>
        public IList<ParserDiagnostic> Diagnostics { get; private set; }
    }
}

 
