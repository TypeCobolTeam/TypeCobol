using System;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// View of a source document as a set of CodeElements after parsing
    /// </summary>
    public class CodeElementsDocument : ICompilerStepDocumentSnapshot<IProcessedTokensLine,ICodeElementsLine>
    {
        public CodeElementsDocument(ProcessedTokensDocument previousStepSnapshot, DocumentVersion<ICodeElementsLine> codeElementsLinesVersion, ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines)
        {
            TextSourceInfo = previousStepSnapshot.TextSourceInfo;
            PreviousStepSnapshot = previousStepSnapshot;
            CurrentVersion = codeElementsLinesVersion;
            Lines = codeElementsLines;
        }

        /// <summary>
        /// Informations on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// Snapshot of the tokens document which was used to compute the current step
        /// </summary>
        public IDocumentSnapshot<IProcessedTokensLine> PreviousStepSnapshot { get; private set; }

        /// <summary>
        /// Document version identifier for the snapshot of the previous document
        /// </summary>
        public DocumentVersion<ICodeElementsLine> CurrentVersion { get; private set; }

        /// <summary>
        /// Lines of the source text file viewed as lists of tokens and error messages
        /// </summary>
        public ISearchableReadOnlyList<ICodeElementsLine> Lines { get; private set; }
    }
}

 
