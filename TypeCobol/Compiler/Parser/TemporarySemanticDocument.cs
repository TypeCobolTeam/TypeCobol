using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{

    public class TemporarySemanticDocument : ICompilerStepDocumentSnapshot<ICodeElementsLine, ICodeElementsLine>
    {
        public TemporarySemanticDocument(CodeElementsDocument previousSnapShot, DocumentVersion<ICodeElementsLine> codeElementsLinesVersion, ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines, SourceFile root, List<Diagnostic> diagnostics, Dictionary<CodeElement, Node> nodeCodeElementLinkers)
        {
            PreviousStepSnapshot = previousSnapShot;
            Root = root;
            Diagnostics = diagnostics;
            NodeCodeElementLinkers = nodeCodeElementLinkers;
            TextSourceInfo = previousSnapShot.TextSourceInfo;
            CurrentVersion = codeElementsLinesVersion;
            Lines = codeElementsLines;
        }

        public TextSourceInfo TextSourceInfo { get; set; }
        public SourceFile Root { get; private set; }
        public Dictionary<CodeElement, Node> NodeCodeElementLinkers { get; private set; }
        /// <summary>
        /// Errors found while parsing Program or Class
        /// </summary>
        public List<Diagnostic> Diagnostics { get; private set; }

        //USeless in this case
        public DocumentVersion<ICodeElementsLine> CurrentVersion { get; }
        public IDocumentSnapshot<ICodeElementsLine> PreviousStepSnapshot { get; }
        public ISearchableReadOnlyList<ICodeElementsLine> Lines { get; }
    }
}
