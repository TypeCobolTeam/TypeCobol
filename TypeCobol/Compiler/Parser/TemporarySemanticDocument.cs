using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{

    public class TemporarySemanticDocument : ICompilerStepDocumentSnapshot<ICodeElementsLine, ICodeElementsLine>
    {
        public TemporarySemanticDocument(CodeElementsDocument previousSnapShot, DocumentVersion<ICodeElementsLine> codeElementsLinesVersion, 
            ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines, SourceFile root, [NotNull] List<Diagnostic> diagnostics, 
            Dictionary<CodeElement, Node> nodeCodeElementLinkers, 
            List<DataDefinition> typedVariablesOutsideTypedef, 
            List<TypeDefinition> typeThatNeedTypeLinking)
        {
            PreviousStepSnapshot = previousSnapShot;
            Root = root;
            Diagnostics = diagnostics;
            NodeCodeElementLinkers = nodeCodeElementLinkers;
            TextSourceInfo = previousSnapShot.TextSourceInfo;
            CurrentVersion = codeElementsLinesVersion;
            Lines = codeElementsLines;
            TypedVariablesOutsideTypedef = typedVariablesOutsideTypedef;
            TypeThatNeedTypeLinking = typeThatNeedTypeLinking;
        }

        public TextSourceInfo TextSourceInfo { get; set; }
        public SourceFile Root { get; private set; }
        public Dictionary<CodeElement, Node> NodeCodeElementLinkers { get; private set; }


        [NotNull] [ItemNotNull]
        public List<DataDefinition> TypedVariablesOutsideTypedef { get; }

        [NotNull][ItemNotNull]
        public List<TypeDefinition> TypeThatNeedTypeLinking { get; }

        /// <summary>
        /// Errors found while parsing Program or Class
        /// </summary>
        [NotNull]
        public List<Diagnostic> Diagnostics { get; private set; }

        //USeless in this case
        public DocumentVersion<ICodeElementsLine> CurrentVersion { get; }
        public IDocumentSnapshot<ICodeElementsLine> PreviousStepSnapshot { get; }
        public ISearchableReadOnlyList<ICodeElementsLine> Lines { get; }
    }
}
