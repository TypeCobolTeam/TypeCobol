using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Analysis;
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
            ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines, SourceFile root, List<Node> nodes,
            [NotNull] List<Diagnostic> diagnostics, 
            Dictionary<CodeElement, Node> nodeCodeElementLinkers, 
            List<DataDefinition> typedVariablesOutsideTypedef, 
            List<TypeDefinition> typeThatNeedTypeLinking,
            [NotNull] Dictionary<string, object> analyzerResults)
        {
            PreviousStepSnapshot = previousSnapShot;
            Root = root;
            Nodes = nodes;
            Diagnostics = diagnostics;
            NodeCodeElementLinkers = nodeCodeElementLinkers;
            TextSourceInfo = previousSnapShot.TextSourceInfo;
            CurrentVersion = codeElementsLinesVersion;
            Lines = codeElementsLines;
            TypedVariablesOutsideTypedef = typedVariablesOutsideTypedef;
            TypeThatNeedTypeLinking = typeThatNeedTypeLinking;
            AnalyzerResults = new AnalyzerResults(analyzerResults);
        }

        public TextSourceInfo TextSourceInfo { get; set; }
        public SourceFile Root { get; private set; }
        public List<Node> Nodes { get; }
        public Dictionary<CodeElement, Node> NodeCodeElementLinkers { get; private set; }

        [NotNull]
        public AnalyzerResults AnalyzerResults { get; }

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
