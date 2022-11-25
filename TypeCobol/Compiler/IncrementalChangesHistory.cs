using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Helper class to collect all incremental changes happening on a given CompilationUnit.
    /// </summary>
    public class IncrementalChangesHistory
    {
        private readonly List<DocumentChangedEvent<ICobolTextLine>> _textChangedEvents;
        private readonly List<DocumentChangedEvent<ITokensLine>> _tokensChangedEvents;
        private readonly List<DocumentChangedEvent<IProcessedTokensLine>> _processedTokensChangedEvents;
        private readonly List<DocumentChangedEvent<ICodeElementsLine>> _codeElementsChangedEvents;

        public IncrementalChangesHistory()
        {
            _textChangedEvents = new List<DocumentChangedEvent<ICobolTextLine>>();
            _tokensChangedEvents = new List<DocumentChangedEvent<ITokensLine>>();
            _processedTokensChangedEvents = new List<DocumentChangedEvent<IProcessedTokensLine>>();
            _codeElementsChangedEvents = new List<DocumentChangedEvent<ICodeElementsLine>>();
        }

        public IEnumerable<DocumentChangedEvent<ICobolTextLine>> TextChangedEvents => _textChangedEvents;
        public IEnumerable<DocumentChangedEvent<ITokensLine>> TokensChangedEvents => _tokensChangedEvents;
        public IEnumerable<DocumentChangedEvent<IProcessedTokensLine>> ProcessedTokensChangedEvents => _processedTokensChangedEvents;
        public IEnumerable<DocumentChangedEvent<ICodeElementsLine>> CodeElementsChangedEvents => _codeElementsChangedEvents;

        public void AddEvent(DocumentChangedEvent<ICobolTextLine> textChangedEvent) => _textChangedEvents.Add(textChangedEvent);
        public void AddEvent(DocumentChangedEvent<ITokensLine> tokensChangedEvent) => _tokensChangedEvents.Add(tokensChangedEvent);
        public void AddEvent(DocumentChangedEvent<IProcessedTokensLine> processedTokensChangedEvent) => _processedTokensChangedEvents.Add(processedTokensChangedEvent);
        public void AddEvent(DocumentChangedEvent<ICodeElementsLine> codeElementsChangedEvent) => _codeElementsChangedEvents.Add(codeElementsChangedEvent);
    }

    public static class CompilationUnitExtension
    {
        /// <summary>
        /// Create a new instance of incremental changes history and wire CompilationUnit events to it.
        /// </summary>
        /// <param name="compilationUnit">Non-null CompilationUnit to monitor.</param>
        /// <returns>New instance of IncrementalChangesHistory.</returns>
        public static IncrementalChangesHistory TrackChanges([NotNull] this CompilationUnit compilationUnit)
        {
            var history = new IncrementalChangesHistory();
            compilationUnit.TextLinesChanged += (sender, e) => history.AddEvent(e);
            compilationUnit.TokensLinesChanged += (sender, e) => history.AddEvent(e);
            compilationUnit.ProcessedTokensLinesChangedEventsSource += (sender, e) => history.AddEvent(e);
            compilationUnit.CodeElementsLinesChanged += (sender, e) => history.AddEvent(e);
            return history;
        }
    }
}
