using System;
using System.Collections;
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
        private class EventStore<TLine> : IEnumerable<DocumentChangedEvent<TLine>>
            where TLine : ICobolTextLine
        {
            private readonly int _maxCount;
            private readonly Queue<DocumentChangedEvent<TLine>> _queue;
            private readonly Action<DocumentChangedEvent<TLine>> _add;

            public EventStore(int maxCount)
            {
                _maxCount = maxCount;
                if (_maxCount > 0)
                {
                    // Max depth enabled
                    _queue = new Queue<DocumentChangedEvent<TLine>>(_maxCount);
                    _add = CheckCountAndAdd;
                }
                else
                {
                    // No limit !
                    _queue = new Queue<DocumentChangedEvent<TLine>>();
                    _add = AddWithoutCheckingCount;
                }
            }

            private void CheckCountAndAdd(DocumentChangedEvent<TLine> documentChangedEvent)
            {
                if (_queue.Count == _maxCount)
                {
                    _queue.Dequeue();
                }

                _queue.Enqueue(documentChangedEvent);
            }

            private void AddWithoutCheckingCount(DocumentChangedEvent<TLine> documentChangedEvent) => _queue.Enqueue(documentChangedEvent);

            public void Add(DocumentChangedEvent<TLine> documentChangedEvent) => _add(documentChangedEvent);

            public IEnumerator<DocumentChangedEvent<TLine>> GetEnumerator() => _queue.GetEnumerator();

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }

        private readonly EventStore<ICobolTextLine> _textChangedEvents;
        private readonly EventStore<ITokensLine> _tokensChangedEvents;
        private readonly EventStore<IProcessedTokensLine> _processedTokensChangedEvents;
        private readonly EventStore<ICodeElementsLine> _codeElementsChangedEvents;

        public IncrementalChangesHistory(int depth)
        {
            _textChangedEvents = new EventStore<ICobolTextLine>(depth);
            _tokensChangedEvents = new EventStore<ITokensLine>(depth);
            _processedTokensChangedEvents = new EventStore<IProcessedTokensLine>(depth);
            _codeElementsChangedEvents = new EventStore<ICodeElementsLine>(depth);
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
        /// <param name="depth">Max allowed depth of history. Default is 0 meaning no max depth, a negative value is also interpreted as no max depth.</param>
        /// <returns>New instance of IncrementalChangesHistory.</returns>
        public static IncrementalChangesHistory TrackChanges([NotNull] this CompilationUnit compilationUnit, int depth = 0)
        {
            var history = new IncrementalChangesHistory(depth);
            compilationUnit.TextLinesChanged += (sender, e) => history.AddEvent(e);
            compilationUnit.TokensLinesChanged += (sender, e) => history.AddEvent(e);
            compilationUnit.ProcessedTokensLinesChangedEventsSource += (sender, e) => history.AddEvent(e);
            compilationUnit.CodeElementsLinesChanged += (sender, e) => history.AddEvent(e);
            return history;
        }
    }
}
