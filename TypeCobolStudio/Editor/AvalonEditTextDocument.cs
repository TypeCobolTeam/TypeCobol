using ICSharpCode.AvalonEdit.Document;
using System;
using System.Collections.Generic;
using System.Reactive.Subjects;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobolStudio.Editor
{
    /// <summary>
    /// Adapter used to implement the TypeCobol.Compiler.Text.ITextDocument interface 
    /// on top of an AvalonEdit.TextDocument instance.
    /// </summary>
    internal class AvalonEditTextDocument : ITextDocument, ILineTracker, IDisposable
    {
        private ICSharpCode.AvalonEdit.Document.TextDocument _avalonEditTextDocument;
        private WeakLineTracker _weakLineTracker;

        public AvalonEditTextDocument(ICSharpCode.AvalonEdit.Document.TextDocument avalonEditTextDocument, Encoding encodingForAlphanumericLiterals, ColumnsLayout columnsLayout)
        {
            // Document source name and text format
            Source = new TextSourceInfo(_avalonEditTextDocument.FileName, encodingForAlphanumericLiterals, columnsLayout);

            _avalonEditTextDocument = avalonEditTextDocument;
            // Listen to all line changes in the editor
            _avalonEditTextDocument.VerifyAccess();
            _weakLineTracker = WeakLineTracker.Register(_avalonEditTextDocument, this);
        }

        /// <summary>
        /// Reloads the text document with new chars
        /// </summary>
        public void LoadChars(IEnumerable<char> textSource)
        {
            // TO DO : find a more efficient implementation if necessary
            StringBuilder sb = new StringBuilder();
            foreach (char chr in textSource)
            {
                sb.Append(chr);
            }
            _avalonEditTextDocument.Text = sb.ToString();
        }

        /// <summary>
        /// Document source name and text format
        /// </summary>
        public TextSourceInfo Source { get; private set; }

        /// <summary>
        /// Iterator over the document chars
        /// (without line delimiters)
        /// </summary>
        public IEnumerable<char> Chars
        {
            get
            {
                string documentText = _avalonEditTextDocument.Text;
                return documentText;
            }
        }

        /// <summary>
        /// Access the document like a char array
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public char CharAt(int offset)
        {
            return _avalonEditTextDocument.GetCharAt(offset);
        }

        /// <summary>
        /// Part of the text between start offset and end offset (included), 
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public string TextSegment(int startOffset, int endOffset)
        {
            return _avalonEditTextDocument.GetText(startOffset, endOffset - startOffset + 1);
        }

        /// <summary>
        /// Total number of chars in the document, excluding line delimiters
        /// </summary>
        public int Length
        {
            get
            {
                return _avalonEditTextDocument.TextLength;
            }
        }

        /// <summary>
        /// Iterator over the document lines
        /// </summary>
        public IEnumerable<ITextLine> Lines
        {
            get
            {
                foreach (IDocumentLine documentLine in _avalonEditTextDocument.Lines)
                {
                    yield return BuildTextLineFromDocumentLine(documentLine);
                }
            }
        }

        private TextLineSnapshot BuildTextLineFromDocumentLine(IDocumentLine documentLine)
        {
            return new TextLineSnapshot(documentLine.LineNumber - 1, _avalonEditTextDocument.GetText(documentLine.Offset, documentLine.Length), documentLine);
        }

        private TextLineSnapshot BuildTextLineFromDocumentLine(IDocumentLine documentLine, int newLength)
        {
            return new TextLineSnapshot(documentLine.LineNumber - 1, _avalonEditTextDocument.GetText(documentLine.Offset, newLength), documentLine);
        }

        /// <summary>
        /// Gets the document line with the specified index
        /// (first line at index 0)
        /// </summary>
        public ITextLine GetLineByIndex(int lineIndex)
        {
            IDocumentLine documentLine = _avalonEditTextDocument.GetLineByNumber(lineIndex + 1);
            return BuildTextLineFromDocumentLine(documentLine);
        }

        /// <summary>
        /// Gets the line containing the character at a specific offset in the document
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public ITextLine GetLineByOffset(int offset, out int indexOfCharInLine)
        {
            IDocumentLine documentLine = _avalonEditTextDocument.GetLineByOffset(offset);
            indexOfCharInLine = offset - documentLine.Offset;
            return BuildTextLineFromDocumentLine(documentLine);
        }

        /// <summary>
        /// Total number of lines in the document
        /// </summary>
        public int LineCount
        {
            get
            {
                return _avalonEditTextDocument.LineCount;
            }
        }

        /// <summary>
        /// The first line has the index 0
        /// </summary>
        public int FindIndexOfLine(ITextLine line)
        {
            IDocumentLine docLine = line.LineTrackingReferenceInSourceDocument as IDocumentLine;
            if (docLine != null)
            {
                return docLine.LineNumber;
            }
            else
            {
                return -1;
            }
        }

        /// <summary>
        /// Offset of the first char of this line in the document 
        /// </summary>
        public int FindStartOffsetOfLine(ITextLine line)
        {
            IDocumentLine docLine = line.LineTrackingReferenceInSourceDocument as IDocumentLine;
            if (docLine != null)
            {
                return docLine.Offset;
            }
            else
            {
                return -1;
            }
        }

        // Tracks the current state
        private bool sendNextChangeEvents = false;

        /// <summary>
        /// Call this method only after all observers have been chained to form the compilation pipeline
        /// </summary>
        public void StartSendingChangeEvents()
        {
            // Enable further notifications
            sendNextChangeEvents = true;

            // Send an initial TextChangedEvent grouping all line insertions in one batch
            SendSocumentChangeEvent();
        }

        /// <summary>
        /// Send a change notification including all the text lines
        /// </summary>
        private void SendSocumentChangeEvent()
        {
            TextChangedEvent initialEvent = new TextChangedEvent();
            int lineIndex = 0;
            foreach (ITextLine textLine in Lines)
            {
                TextChange lineAdded = new TextChange(TextChangeType.LineInserted, lineIndex, textLine);
                initialEvent.TextChanges.Add(lineAdded);
                lineIndex++;
            }
            RaiseTextChanged(initialEvent);
        }

        public event EventHandler<TextChangedEvent> TextChanged;

        private void RaiseTextChanged(TextChangedEvent textEvent)
        {
            EventHandler<TextChangedEvent> textChanged = TextChanged;
            if (textChanged != null)
            {
                textChanged(this, textEvent);
            }
        }

        public void Dispose()
        {
            if (_weakLineTracker != null)
                _weakLineTracker.Deregister();
        }

        // --- ILineTracker interface ---

        TextChangedEvent textChangedEvent = new TextChangedEvent();

        void ILineTracker.BeforeRemoveLine(DocumentLine line)
        {
            if (sendNextChangeEvents)
            {
                int lineIndex = line.LineNumber - 1;
                textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineRemoved, lineIndex, null));
            }
        }

        void ILineTracker.SetLineLength(DocumentLine line, int newTotalLength)
        {
            if (sendNextChangeEvents)
            {
                int lineIndex = line.LineNumber - 1;
                int delimiterLength = Math.Min(line.DelimiterLength, newTotalLength);
                ITextLine newTextLine = BuildTextLineFromDocumentLine(line, newTotalLength - delimiterLength);
                // Special case when you press enter at the end of the last line of the document
                if (delimiterLength == 0 && newTotalLength >= 2)
                {
                    if (newTextLine.Text.EndsWith("\r\n"))
                    {
                        newTextLine = BuildTextLineFromDocumentLine(line, newTotalLength - 2);
                    }
                }
                textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineUpdated, lineIndex, newTextLine));
            }
        }

        void ILineTracker.LineInserted(DocumentLine insertionPos, DocumentLine newLine)
        {
            if (sendNextChangeEvents)
            {
                int lineIndex = newLine.LineNumber - 1;
                ITextLine newTextLine = BuildTextLineFromDocumentLine(newLine);
                // Special case when you press enter at the end of a line in the middle of the document
                if (newLine.DelimiterLength == 0 && newLine.TotalLength >= 2)
                {
                    if (newTextLine.Text.EndsWith("\r\n"))
                    {
                        newTextLine = BuildTextLineFromDocumentLine(newLine, newLine.TotalLength - 2);
                    }
                }
                textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineInserted, lineIndex, newTextLine));
            }
        }

        void ILineTracker.ChangeComplete(DocumentChangeEventArgs e)
        {
            if (sendNextChangeEvents)
            {
                RaiseTextChanged(textChangedEvent);
                textChangedEvent = new TextChangedEvent();
            }
        }

        void ILineTracker.RebuildDocument()
        {
            // Event activated
            // - at the first char input in a brand new document
            // - when you delete all chars of an existing document
            // - when a replace operation starts at offset 0

            // => in this case, the text changes but the line trackers are not notified !

            if (sendNextChangeEvents)
            {
                textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.DocumentCleared, 0, null));
                int lineIndex = 0;
                foreach (ITextLine line in Lines)
                {
                    textChangedEvent.TextChanges.Add(new TextChange(TextChangeType.LineInserted, lineIndex, line));
                    lineIndex++;
                }
            }
        }
    }
}
