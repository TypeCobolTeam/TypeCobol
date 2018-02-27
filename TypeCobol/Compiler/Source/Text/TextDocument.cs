using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Source.Text
{
    /// <summary>
    /// A Source document à la TypeCobol similar to ReadOnlyTextDocument, that is not Readonly
    /// </summary>
    public class TextDocument : SourceDocument, ITextDocument
    {
        /// <summary>
        /// Initialize a cobol document from any source of characters
        /// </summary> 
        /// <param name="fileName">Name of the file the document is stored in</param>
        /// <param name="textSource">Sequence of unicode characters with line delimiters (Cr? Lf)</param>
        public TextDocument(string fileName, Encoding encodingForAlphanumericLiterals, ColumnsLayout columnsLayout,
            IEnumerable<char> textSource)
        {
            // Document source name and text format
            Source = new TextSourceInfo(fileName, encodingForAlphanumericLiterals, columnsLayout);

            // Initialize document text lines
            LoadChars(textSource);
        }

        /// <summary>
        /// Override method to Create TextLine instance.
        /// </summary>
        /// <param name="from">The start position</param>
        /// <param name="to">The end position</param>
        protected override SourceLine CreateSourceLine(Position from, Position to)
        {
            return new TextLine(new Position(0, 0), new Position(0, 0), this);
        }

        public static string ToString(IEnumerable<char> textSource)
        {
            StringBuilder sb = new StringBuilder();
            foreach (char chr in textSource)
                sb.Append(chr);
            return sb.ToString();
        }

        /// <summary>
        /// Document source name and text format
        /// </summary>
        public new TextSourceInfo Source { get; private set; }

        /// <summary>
        /// Reloads the text document with new chars.
        /// The text source must be normalized as a sequence of Unicode chars with \r and/or \n end of line chars.
        /// </summary>
        public void LoadChars(IEnumerable<char> textSource)
        {
            string text = ToString(textSource);
            LoadSourceText(text);
            // Send a notification of the change if enabled
            if (sendNextChangeEvents)
            {
                // Send document cleared event
                TextChangedEvent documentClearedEvent = new TextChangedEvent();
                documentClearedEvent.TextChanges.Add(new TextChange(TextChangeType.DocumentCleared, 0, null));

                EventHandler<TextChangedEvent> textChangedEvent = TextChanged;
                if (textChangedEvent != null)
                {
                    textChangedEvent(this, documentClearedEvent);
                }

                // Send all new text lines
                SendDocumentChangeEvent();
            }
        }

        /// <summary>
        /// Iterator over the document chars
        /// (without line delimiters)
        /// </summary>
        public IEnumerable<char> Chars
        {
            get
            {
                List<char> chars = new List<char>();
                for (int i = 0; i < LineCount; i++)
                {
                    SourceLine line = this[i];
                    string text = line.ToString();
                    foreach (char c in text)
                    {
                        if (c == '\r' || c== '\n')
                            break;
                        chars.Add(c);
                    }
                }
                return chars;
            }
        }
        public char CharAt(int offset)
        {
            if (offset < 0 || offset >= Length)
            {
                throw new InvalidOperationException("offset must be a number between 0 and " + Length);
            }
            return base.Source[offset];
        }

        /// <summary>
        /// Part of the text between start offset and end offset (included), 
        /// (first char at index 0 at the beginning of document)
        /// </summary>

        public string TextSegment(int startOffset, int endOffset)
        {

            if (startOffset < 0 || startOffset >= Length)
            {
                throw new InvalidOperationException("startOffset must be a number between 0 and " + Length);
            }
            if (endOffset < 0 || endOffset >= Length)
            {
                throw new InvalidOperationException("endOffset must be a number between 0 and " + Length);
            }
            string text = base.Source.GetTextAt(startOffset, endOffset);
            return text;
        }

        /// <summary>
        /// Iterator over the document lines
        /// </summary>
        public IEnumerable<ITextLine> Lines
        {
            get
            {
                List<ITextLine> lines = new List<ITextLine>();
                for (int i = 0; i < base.LineCount; i++)
                {
                    lines.Add(this[i] as ITextLine);
                }
                return lines;
            }
        }


        /// <summary>
        /// Gets the document line with the specified index
        /// (first line at index 0)
        /// </summary>
        public ITextLine GetLineByIndex(int lineIndex)
        {
            if (lineIndex < 0 || lineIndex >= base.LineCount)
            {
                throw new InvalidOperationException("lineIndex must be a number between 0 and " + base.LineCount);
            }

            return this[lineIndex] as ITextLine;
        }

        /// <summary>
        /// Gets the line containing the character at a specific offset in the document
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public ITextLine GetLineByOffset(int offset, out int indexOfCharInLine)
        {
            if (offset < 0 || offset >= Length)
            {
                throw new InvalidOperationException("offset must be a number between 0 and " + Length);
            }
            int lineIndex = base.GetLineIndex(offset);
            SourceLine line = this[lineIndex];
            indexOfCharInLine = offset - line.From;
            return line as ITextLine;
            ;
        }

        /// <summary>
        /// The first line has the index 0
        /// </summary>
        public int FindIndexOfLine(ITextLine line)
        {
            SourceLine sourceLine = line as SourceLine;
            if (sourceLine != null)
            {
                int from = sourceLine.From;
                int to = sourceLine.To;
                if (from >= 0 && from < Length && to >= 0 && to < Length)
                {
                    int lineIndex = base.GetLineIndex(from);                    
                    SourceLine target = this[lineIndex];
                    if (target == sourceLine)
                        return lineIndex;
                }
            }
            return -1;
        }

        /// <summary>
        /// Offset of the first char of this line in the document 
        /// </summary>
        public int FindStartOffsetOfLine(ITextLine line)
        {
            SourceLine sourceLine = line as SourceLine;
            if (sourceLine != null)
            {
                return sourceLine.From;
            }
            return -1;
        }

        // Tracks the current state
        private bool sendNextChangeEvents = false;

        /// <summary>
        /// A TextChangedEvent is sent to all observers each time a line 
        /// is inserted, updated, or removed in the document
        /// </summary>
        public event EventHandler<TextChangedEvent> TextChanged;

        /// <summary>
        /// Call this method only after all observers have been chained to form the compilation pipeline
        /// </summary>
        public void StartSendingChangeEvents()
        {
            // Send an initial TextChangedEvent grouping all line insertions in one batch
            SendDocumentChangeEvent();

            // Enable further notifications
            sendNextChangeEvents = true;
        }

        /// <summary>
        /// Send a change notification including all the text lines
        /// </summary>
        private void SendDocumentChangeEvent()
        {
            TextChangedEvent textLoadedEvent = new TextChangedEvent();
            for (int i = 0; i < base.LineCount; i++)
            {
                TextChange textChange = new TextChange(TextChangeType.LineInserted, i, this[i] as ITextLine);
                textLoadedEvent.TextChanges.Add(textChange);
            }

            EventHandler<TextChangedEvent> textChangedEvent = TextChanged;
            if (textChangedEvent != null)
            {
                textChangedEvent(this, textLoadedEvent);
            }
        }

        /// <summary>
        /// Dump the source to the Debug output Stream.
        /// </summary>
        public void DebugDumpSource()
        {
            string text = base.Source.GetTextAt(0, Length);
            System.Diagnostics.Debug.Write(text);
        }
    }
}