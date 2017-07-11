using System;
using System.Collections.Generic;
using System.Text;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Read-only text document for batch compilation.
    /// Document loaded once from a file and never modified.
    /// </summary>
    public class ReadOnlyTextDocument : ITextDocument
    {
        // This document simply maintains a list of read-only text lines
        private IList<ReadOnlyTextLine> lines = new List<ReadOnlyTextLine>();

        // Total number of chars (without line delimiters)
        private int charsCount;

        /// <summary>
        /// Initialize a cobol document from any source of characters
        /// </summary> 
        /// <param name="fileName">Name of the file the document is stored in</param>
        /// <param name="textSource">Sequence of unicode characters with line delimiters (Cr? Lf)</param>
        public ReadOnlyTextDocument(string fileName, Encoding encodingForAlphanumericLiterals, ColumnsLayout columnsLayout, IEnumerable<char> textSource)
        {
            // Document source name and text format
            Source = new TextSourceInfo(fileName, encodingForAlphanumericLiterals, columnsLayout);

            // Initialize document text lines
            LoadChars(textSource);
        }

        /// <summary>
        /// Reloads the text document with new chars.
        /// The text source must be normalized as a sequence of Unicode chars with \r and/or \n end of line chars.
        /// </summary>
        public void LoadChars(IEnumerable<char> textSource)
        {
            // Reset document contents
            lines.Clear();

            // Build TextLines from chars enumerator
            charsCount = 0;
            int lineIndex = 0;
            StringBuilder currentLineText = new StringBuilder();
            bool previousCharWasCr = false;
            foreach (char chr in textSource)
            {
                if (chr == '\r')
                {
                    // If an end of line char is encountered, create a new line
                    ReadOnlyTextLine line = new ReadOnlyTextLine(lineIndex, charsCount, currentLineText.ToString(), null);
                    lines.Add(line);
                    lineIndex++;
                    charsCount += line.Length + 1; //+1 to add the \r char

                    // Reset StringBuilder contents for next line
                    currentLineText = new StringBuilder();

                    previousCharWasCr = true;
                }
                else if (chr == '\n')
                {
                    if (!previousCharWasCr)
                    {
                        // If an end of line char is encountered, create a new line
                        ReadOnlyTextLine line = new ReadOnlyTextLine(lineIndex, charsCount, currentLineText.ToString(), null);
                        lines.Add(line);
                        lineIndex++;
                        charsCount += line.Length + 1; //+1 to add the \n char

                        // Reset StringBuilder contents for next line
                        currentLineText = new StringBuilder();
                    }

                    charsCount++;
                    previousCharWasCr = false;
                }
                else
                {
                    // Append the current char to the text of the current line
                    currentLineText.Append(chr);

                    previousCharWasCr = false;
                }
            }
            // If the last line was not terminated with end of line chars
            if (currentLineText.Length > 0)
            {
                ReadOnlyTextLine line = new ReadOnlyTextLine(lineIndex, charsCount, currentLineText.ToString(), null);
                lines.Add(line);
                charsCount += line.Length;
            }

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
                IList<char> chars = new List<char>();
                foreach (ITextLine line in lines)
                {
                    foreach (char c in line.Text)
                    {
                        chars.Add(c);
                    }
                }
                return chars;
            }
        }

        /// <summary>
        /// Access the document like a char array
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public char CharAt(int offset)
        {
            if (offset < 0 || offset >= charsCount)
            {
                throw new InvalidOperationException("offset must be a number between 0 and " + charsCount);
            }

            int indexOfCharInLine;
            ITextLine line = GetLineByOffset(offset, out indexOfCharInLine);
            return line.Text[indexOfCharInLine];
        }

        /// <summary>
        /// Part of the text between start offset and end offset (included), 
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public string TextSegment(int startOffset, int endOffset)
        {
            if (startOffset < 0 || startOffset >= charsCount)
            {
                throw new InvalidOperationException("startOffset must be a number between 0 and " + charsCount);
            }
            if (endOffset < 0 || endOffset >= charsCount)
            {
                throw new InvalidOperationException("endOffset must be a number between 0 and " + charsCount);
            }

            int indexOfCharInLineStart;
            ReadOnlyTextLine lineStart = (ReadOnlyTextLine)GetLineByOffset(startOffset, out indexOfCharInLineStart);
            if (endOffset - startOffset + indexOfCharInLineStart < lineStart.Length)
            {
                return lineStart.TextSegment(indexOfCharInLineStart, indexOfCharInLineStart + endOffset - startOffset);
            }
            else
            {
                int indexOfCharInLineEnd;
                ReadOnlyTextLine lineEnd = (ReadOnlyTextLine)GetLineByOffset(endOffset, out indexOfCharInLineEnd);

                StringBuilder sbSegment = new StringBuilder();
                // First line
                sbSegment.Append(lineStart.TextSegment(indexOfCharInLineStart, lineStart.Length - 1));
                // Intermediate lines
                for (int index = lineStart.LineIndex + 1; index < lineEnd.LineIndex; index++)
                {
                    sbSegment.Append(lines[index].Text);
                }
                // Last line
                sbSegment.Append(lineEnd.TextSegment(0, indexOfCharInLineEnd));
                return sbSegment.ToString();
            }
        }

        /// <summary>
        /// Total number of chars in the document, excluding line delimiters
        /// </summary>
        public int Length
        {
            get
            {
                return charsCount;
            }
        }

        /// <summary>
        /// Iterator over the document lines
        /// </summary>
        public IEnumerable<ITextLine> Lines
        {
            get
            {
                return lines;
            }
        }

        /// <summary>
        /// Gets the document line with the specified index
        /// (first line at index 0)
        /// </summary>
        public ITextLine GetLineByIndex(int lineIndex)
        {
            if (lineIndex < 0 || lineIndex >= lines.Count)
            {
                throw new InvalidOperationException("lineIndex must be a number between 0 and " + lines.Count);
            }

            return lines[lineIndex];
        }

        /// <summary>
        /// Gets the line containing the character at a specific offset in the document
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        public ITextLine GetLineByOffset(int offset, out int indexOfCharInLine)
        {
            if (offset < 0 || offset >= charsCount)
            {
                throw new InvalidOperationException("offset must be a number between 0 and " + charsCount);
            }

            // Initialize binary search bounds
            int lineIndexStart = 0;
            int lineIndexEnd = lines.Count - 1;

            // Optimization : fast path for fixed length lines of 80 characters
            int lineIndexMid = offset / 80;
            if (lineIndexMid >= lines.Count)
            {
                lineIndexMid = lineIndexStart + (lineIndexEnd - lineIndexStart) / 2;
            }

            // Execute binary search
            ReadOnlyTextLine line = null;
            while (lineIndexEnd > lineIndexStart)
            {
                line = lines[lineIndexMid];
                if (offset >= line.StartOffset)
                {
                    if (offset < (line.StartOffset + line.Length))
                    {
                        indexOfCharInLine = offset - line.StartOffset;
                        return line;
                    }
                    else
                    {
                        lineIndexStart = lineIndexMid + 1;
                    }
                }
                else if (offset < line.StartOffset)
                {
                    lineIndexEnd = lineIndexMid - 1;
                }
                lineIndexMid = lineIndexStart + (lineIndexEnd - lineIndexStart) / 2;
            }

            // Line found
            line = lines[lineIndexStart];
            indexOfCharInLine = offset - line.StartOffset;
            return line;
        }

        /// <summary>
        /// Total number of lines in the document
        /// </summary>
        public int LineCount
        {
            get
            {
                return lines.Count;
            }
        }

        /// <summary>
        /// The first line has the index 0
        /// </summary>
        public int FindIndexOfLine(ITextLine line)
        {
            ReadOnlyTextLine roTextLine = line as ReadOnlyTextLine;
            if (roTextLine != null)
            {
                return roTextLine.LineIndex;
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
            ReadOnlyTextLine roTextLine = line as ReadOnlyTextLine;
            if (roTextLine != null)
            {
                return roTextLine.StartOffset;
            }
            else
            {
                return -1;
            }
        }

        /// <summary>
        /// A TextChangedEvent is sent to all observers each time a line 
        /// is inserted, updated, or removed in the document
        /// </summary>
        public event EventHandler<TextChangedEvent> TextChanged;

        // Tracks the current state
        private bool sendNextChangeEvents = false;

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
            foreach (var line in lines)
            {
                TextChange textChange = new TextChange(TextChangeType.LineInserted, line.LineIndex, line);
                textLoadedEvent.TextChanges.Add(textChange);
            }

            EventHandler<TextChangedEvent> textChangedEvent = TextChanged;
            if (textChangedEvent != null)
            {
                textChangedEvent(this, textLoadedEvent);
            }
        }               
    }
}
