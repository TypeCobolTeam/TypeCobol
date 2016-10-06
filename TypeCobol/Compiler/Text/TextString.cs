using System.Collections.Generic;
using System.Text;

namespace TypeCobol.Compiler.Text
{
    public class TextString : ReadOnlyTextDocument
    {
        public string Text { get; private set; }

        public TextString(string text) : base(null, Encoding.UTF8, ColumnsLayout.FreeTextFormat, text)
        {
            Text = text;
        }

        public IEnumerable<char> Chars {
            get { 
                return Text;
            }
        }

        public int Length {
            get {
                return Text.Length;
            }
        }

        public char CharAt(int offset) {
            return Text[offset];
        }

        public TextSourceInfo Source { get; private set; }

        public int LineCount {
            get {
                int lines = 0;
                foreach (char c in Text)
                    if (c == '\n') lines++;
                return lines;
            }
        }

        public string TextSegment(int startOffset, int endOffset)
        {
            return Text.Substring(startOffset, endOffset - startOffset);
        }

        public ITextLine GetLineByIndex(int lineIndex)
        {
            ITextLine[] lines = Lines as ITextLine[];
            return lines[lineIndex];
        }
        /*
        public ITextLine GetLineByOffset(int offset, out int indexOfCharInLine)
        {
            int l = 0;
            int os = 0;
            int last = 0;
            foreach (char c in Text)
            {
                if (os == offset)
                {
                    indexOfCharInLine = offset - last;
                    return GetLineByIndex(l);
                }
                if (c == '\n')
                {
                    l++;
                    last = os + 1;
                }
                os++;
            }
            throw new System.IndexOutOfRangeException(offset+" should be < "+Text.Length);
        }
        */
        // IEnumerable<ITextLine> Lines in base class
        // void LoadChars(IEnumerable<char> textSource) in base class
        // int FindIndexOfLine(ITextLine line) in base class
        // int FindStartOffsetOfLine(ITextLine line) in base class
        // System.IObservable<TextChangedEvent> TextChangedEventsSource in base class
        // void StartSendingChangeEvents() in base class
    }
}
