using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// Class that represents a source document. 
    /// </summary>
    public class SourceDocument
    {
        /// <summary>
        /// The Associated source Text.
        /// </summary>
        public SourceText Source
        {
            get;
            private set;
        }

        /// <summary>
        /// The Array of lines
        /// </summary>
        private SourceLine[] lines;
        // the lien counts
        private int nlines;
        /// <summary>
        /// The last index in the array of lines.
        /// </summary>
        private int lastIndex;

        /// <summary>
        /// Empty constructor with by default a GapSourceText.
        /// </summary>
        public SourceDocument() : this (new GapSourceText())
        {
        }

        /// <summary>
        /// Source Text constructor
        /// </summary>
        public SourceDocument(SourceText text)
        {
            Source = text;         
            lines = new SourceLine[1];
            lines[0] = new SourceLine(new Position(0, 0), new Position(0, 0));
            nlines = 0;
            lastIndex = -1;
            ///Add a listener to us.
            text.Observers += TextChangeObserver;
            ///Build all lines
            SourceText.TextChangeInfo info = new SourceText.TextChangeInfo(SourceText.TextChanges.TextReplaced, 0, 0, text.Size);
            text.Send(info, text);
        }

        /// <summary>
        /// Load a Source File
        /// </summary>
        /// <param name="filepath">The file to load</param>
        public void LoadSourceFile(String filepath)
        {
            string text = "";
            using (var streamReader = new StreamReader(filepath, Encoding.UTF8))
            {
                text = streamReader.ReadToEnd();
            }
            LoadSourceText(text);
        }

        /// <summary>
        /// Load a source text
        /// </summary>
        /// <param name="text">The text to load</param>
        public void LoadSourceText(String text)
        {
            Source.Delete(0, Source.Size);
            Source.Insert(text, 0, 0);
        }

        /// <summary>
        /// Load a source text as an array of characters
        /// </summary>
        /// <param name="text">The text</param>
        public void LoadSourceChar(char[] text)
        {
            Source.Delete(0, Source.Size);
            Source.Insert(text, 0, 0);
        }

        /// <summary>
        /// Get the start Offset of the Document in fact 0
        /// </summary>
        public int From
        {
            get
            {
                return lines[0].From;
            }
        }
    
        /// <summary>
        /// Get the end offset of the document.
        /// </summary>
        public int To
        {
            get
            {
                return lines[nlines - 1].To;
            }
        }

        /// <summary>
        /// Get the Line at the given index
        /// </summary>
        /// <param name="index">The line index</param>
        /// <returns>The SourceLine instance if any, null otherwise.</returns>
        public SourceLine this[int index]
        {
            get
            {
                if (index >= 0 && index < nlines) {
                    return lines[index];
                }
                return null;
            }
        }

        /// <summary>
        /// Gets the count of lines.
        /// </summary>
        public int LineCount  
        {
            get
            {
                return nlines;
            }
        }

        /// <summary>
        /// Get the line that contains a the given position
        /// </summary>
        /// <param name="at">The position to get the line</param>
        /// <returns>The Source Line instance if the position is valid, null otherwise</returns>
        public SourceLine GetLineAtPosition(int at)
        {
            int index = GetLineIndex(at);
            SourceLine line = lines[index];
            int from = line.From;
            int to = line.To;
            if ((at >= from) && (at < to))
            {
                return line;
            }
            return null;
        }

        /// <summary>
        /// The Text Change event Observer
        /// </summary>
        /// <param name="source"></param>
        /// <param name="args"></param>
        private void TextChangeObserver(object source, EventArgs args)
        {
            SourceText.TextChangeInfo info = (SourceText.TextChangeInfo)args;
            switch (info.Kind)
            {
                case SourceText.TextChanges.TextReplaced://Insertion
                    {                        
                        int from = info.From;
                        int length = info.Size;
                        int index = GetLineIndex(from);
                        SourceLine removeLine = this[index];
                        int removeFrom = removeLine != null ? removeLine.From : 0;
                        int removeTo = removeLine != null ? removeLine.To : 0;
                        int lastPos = removeFrom;
                        try 
                        {
                            List<SourceLine> added = new List<SourceLine>();                        
                            IEnumerable<char> s = info.Data == null ? this.Source.GetTextAt(from, from + info.Size) : info.Data;
                            bool hasLineFeed = false;
                            int i = -1;
                            foreach(char c in s) 
                            {   //Check line feed to detect splitted lines
                                i++;
                                if (c == '\n') 
                                {
                                    int lineFeedPos = from + i + 1;
                                    added.Add(new SourceLine(Source.AddPosition(new Position(lastPos,0)), Source.AddPosition(new Position(lineFeedPos))));
                                    lastPos = lineFeedPos;
                                    hasLineFeed = true;
                                }
                            }
                            if (hasLineFeed) 
                            {
                                int nremoved = removeLine != null ? 1 : 0;
                                if ((from + length == removeTo) && (lastPos != removeTo) && ((index+1) < LineCount)) 
                                {
                                    SourceLine l = this[index+1];                                    
                                    removeTo = l.To;
                                    nremoved++;
                                }
                                if (lastPos < removeTo) 
                                {
                                    added.Add(new SourceLine(Source.AddPosition(new Position(lastPos, 0)), Source.AddPosition(new Position(removeTo, 0))));
                                }
                                SourceLine[] added_lines = added.ToArray();
                                Replace(index, nremoved, added_lines);
                            }
                        } catch (Exception e) 
                        {
                            throw e;
                        }
                    }
                    break;
                case SourceText.TextChanges.TextAboutDeleted:
                    {
                        int from = info.From;
                        int firstLine = GetLineIndex(from);
                        int lastLine = GetLineIndex(info.To);
                        if (firstLine != lastLine) 
                        {
                            int nremoved = (lastLine - firstLine) + 1;//Count of line removed
                            int startPos = this[firstLine].From;
                            int endPos = this[lastLine].To;
                            SourceLine[] added_lines = new SourceLine[1];
                            added_lines[0] = new SourceLine(Source.AddPosition(new Position(startPos, 0)), Source.AddPosition(new Position(endPos, 0)));
                            Replace(firstLine, nremoved, added_lines);
                        }
                    }
                    break;
                default:
                    break;
            }
        }

        /// <summary>
        /// Replaces some lines of the document with new ones.
        /// </summary>
        /// <param name="from">The start offset of the replacement</param>
        /// <param name="length">The length of the replacement</param>
        /// <param name="replace_lines">The new lines to replace</param>
        private void Replace(int from, int length, SourceLine[] replace_lines) 
        {
            int amount = replace_lines.Length - length;//The amount of line to replace
            int src = from + length;
            //Count of line to shift
            int nshift = nlines - src;
            int target = src + amount;
            if ((nlines + amount) >= lines.Length) 
            {
                // Expand the array by a multiple of two
                int newLength = Math.Max(lines.Length << 1, nlines + amount);
                SourceLine[] newlines = new SourceLine[newLength];
                Array.Copy(lines, 0, newlines, 0, from);
                Array.Copy(replace_lines, 0, newlines, from, replace_lines.Length);
                Array.Copy(lines, src, newlines, target, nshift);
                lines = newlines;
            } 
            else 
            {
                // Update the current array
                Array.Copy(lines, src, lines, target, nshift);
                Array.Copy(replace_lines, 0, lines, from, replace_lines.Length);
            }
            nlines = nlines + amount;
        }

        /// <summary>
        /// Gets the line index closest to the given offset. This performed by a binary search.
        /// </summary>
        /// <param name="pos">pos the pos >= 0</param>
        /// <returns>the element index >= 0</returns>
        public int GetLineIndex(int pos) 
        {
            if (nlines == 0)
            {//No Line
                return 0;
            }

            int index;
            int top = 0;
            int bottom = nlines - 1;
            int middle = 0;
            int from = From;
            int to;
    
            if (pos >= To) 
            {//Out of document ==> last index
                return bottom;
            }
   
            if ((lastIndex >= top) && (lastIndex <= bottom)) 
            {
                SourceLine lastLine = lines[lastIndex];
                from = lastLine.From;
                to = lastLine.To;
                if ((pos >= from) && (pos < to)) 
                {
                    return lastIndex;
                }    
                if (pos < from) 
                {
                    bottom = lastIndex;
                } else  
                {
                    top = lastIndex;
                }
            }
    
            while (top <= bottom) 
            {
                middle = top + ((bottom - top) >> 1);
                SourceLine line = lines[middle];
                from = line.From;
                to = line.To;
                if ((pos >= from) && (pos < to)) 
                {
                    //we get the location
                    lastIndex = index = middle;
                    return index;
                } 
                else if (pos < from) 
                {
                    bottom = middle - 1;
                } else 
                {
                    top = middle + 1;
                }
            }
               
            //The index was not found but determine where it should be
            lastIndex = index = (pos < from) ? index = middle : index = middle + 1;
            return index;
        }

        /// <summary>
        /// Dump this Source Document.
        /// </summary>
        public void Dump()
        {
            Source.Dump();
            DumpLinePosition(true);
            //Source.GetPositionList().Dump();
        }

        /// <summary>
        /// Dump this Source Document Line Position.
        /// <param name="bShowText">true if the text of the line must be displayed</param>
        /// </summary>
        public void DumpLinePosition(bool bShowText)
        {
            //char[] buffer = new char[255];
            for (int i = 0; i < LineCount; i++)
            {
                SourceLine line = lines[i];
                System.Console.Write("[{0:00000} - {1:00000}]", line.From, line.To);
                if (bShowText)
                {
                    //Source.CopyInStr(buffer, buffer.Length, line.From, line.To);
                    String  text = Source.GetTextAt(line.From, line.To);
                    System.Console.Write(text);
                }
            }
        }

        /// <summary>
        /// Write this Source document
        /// </summary>
        /// <param name="writer">The target StringBuilder</param>
        /// <param name="bTrimRight">true if trailing white spaces must be removed</param>
        public void Write(StringBuilder stringBuilder, bool bTrimRight = false)
        {
            if (!bTrimRight)
            {
                //Directly write the source text
                this.Source.Write(stringBuilder);
            }
            else
            {   //TCCODEGEN_NO_TRAILING_SPACES
                //Here write line by line and trim end each one.
                for (int i = 0; i < LineCount; i++)
                {
                    SourceLine line = lines[i];
                    String text = Source.GetTextAt(line.From, line.To);
                    text = text.TrimEnd();
                    stringBuilder.AppendLine(text);
                }
            }
        }

        /// <summary>
        /// Class that represents a source line in a source document.
        /// </summary>
        public class SourceLine
        {
            /// <summary>
            /// The start position
            /// </summary>
            public Position Start
            {
                get;
                private set;
            }
            /// <summary>
            /// The end position
            /// </summary>
            public Position End
            {
                get;
                private set;
            }

            /// <summary>
            /// The Start Offset
            /// </summary>
            public int From
            {
                get
                {
                    return Start.Pos;
                }
            }

            /// <summary>
            /// The End Offset
            /// </summary>
            public int To
            {
                get
                {
                    return End.Pos;
                }
            }
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="start">The Start Position</param>
            /// <param name="end">The end position</param>
            public SourceLine(Position start, Position end)
            {
                this.Start = start;
                this.End = end;
            }
        }
    }
}
