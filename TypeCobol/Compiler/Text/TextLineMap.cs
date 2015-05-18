using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Line types defined in the Cobol reference format
    /// </summary>
    public enum TextLineType
    {
        /// <summary>
        /// Blank indicator in column 7
        /// </summary>
        Source,
        /// <summary>
        /// 'D' or 'd' indicator in colunm 7
        /// </summary>
        Debug,
        /// <summary>
        /// '*' or '/' indicator in column 7
        /// </summary>
        Comment,
        /// <summary>
        /// '-' indicator in column 7
        /// </summary>
        Continuation,
        /// <summary>
        /// Any other indicator char is invalid
        /// </summary>
        Invalid,
        /// <summary>
        /// A blank line contains nothing but spaces in column 7 through column 72
        /// </summary>
        Blank
    }

    /// <summary>
    /// Portion of a text line with a specific meaning
    /// </summary>
    public class TextArea
    {
        internal TextArea(int startIndex, int endIndex)
        {
            StartIndex = startIndex;
            EndIndex = endIndex;
        }

        /// <summary>
        /// Index of the first char of the area
        /// </summary>
        public int StartIndex { get; private set; }
        
        /// <summary>
        /// Index of the last char of the area
        /// </summary>
        public int EndIndex   { get; private set; }

        // True if EndIndex < StartIndex : no char in the area
        public bool IsEmpty
        {
            get { return EndIndex < StartIndex; }
        }


        public override string ToString()
        {
            return "[" + StartIndex + "," + EndIndex + "]";
        }
    }

    /// <summary>
    /// Enumeration of the standard text areas
    /// </summary>
    public enum TextAreaType
    {
        SequenceNumber,
        Indicator,
        Source,
        Comment
    }

    /// <summary>
    /// Partition of a COBOL source line into reference format areas
    /// </summary>
    public class TextLineMap
    {
        public TextLineMap(ITextLine textLine, ColumnsLayout columnsLayout)
        {
            TextLine = textLine;

            // Scan the line to find the indexes of the different areas
            if (columnsLayout == ColumnsLayout.CobolReferenceFormat)
            {
                MapVariableLengthLineWithReferenceFormat();
            }
            // unlimited length lines
            else
            {
                MapVariableLengthLineWithFreeFormat();
            }

            // Study the indicator char to determine the type of the line
            ComputeLineTypeFromIndicator();
        }

        private void MapVariableLengthLineWithReferenceFormat()
        {
            string line = TextLine.Text;
            int lastIndexOfLine = line.Length - 1;

            // Test for free format compiler directives embedded in a reference format file
            int compilerDirectiveIndex = FindFirstCharOfCompilerDirectiveBeforeColumn8(line);
            if (compilerDirectiveIndex >= 0)
            {
                // Free text format line embedded in reference format file
                SequenceNumber = new TextArea(0, compilerDirectiveIndex - 1);
                Indicator = new TextArea(compilerDirectiveIndex, compilerDirectiveIndex - 1);
                Source = new TextArea(compilerDirectiveIndex, lastIndexOfLine > 71 ? 71 : lastIndexOfLine);
                Comment = new TextArea(lastIndexOfLine > 71 ? 72 : lastIndexOfLine + 1, lastIndexOfLine);
            }
            else
            {
                // Cobol reference format
                if (lastIndexOfLine >= 7)
                {
                    SequenceNumber = new TextArea(0, 5);
                    Indicator = new TextArea(6, 6);
                    Source = new TextArea(7, lastIndexOfLine > 71 ? 71 : lastIndexOfLine);
                    Comment = new TextArea(lastIndexOfLine > 71 ? 72 : lastIndexOfLine + 1, lastIndexOfLine);
                }
                else if (lastIndexOfLine == 6)
                {
                    SequenceNumber = new TextArea(0, 5);
                    Indicator = new TextArea(6, 6);
                    Source = new TextArea(7, 6);
                    Comment = new TextArea(7, 6);
                }
                else
                {
                    SequenceNumber = new TextArea(0, lastIndexOfLine);
                    Indicator = new TextArea(lastIndexOfLine + 1, lastIndexOfLine);
                    Source = new TextArea(lastIndexOfLine + 1, lastIndexOfLine);
                    Comment = new TextArea(lastIndexOfLine + 1, lastIndexOfLine);
                }
            }
        }

        private void MapVariableLengthLineWithFreeFormat()
        {
            string line = TextLine.Text;
            int lastIndexOfLine = line.Length - 1;

            // No SequenceNumber area in free format text 
            SequenceNumber = new TextArea(0, -1);

            // In free format source text :
            // - a line starting with char * is a comment line or a compiler directive            
            if (lastIndexOfLine >= 0 && line[0] == '*')
            {
                // Check for compiler directives
                if ((line.Length >= 5 && line.StartsWith("*CBL ")) ||
                    (line.Length >= 9 && line.StartsWith("*CONTROL ")))
                {
                    Indicator = new TextArea(0, -1);
                    Source = new TextArea(0, lastIndexOfLine);
                }
                else
                {
                    Indicator = new TextArea(0, 0);
                    Source = new TextArea(1, lastIndexOfLine);
                }
            }
            // - a line starting with char / is a comment line 
            // - a line starting with char - is a continuation line
            // => a free format program line cannot start with one of these three chars, insert a space before if needed
            else if (lastIndexOfLine >= 0 && (line[0] == '/' || line[0] == '-'))
            {
                Indicator = new TextArea(0, 0);
                Source = new TextArea(1, lastIndexOfLine);
            }
            // - a line starting with d or D + space char is a debug ligne
            else if (lastIndexOfLine >= 1 && ((line[0] == 'd' || line[0] == 'D') & line[1] == ' '))
            {
                Indicator = new TextArea(0, 1);
                Source = new TextArea(2, lastIndexOfLine);
            }
            else // no indicator
            {
                Indicator = new TextArea(0, -1);
                Source = new TextArea(0, lastIndexOfLine);
            }

            // No Comment area in free format text
            Comment = new TextArea(lastIndexOfLine + 1, lastIndexOfLine);
        }

        // List of compiler directives keywords which can be encountered before column 8
        // even in Cobol reference format, and which force a certain form a free format
        // text lines in otherwise reference format files
        internal static string[] COMPILER_DIRECTIVE_KEYWORDS_STARTING_BEFORE_LINE_8 = new string[] {
            "CBL",
            "PROCESS",
            "*CBL",
            "*CONTROL",
            "BASIS",
            "DELETE",
            "INSERT",
        };
        
        private static int FindFirstCharOfCompilerDirectiveBeforeColumn8(string line)
        {
            bool compilerDirectiveFound = false;

            // We are interested in compiler directives starting before column 8
            int endOfSearchIndex = line.Length >= 8 ? 6 : (line.Length - 1);

            // Scan the first seven chars of the line to find a compiler directive start
            int firstCharIndex;
            for (firstCharIndex = 0; (firstCharIndex <= endOfSearchIndex && !compilerDirectiveFound); firstCharIndex++)
            {
                char firstChar = line[firstCharIndex];

                // If the char is numeric or space (regular sequence number), abort the search immediately
                if (Char.IsDigit(firstChar) || firstChar == ' ') continue;

                // Try to match each one of the potential compiler directives in turn
                for (int compilerDirectiveIndex = 0; (compilerDirectiveIndex < COMPILER_DIRECTIVE_KEYWORDS_STARTING_BEFORE_LINE_8.Length && !compilerDirectiveFound); compilerDirectiveIndex++)
                {
                    string compilerDirectiveKeyword = COMPILER_DIRECTIVE_KEYWORDS_STARTING_BEFORE_LINE_8[compilerDirectiveIndex];

                    // Try to match the first character of a compiler directive
                    if (Char.ToUpper(firstChar) == compilerDirectiveKeyword[0])
                    {
                        // Check to see if the line is long enough to hold the compiler directive
                        if (line.Length >= firstCharIndex + compilerDirectiveKeyword.Length)
                        {
                            int afterLastCharIndex = firstCharIndex + compilerDirectiveKeyword.Length;

                            // Try to match the following characters
                            int nextCharIndex = firstCharIndex + 1;
                            int keywordCharIndex = 1;
                            for (; nextCharIndex < afterLastCharIndex; nextCharIndex++, keywordCharIndex++)
                            {
                                if (Char.ToUpper(line[nextCharIndex]) != compilerDirectiveKeyword[keywordCharIndex])
                                {
                                    // Exit the loop as soon as a char does not match
                                    break;
                                }
                            }
                            // Complete keyword matched => exit both loops
                            if (nextCharIndex == afterLastCharIndex)
                            {
                                if (nextCharIndex == line.Length || CobolChar.IsCobolWordSeparator(line[nextCharIndex]))
                                {
                                    compilerDirectiveFound = true;
                                }
                            }
                        }
                    }
                }
            }

            if (compilerDirectiveFound)
            {
                return firstCharIndex - 1;
            }
            else
            {
                return -1;
            }
        }

        private void ComputeLineTypeFromIndicator()
        {
            // Compute line type
            switch (IndicatorChar)
            {
                case ' ':
                    Type = TextLineType.Source;
                    break;
                case '*':
                case '/':
                    Type = TextLineType.Comment;
                    break;
                case 'D':
                case 'd':
                    Type = TextLineType.Debug;
                    break;
                case '-':
                    Type = TextLineType.Continuation;
                    break;
                default:
                    Type = TextLineType.Invalid;
                    break;
            }

            // Detect blank lines
            if((Type == TextLineType.Source || Type == TextLineType.Debug || Type == TextLineType.Continuation) &&
               (Source.IsEmpty || String.IsNullOrWhiteSpace(SourceText)))
            {
                Type = TextLineType.Blank;
            }
        }

        /// <summary>
        /// Source, Debug, Comment or Continuation
        /// </summary>
        public TextLineType Type { get; set; }

        /// <summary>
        /// Underlying text line
        /// </summary>
        public ITextLine TextLine { get; private set; }
        
        /// <summary>
        /// Sequence number area : Columns 1 through 6
        /// </summary>
        public TextArea SequenceNumber { get; private set; }

        public string SequenceNumberText
        {
            get
            {
                return SequenceNumber.IsEmpty ? null : TextLine.TextSegment(SequenceNumber.StartIndex, SequenceNumber.EndIndex);
            }
        }

        /// <summary>
        /// Indicator area : Column 7
        /// </summary>
        public TextArea Indicator { get; private set; }

        public char IndicatorChar
        {
            get
            {
                return Indicator.IsEmpty ? ' ' : TextLine.TextSegment(Indicator.StartIndex, Indicator.EndIndex)[0];
            }
        }

        /// <summary>
        /// Area A : Columns 8 through 11 
        /// Area B : Columns 12 through 72 
        /// </summary>
        public TextArea Source { get; private set; }

        public string SourceText
        {
            get
            {
                return Source.IsEmpty ? null : TextLine.TextSegment(Source.StartIndex, Source.EndIndex);
            }
        }

        /// <summary>
        /// Comment Area : Columns 73 through 80+
        /// </summary>
        public TextArea Comment { get; private set; }

        public string CommentText
        {
            get
            {
                return Comment.IsEmpty ? null : TextLine.TextSegment(Comment.StartIndex, Comment.EndIndex);
            }
        }

        public override string ToString()
        {
            return "SequenceNumber" + SequenceNumber + " Indicator" + Indicator + " Source" + Source + " Comment" + Comment;
        }

        /// <summary>
        /// Create an isolated TextLineMap object, not based on a real line of a TextDocument.
        /// Useful only for unit tests, or to compute intermediary results.
        /// </summary>
        public static TextLineMap Create(string text)
        {
            ITextLine isolatedTextLine = new TextLine(0, 0, text);
            return new TextLineMap(isolatedTextLine, ColumnsLayout.FreeTextFormat);
        }
    }
}
