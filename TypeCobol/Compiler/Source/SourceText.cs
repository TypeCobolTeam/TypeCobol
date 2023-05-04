using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// An abstract representation of a Source Text. The class Text defines methods
    /// to manage a buffer for holding source text. Together with the source text is a 
    /// list of robust pointers (called Positions) into the source text. 
    /// Dependents of the Source Text are notivied by the observer mechanism and
    /// receive a change event.
    /// </summary>
    public abstract class SourceText : IEnumerable<char>
    {
       // A delegate type for hooking up change notifications.
       public delegate void ChangedEventHandler(object sender, EventArgs e);


        /// <summary>
        /// The kind of text changes.
        /// </summary>
        public enum TextChanges
        {
            TextChanngeRange,
            TextAboutDeleted,
            TextDeleted,
            TextReplaced // from,to=> replaced range, size=>size of inserted text
        };

        /// <summary>
        /// Text change information for Text Change Events
        /// </summary>
        public class TextChangeInfo : EventArgs
        {
            /// <summary>
            /// The Kin of change
            /// </summary>
            public TextChanges Kind
            {
                get;
                private set;
            }

            /// <summary>
            /// Source location
            /// </summary>
            public int From
            {
                get;
                private set;
            }

            /// <summary>
            /// End location
            /// </summary>
            public int To
            {
                get;
                private set;
            }

            /// <summary>
            /// Size of the change
            /// </summary>
            public int Size
            {
                get;
                private set;
            }

            /// <summary>
            /// Inserted Data if any
            /// </summary>
            public IEnumerable<char> Data
            {
                get;
                internal set;
            }

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="from">Start locatin</param>
            /// <param name="to">End location</param>
            /// <param name="size">Size of the change if Insert</param>
            public TextChangeInfo(TextChanges kind, int from, int to, int size = 0)
            {
                Kind = kind;
                From = from;
                To = to;
                Size = size;
            }
        }
        /// <summary>
        /// The Default TAB width
        /// </summary>
        public const int DEFAULT_TAB_WIDTH = 8;

        /// <summary>
        /// Observers on Text Change Events.
        /// </summary>
        public event ChangedEventHandler Observers;
        /// <summary>
        /// The list of positions
        /// </summary>
        PositionList Positions;

        /// <summary>
        /// The with of a tabulation to by applied in this Source Text.
        /// </summary>
        public int TabWidth
        {
            get;
            set;
        }

        /// <summary>
        /// Allow to associate a Custom Flag to this buffer?
        /// </summary>
        public uint CustomFlags
        {
            get;
            set;
        }

        /// <summary>
        /// Set the Value of a Custom flag.
        /// </summary>
        /// <param name="flag"></param>
        /// <param name="value"></param>
        /// 
        public void SetCustomFlag(uint flag, bool value)
        {
            if (value)
                CustomFlags |= flag;
            else
                CustomFlags &= ~flag;
        }

        /// <summary>
        /// Check if a given flag is set.
        /// </summary>
        /// <param name="flag">The glag to check</param>
        /// <returns>true if yes, false otherwise.</returns>
        public bool IsFlagSet(int flag)
        {
            return (CustomFlags & flag) != 0;
        }

        /// <summary>
        /// Empty constructor
        /// </summary>
        protected SourceText()
        {
            Positions = null;
        }

        /// <summary>
        /// Delete a portion of text
        /// </summary>
        /// <param name="from">The start offset</param>
        /// <param name="to">and the end offset</param>
        public virtual void Delete(int from, int to)
        {
            if (Positions != null)
                Positions.Delete(from, to - from);
        }

        /// <summary>
        /// Insert the content of a SourceText from a position up to a position.
        /// </summary>
        /// <param name="text">The SourceText to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public virtual void Insert(SourceText text, int from, int to)
        {
            if (Positions != null)
            {
                if (from != to)
                    Positions.Delete(from, to - from);
                Positions.Insert(from, text.Size);
            }
        }

        /// <summary>
        /// Insert the content of a String from a position up to a position.
        /// </summary>
        /// <param name="text">The String to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public virtual void Insert(String text, int from, int to)
        {
            if (Positions != null)
            {
                if (from != to)
                    Positions.Delete(from, to - from);
                Positions.Insert(from, text.Length);
            }
        }

        /// <summary>
        /// Insert the content of an array of characters from a position up to a position.
        /// </summary>
        /// <param name="text">The array of characters to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public virtual void Insert(char[] buffer, int from, int to)
        {
            if (Positions != null)
            {
                if (from != to)
                    Positions.Delete(from, to - from);
                Positions.Insert(from, buffer.Length);
            }
        }

        /// <summary>
        /// Copy a portion of source text into a target SourceText instance
        /// </summary>
        /// <param name="target"></param>
        /// <param name="from">Start offste of the portion</param>
        /// <param name="to">End offset of the portion</param>
        public abstract void Copy(SourceText target, int from , int to);

        /// <summary>
        /// Copy a portion of text from this SourceText to a given buffer.
        /// </summary>
        /// <param name="buffer">The buffer in to which to copy a portion of text</param>
        /// <param name="length">The buffer's length</param>
        /// <param name="from">The start offset of the portion</param>
        /// <param name="to">The end offset of the portion</param>
        public abstract void CopyInStr(char[] buffer, int length, int from, int to);

        /// <summary>
        /// Copy The content of a given buffer in to this Source Text,
        /// The content of the buffer replaces the content of this SourceText.
        /// </summary>
        /// <param name="buffer">The buffer which contains the text to copy</param>
        /// <param name="count">The count of characters to copy</param>
        public abstract void ReplaceWithStr(char[] buffer, int count);


        /// <summary>
        /// Get the text of a portion
        /// </summary>
        /// <param name="from">The Start position of the portion of text to retrieve</param>
        /// <param name="to">The End position of the portion of text to retrieve</param>
        /// <returns>The String of the portion</returns>
        public abstract String GetTextAt(int from, int to);
        /// <summary>
        /// Save a text portion of this SourceText in another SourceText
        /// </summary>
        /// <param name="from">The start position of the portion</param>
        /// <param name="to">The end position of the portion</param>
        /// <returns>The SourceText containing the portion</returns>
        public abstract SourceText Save(int from, int to);

        /// <summary>
        /// Insert one character in this SourceText from a location to another location.
        /// </summary>
        /// <param name="c">The character to insert</param>
        /// <param name="from">The start offset offset the insert location</param>
        /// <param name="to">The end offset of the insertion location.</param>
        public virtual void Insert(char c, int from, int to)
        {
            if (Positions != null) 
            {
                if (from != to)
                    Positions.Delete(from, to - from);
                Positions.Insert(from, 1); 
            }
        }

        /// <summary>
        /// Append a character in this SourceText
        /// </summary>
        /// <param name="c">The caharacter to append</param>
        public virtual void Append(char c)
        {
            Insert(c, Size, Size);
        }

        /// <summary>
        /// Reset this Source Text to an empty content with given size capacity.
        /// </summary>
        /// <param name="initSize">The reset capacity</param>
        public abstract void Empty(int initSize = 0);

        /// <summary>
        /// Indexer operator
        /// </summary>
        /// <param name="i">Index of the character</param>
        /// <returns>The character at the given index.</returns>
        public abstract char this[int i]
        {
            get;
            set;
        }

        /// <summary>
        /// Get the size of this Source Text
        /// </summary>
        /// <returns>The Size</returns>
        public abstract int Size
        {
            get;
        }

        /// <summary>
        /// Is this Source Text Empty
        /// </summary>
        public bool IsEmpty
        {
            get
            {
                return Size == 0;
            }
        }

        /// <summary>
        /// Check range boundaries
        /// </summary>
        /// <param name="max">Maximal boundary</param>
        /// <param name="from">The start offset of the range</param>
        /// <param name="to">The end offset of the range</param>
        /// <returns></returns>
        public static bool CheckRange (int max, int from, int to)
        {
            return (to > max || from < 0 || from > to) ? false: true;
        }

        /// <summary>
        /// A simple implementation to check if a character can be part of a word.
        /// </summary>
        /// <param name="c">The character to be checked</param>
        /// <returns>true if yes, false otherwise.</returns>
        public static bool IsWordCharacter(char c)
        {
            return Char.IsLetterOrDigit(c) || c == '_';
        }

        /// <summary>
        /// Get the boundaries of a word at a given position
        /// </summary>
        /// <param name="at">The word's position to be gotten the boundaries</param>
        /// <param name="start">Output of teh start offset of teh boundarry</param>
        /// <param name="end">Output the end offset of the boundary</param>
        /// <exception cref="InvalidPositionException">thrown if the at argument is invalid</exception>
        public void GetWordBoundaries(int at, out int start, out int end)
        {
            int i;

            if (!CheckRange(Size, at, at))
            {
                throw new InvalidPositionException("GetWordBoundaries" ,at, 0);
            }
            
            for (i = at-1; i >= 0 && IsWordCharacter(this[i]); i--)
            {
                ;
            }
            start = i+1; 
            for (i = at; i < Size && IsWordCharacter(this[i]); i++)
            {
                ; 
            }
            end = i;
        }

        /// <summary>
        /// Get the boundaries of a paragraph  at a given position, in fact the boudaries of the line
        /// that contains the given position.
        /// </summary>
        /// <param name="at">The paragraph's position to be gotten the boundaries</param>
        /// <param name="start">Output of teh start offset of teh boundarry</param>
        /// <param name="end">Output the end offset of the boundary</param>
        /// <exception cref="InvalidPositionException">thrown if the at argument is invalid</exception>
        public void GetParagraphBoundaries(int at, out int start, out int end)
        {
            int i, ch;

            if (!CheckRange(Size, at, at))
            {
                throw new InvalidPositionException("GetParagraphBoundaries", at, 0);
            }

            for (i = at-1; i >= 0; i--) {
            ch= this[i];
            if (ch == '\n' || ch == '\r')
                break;
            }
            start = i+1; 
            for (i = at; i < Size; i++) {
            ch= this[i];
            if (ch == '\n' || ch == '\r')
                break; 
            }
            end= Math.Min(Size, i+1);
        }

        /// <summary>
        /// Get the boundaries of a line  at a given position, in fact the boudaries of the line
        /// that contains the given position.
        /// </summary>
        /// <param name="at">The line's position to be gotten the boundaries</param>
        /// <param name="start">Output of teh start offset of teh boundarry</param>
        /// <param name="end">Output the end offset of the boundary</param>
        /// <exception cref="InvalidPositionException">thrown if the at argument is invalid</exception>
        public void GetLineBoundaries(int at, out int start, out int end)
        {
            int i, ch;

            if (!CheckRange(Size, at, at))
            {
                throw new InvalidPositionException("GetParagraphBoundaries", at, 0);
            }

            for (i = at - 1; i >= 0; i--)
            {
                ch = this[i];
                if (ch == '\n')
                    break;
            }
            start = i + 1;
            for (i = at; i < Size; i++)
            {
                ch = this[i];
                if (ch == '\n')
                    break;
            }
            end = Math.Min(Size, i + 1);
        }

        /// <summary>
        /// Compute the position of all lines from a start position to an end position.
        /// </summary>
        /// <param name="start">The start position</param>
        /// <param name="end">The end position</param>
        /// <returns>An array of dimension [n,2] where n is the count of line encountered,
        /// [i,0] will be teh start postion of line i, and [i,1] will be the end position of line i.</returns>
        public int[,] ComputeLinePositions(int start, int end)
        {
            List<int> lineStart = new List<int>();
            List<int> lineEnd = new List<int>();
            do
            {
                int bstart, bend;
                GetLineBoundaries(start, out bstart, out bend);
                lineStart.Add(bstart);
                lineEnd.Add(bend);
                start = bend;
            } while (start < end);
            int n = lineStart.Count;
            int[,] positions = new int[n, 2];
            for (int i = 0; i < n; i++)
            {
                positions[i, 0] = lineStart[i];
                positions[i, 1] = lineEnd[i];
            }
            return positions;
        }

        /// <summary>
        /// Compute the count of lines from the start position to an end position
        /// </summary>
        /// <param name="start">The start position</param>
        /// <param name="end">The end position</param>
        /// <returns>The count of lines.</returns>
        public int LineCount(int start, int end)
        {
            int count = 0;
            do
            {
                int bstart, bend;
                GetLineBoundaries(start, out bstart, out bend);
                start = bend;
                count++;                
            } while (start < end);
            return count;
        }

        /// <summary>
        /// Get the line infiormation of a length that contains a given position, the length does not take in account any \r or \n  characters.
        /// </summary>
        /// <param name="at">The Position to get the line of the length which conatins it.</param>
        /// <param name="start">Output of teh start offset of the line</param>
        /// <param name="end">Output the end offset of the line</param>
        /// <returns>The line length, the length does not take in account any \r or \n  characters</returns>
        public int GetLineInfo(int at, out int start, out int end)
        {
            int i, ch;

            if (!CheckRange(Size, at, at))
            {
                throw new InvalidPositionException("GetLineInfo", at, 0);
            }

            for (i = at - 1; i >= 0; i--)
            {
                ch = this[i];
                if (ch == '\n')
                    break;
            }
            bool bHasCr = false;
            start = i + 1;
            for (i = at; i < Size; i++)
            {
                ch = this[i];
                if (ch == '\r')
                    bHasCr = true;
                if (ch == '\n')
                    break;
            }
            end = Math.Min(Size, i + 1);
            int len = (i - start) - (bHasCr ? 1 : 0);
            return len;
        }

        /// <summary>
        /// Calculate the offset of a tabulation from a given offset
        /// </summary>
        /// <param name="x">The offset from which to calculate the tabulation</param>
        /// <returns>The offset of the tabulation from the given start offset</returns>
        public int Tabulate(int x)
        {
            if (TabWidth > 0) {
            int n = x / TabWidth;
            return ((n+1) * TabWidth - x);
            }
            return 0;
        }

        /// <summary>
        /// Check if x is in the range [lb, ub] if yes then return x, otherwise lb or ub
        /// </summary>
        /// <param name="lb">The lower bound</param>
        /// <param name="ub">The upper bound</param>
        /// <param name="x">The value to be checked</param>
        /// <returns>x if x is in [lb..ub], lb if x lesset than lb, ub if x is greater than ub</returns>
        protected static int range(int lb, int ub, int x)
        {
            return x < lb ? lb : (x > ub ? ub : x);
        }

        /// <summary>
        /// Grow the size to a desired size.
        /// </summary>
        /// <param name="desiredSize"></param>
        /// <returns>The Grow size</returns>
        protected int GrowBy(int desiredSize)
        {
            int s= 0;

            if (Size >= Int32.MaxValue)
                throw new ArgumentException("GrowBy", "cannot expand text");
            else
                s = range(2, Int32.MaxValue - desiredSize, desiredSize);
            return Size + s;
        } 

        /// <summary>
        /// Add a new position in the possition list
        /// </summary>
        /// <param name="p">The position to be added</param>
        /// <returns>The position added</returns>
        public Position AddPosition(Position p)
        {
            if (Positions == null)
                Positions = new PositionList();
            Positions.Add(p);
            return p;
        }

        /// <summary>
        /// Remmove a position from the list of positions
        /// </summary>
        /// <param name="p">The position to be removed</param>
        /// <returns>true if item is successfully removed; otherwise, false. This method also </returns>
        /// true if item is successfully removed; otherwise, false. 
        /// This method also returns false if position was not found in the list of position.
        public bool RemoveMark(Position p)
        {
            if (Positions == null)
                Positions = new PositionList();
            return Positions.Remove(p);
        }


        /// <summary>
        /// Get an Enumerator on all positions.
        /// </summary>
        /// <returns>An Enumerator on all position</returns>
        public List<Position>.Enumerator GetPositionEnumerator()
        {
            if (Positions == null)
                Positions = new PositionList();
            return Positions.GetEnumerator();
        }

        /// <summary>
        /// Get the Position List
        /// </summary>
        /// <returns>The Position List</returns>
        public PositionList GetPositionList()
        {
            if (Positions == null)
                Positions = new PositionList();
            return Positions;
        }

        /// <summary>
        /// Send a TextChange Event
        /// </summary>
        /// <param name="info"></param>
        internal void Send(TextChangeInfo info)
        {
            if (Observers != null)
            {
                info.Data = null;
                Observers(this, info);
            }
        }

        /// <summary>
        /// Send a TextChange Event
        /// </summary>
        /// <param name="info"></param>
        internal void Send(TextChangeInfo info, IEnumerable<char> data)
        {
            if (Observers != null)
            {
                info.Data = data;
                Observers(this, info);
            }
        }

        /// <summary>
        /// Write the content of this SourceText into a TextWriter
        /// </summary>
        /// <param name="writer">The TextWriter instance</param>
        public abstract void Write(TextWriter writer);

        /// <summary>
        /// Write the content of this SourceText into a StringBuilder
        /// </summary>
        /// <param name="writer">The StringBuilder instance</param>
        public abstract void Write(StringBuilder stringBuilder);

        /// <summary>
        /// Dump the content of this Source Text.
        /// <param name="allChars">Tru eif even non printable chars must be dumped, false otherwise</param>
        /// </summary>
        public virtual void Dump(bool allChars = true)
        {
        }

        /// <summary>
        /// Compare two arrays of characters.
        /// </summary>
        /// <param name="a">The first array</param>
        /// <param name="b">The second array</param>
        /// <param name="length">The length of the comparison</param>
        /// <returns>treue if both arrays are equals according to the length of comparison, false otherwise.</returns>
        protected static bool CompareArrays(char[] a, char[] b, int length)
        {
            if (a.Length != b.Length) { return false; }
            int size = Math.Min(a.Length, length);
            for (int i = 0; i < size; i++)
            {
                if (a[i] != b[i]) { return false; }
            }
            return true;
        }

        /// <summary>
        /// Get the Source Text Enumerator Instance.
        /// </summary>
        /// <returns></returns>
        public IEnumerator<char> GetEnumerator()
        {
            int length = Size;
            for (int i = 0; i < length; i++)
            {
                yield return this[i];
            }
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}

