using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// An abstract representation of a Source Text. The class Text defines methods
    /// to manage a buffer for holding source text. Together with the source text is a 
    /// list of robust pointers (called Positions) into the source text. 
    /// Dependents of the Source Text are notivied by the observer mechanism and
    /// receive a change event.
    /// </summary>
    public abstract class SourceText
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
        /// Empty constructor
        /// </summary>
        public SourceText()
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
        /// The text portion in the range [from - to] will be removed
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

        protected static int range(int lb, int ub, int x)
        {
            return x < lb ? lb : (x > ub ? ub : x);
        }

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
        protected void Send(TextChangeInfo info)
        {
            if (Observers != null)
            {
                Observers(this, info);
            }
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
    }
}

