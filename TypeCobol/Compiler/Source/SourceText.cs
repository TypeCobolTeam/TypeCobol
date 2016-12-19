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
            /// <param name="size">Sizeo fthe change</param>
            public TextChangeInfo(int from, int to, int size)
            {
                From = from;
                To = to;
                Size = size;
            }
        }
        /// <summary>
        /// Observers on Text Change Events.
        /// </summary>
        public event ChangedEventHandler Observers;
        /// <summary>
        /// The list of positions
        /// </summary>
        PositionList Positions;

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
        public void Delete(int from, int to)
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
        public void Insert(SourceText text, int from, int to)
        {
            if (Positions != null)
            {
                if (from != to)
                    Positions.Delete(from, to - from);
                Positions.Insert(from, text.Size());
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
        public void Insert(char c, int from, int to)
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
        public void Append(char c)
        {
            Insert(c, Size(), Size());
        }

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
        public abstract int Size();
    }
}
