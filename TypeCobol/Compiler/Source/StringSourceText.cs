using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// A Source Text which is represented as a String of characters
    /// </summary>
    public class StringSourceText : SourceText
    {
        private const int DEFAULT_SIZE = 16;
        int next;           //Next free slot
        int size;          //sizeof allocated memory
        char[] content;     //The content.
        /// <summary>
        /// Size constructor
        /// </summary>
        /// <param name="size"></param>
        public StringSourceText(int size = DEFAULT_SIZE)
        {
            if (size < 0)
                throw new ArgumentException("size < 0");
            this.size = size;
            content = new char[size];
            next = 0;
            TabWidth = DEFAULT_TAB_WIDTH;
        }

        /// <summary>
        /// Characters constructor
        /// </summary>
        /// <param name="buffer">The buffer of characters</param>
        public StringSourceText(char[] buffer) : this(buffer, -1)
        {
        }

        /// <summary>
        /// Characters constructor
        /// </summary>
        /// <param name="buffer">The buffer of characters</param>
        /// <param name="len">The length of characters to copy, -1 to coy all characters from the buffer</param>
        public StringSourceText(char[] buffer, int len) : this(buffer, 0, len)
        {
        }

        /// <summary>
        /// Characters constructor
        /// </summary>
        /// <param name="buffer">The buffer of characters</param>
        ///<param name="index">Buffer index</param>
        /// <param name="len">The length of characters to copy, -1 to coy all characters from the buffer</param>
        public StringSourceText(char[] buffer, int index, int len)
        {
            if (buffer == null)
                throw new ArgumentNullException("buffer");
            int ll = len;
            next = ll == -1 ? (buffer.Length - index) : len;
            size = Math.Max(DEFAULT_SIZE, (next / 5 + 1) * 6); // give some initial free space
            content = new char[size];
            Array.Copy(buffer, index, content, 0, next);
            TabWidth = DEFAULT_TAB_WIDTH;
        }

        /// <summary>
        /// String Constructor.
        /// </summary>
        /// <param name="text">The initial text</param>
        public StringSourceText(String text) : this(text.ToCharArray())
        {
            if (text == null)
                throw new NullReferenceException();
        }

        /// <summary>
        /// Delete a portion of text
        /// </summary>
        /// <param name="from">The start offset</param>
        /// <param name="to">and the end offset</param>
        public override void Delete(int from,int to)
        {
            if (!CheckRange(next, from, to))
	            return;

            base.Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));     

            Array.Copy(content, to, content, from, next - to);

            next-= (to - from);
            if (LowWaterMark()) 
	            Expand(size/2);
            base.Delete(from, to);
            Send(new TextChangeInfo(TextChanges.TextDeleted, from, to));
        }

        /// <summary>
        /// Insert the content of a SourceText from a position up to a position.
        /// </summary>
        /// <param name="text">The SourceText to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public override void Insert(SourceText paste, int from, int to)
        {
            StringSourceText ct;
            char[] buf = null;

            if (!(paste is StringSourceText))
            { // try to convert
	            int s = paste.Size;
	            buf = new char[s];
	            paste.CopyInStr(buf, s, 0, s);
	            ct= new StringSourceText(buf, s);
            } else
	            ct = (StringSourceText)paste; 

            int shift= ct.next - (to - from);

            if (!CheckRange(next, from, to))
	            return;

            if ((to - from) > 0)
                Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));     

            if (HighWaterMark(shift))
	        Expand(GrowBy(size+shift));

            if (shift < 0)
	            Array.Copy(content, to, content, to + shift, next-to);
            else if (shift > 0)
	            Array.Copy(content, from, content, from + shift, next-from);

            //---- insert pasted text
            Array.Copy(ct.content, 0, content, from, ct.next);
            next += shift;
            if (LowWaterMark())
	            Expand(size/2);
            base.Insert(paste, from, to);
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, paste.Size), paste);     
        }

        /// <summary>
        /// Insert the content of a String from a position up to a position.
        /// </summary>
        /// <param name="text">The String to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public override void Insert(String text, int from, int to)
        {            
            int shift = text.Length - (to - from);

            if (!CheckRange(next, from, to))
                return;

            if ((to - from) > 0)
                Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));     

            if (HighWaterMark(shift))
                Expand(GrowBy(size + shift));

            if (shift < 0)
                Array.Copy(content, to, content, to + shift, next - to);
            else if (shift > 0)
                Array.Copy(content, from, content, from + shift, next - from);

            char[] buffer = text.ToCharArray();
            //---- insert pasted text
            Array.Copy(buffer, 0, content, from, buffer.Length);
            next += shift;
            if (LowWaterMark())
                Expand(size / 2);
            base.Insert(text, from, to);
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, text.Length), text);
        }

        /// <summary>
        /// Insert the content of an array of characters from a position up to a position.
        /// </summary>
        /// <param name="buffer">The array of characters to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public override void Insert(char[] buffer, int from, int to)
        {
            int shift = buffer.Length - (to - from);

            if (!CheckRange(next, from, to))
                return;

            if ((to - from) > 0)
                Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));

            if (HighWaterMark(shift))
                Expand(GrowBy(size + shift));

            if (shift < 0)
                Array.Copy(content, to, content, to + shift, next - to);
            else if (shift > 0)
                Array.Copy(content, from, content, from + shift, next - from);

            //---- insert pasted text
            Array.Copy(buffer, 0, content, from, buffer.Length);
            next += shift;
            if (LowWaterMark())
                Expand(size / 2);
            base.Insert(buffer, from, to);
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, buffer.Length), buffer);
        }

        /// <summary>
        /// Copy a portion of source text into a target SourceText instance
        /// </summary>
        /// <param name="target"></param>
        /// <param name="from">Start offste of the portion</param>
        /// <param name="to">End offset of the portion</param>
        public override void Copy(SourceText target, int from, int to)
        {
            if (!CheckRange(next,from,to) || target == null)
	        return;

            if (!(target is StringSourceText)) 
            { // convert
	            int s = to - from;
	            char[] buf= new char[s];
	            Array.Copy(content, from, buf, 0, s);
	            target.ReplaceWithStr(buf, s);
	            return;
            }

            StringSourceText ct= (StringSourceText)target;    
            int nSave= to-from;

            if (ct.size < nSave) 
	            ct.Expand(nSave);

            Array.Copy(content, from, ct.content, 0, nSave);
            ct.next= nSave;
        }

        /// <summary>
        /// Copy a portion of text from this SourceText to a given buffer.
        /// </summary>
        /// <param name="buffer">The buffer in to which to copy a portion of text</param>
        /// <param name="length">The buffer's length</param>
        /// <param name="from">The start offset of the portion</param>
        /// <param name="to">The end offset of the portion</param>
        public override void CopyInStr(char[] buffer, int length, int from, int to)
        {
            if (!CheckRange(next, from, to) || buffer == null)
	            return;

            int l = Math.Min(to, from + length) - from;
                Array.Copy(content, from, buffer, 0, l);
        }

        /// <summary>
        /// Copy The content of a given buffer in to this Source Text,
        /// The content of the buffer replaces the content of this SourceText.
        /// </summary>
        /// <param name="buffer">The buffer which contains the text to copy</param>
        /// <param name="count">The count of characters to copy</param>
        public override void ReplaceWithStr(char[] buffer, int count)
        {
            if (count == -1)
                count = buffer.Length;
            if (size < count)
                Expand(count);
            Array.Copy(buffer, content, count);
            next = count;
        }

        /// <summary>
        /// Get the text of a portion
        /// </summary>
        /// <param name="from">The Start position of the portion of text to retrieve</param>
        /// <param name="to">The End position of the portion of text to retrieve</param>
        /// <returns>The String of the portion</returns>
        public override string GetTextAt(int from, int to)
        {
            if (!CheckRange(next, from, to))
                return null;
            return new string(content, from, to - from);
        }

        /// <summary>
        /// Save a text portion of this SourceText in another SourceText
        /// </summary>
        /// <param name="from">The start position of the portion</param>
        /// <param name="to">The end position of the portion</param>
        /// <returns>The SourceText containing the portion</returns>
        public override SourceText Save(int from, int to)
        {
            if (!CheckRange(next, from, to))
                return null;
            return new StringSourceText(content, from, to - from);
        }

        /// <summary>
        /// Insert one character in this SourceText from a location to another location.
        /// </summary>
        /// <param name="c">The character to insert</param>
        /// <param name="from">The start offset offset the insert location</param>
        /// <param name="to">The end offset of the insertion location.</param>
        public override void Insert(char c, int from, int to)
        {
            int shift = to - from + 1;

            if (!CheckRange(next, from, to))
	        return;

            if (HighWaterMark(shift))
	            Expand(GrowBy(size + shift));

            if (shift < 0)
	            Array.Copy(content, to, content, to + next, size-to);
            else if (shift > 0)
	            Array.Copy(content, from, content,  from + shift, next-from);

            content[from] = c;
            next += shift;
            if (LowWaterMark())
	            Expand(size / 2);
            base.Insert(c, from, to);
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, 1), new char[] { c });     
        }

        /// <summary>
        /// Indexer operator
        /// </summary>
        /// <param name="i">Index of the character</param>
        /// <returns>The character at the given index.</returns>
        public override char this[int i]
        {
            get
            {
                if (!CheckRange(next, i, i))
                    i = size - 1;
                return content[i];
            }
            set
            {
                if (!CheckRange(next, i, i))
                    i = size - 1;
                content[i] = value;
            }
        }

        /// <summary>
        /// Reset this Source Text to an empty content with given size capacity.
        /// </summary>
        /// <param name="initSize">The reset capacity</param>
        public override void Empty (int initSize)                                         
        {
            next = 0; 
        }

        /// <summary>
        /// Get the size of this Source Text
        /// </summary>
        /// <returns>The Size</returns>
        public override int Size
        {
            get
            {
                return next;
            }
        }

        /// <summary>
        /// Expand the content buffer to a new size
        /// </summary>
        /// <param name="newSize">The new size</param>
        private void Expand (int newSize)                                         
        {
            if (newSize == 0)
	            newSize= size * 2;

            if (newSize < Size)  // texts never shrink
	            return;
            Array.Resize(ref content, newSize);
            next = Math.Min(newSize, next);
            size= newSize;
        }

        /// <summary>
        /// Are we going beyond the buffer limit if we add n characters ?
        /// </summary>
        /// <param name="n">The count of characters to be added</param>
        /// <returns>true if we go beyond the buffer limit, false otherwise</returns>
        bool HighWaterMark(int n)
        { 
            return (bool)(content == null || next + n >= size); 
        }

        /// <summary>
        /// Have we reach a the  minimal suitable ratio of content size?
        /// </summary>
        /// <returns>true if yes, false otherwise</returns>
        bool LowWaterMark()
        { 
            return (bool)(next < size / 5); 
        }

        /// <summary>
        /// Write the content of this SourceText into a TextWriter
        /// </summary>
        /// <param name="writer">The TextWriter instance</param>
        public override void Write(TextWriter writer)
        {
            writer.Write(content, 0, Size);
        }

        /// <summary>
        /// Write the content of this SourceText into a StringBuilder
        /// </summary>
        /// <param name="writer">The StringBuilder instance</param>
        public override void Write(StringBuilder stringBuilder)
        {
            stringBuilder.Append(content, 0, Size);
        }

        /// <summary>
        /// Dump the content of this Source Text.
        /// <param name="allChars">Tru eif even non printable chars must be dumped, false otherwise</param>
        /// </summary>
        public override void Dump(bool allChars = true)
        {
            System.Console.Write(content, 0, next);
        }
    }
}
