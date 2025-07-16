using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// A Gap Source Text, with the concept of https://en.wikipedia.org/wiki/Gap_buffer:
    /// that allows efficient insertion and deletion operations clustered near the same location
    /// </summary>
    public class GapSourceText : SourceText
    {
        int size;                               // size of allocated memory
        int length;                             // length of text
        int part1len;                           // length of text before gap
        int part2len;                           // redundant
        int gaplen;                             // length of gap
        char[] body;                             // access to text before gap
        int part2body;                        // access to text behind gap
        int body2;                            // redundant

        const int SHRINK_FACTOR = 2;
        const int MAX_OUTPUT = 500;
        const int INITIAL_SIZE = 16;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="s">Initial buffer size</param>
        public GapSourceText(int s = INITIAL_SIZE)
        { 
            size = Math.Max(INITIAL_SIZE, s);
            body = new char[size];
            length = 0;
            TabWidth = DEFAULT_TAB_WIDTH;
        }

        /// <summary>
        /// Buffer Constructor
        /// </summary>
        /// <param name="buf">The initial content</param>
        /// <param name="len">The len of the initial content</param>
        /// <param name="ic">true if the initial buffer does not need to be copied, otherwise if false the initial buffer is duplicated</param>
        public GapSourceText(char[] buf, int len, bool ic)
        {   
            if (len < 0)
                len = buf.Length;
            size = Math.Max(INITIAL_SIZE, len);
            if (ic)
                body = buf;
            else 
            {
                body = new char[size];
                Array.Copy(buf, body, len);
            }
            length = len;
            TabWidth = DEFAULT_TAB_WIDTH;
        }

        /// <summary>
        /// Update the Gap indices.
        /// </summary>
        /// <param name="l"></param>
        private void Update(int l)
        {
            part1len = l;
            gaplen = size - length;
            part2body = gaplen;
            part2len = length - part1len;
            body2 = part2body + part1len;
        }

        /// <summary>
        /// Move beyond the Gap from a position
        /// </summary>
        /// <param name="to">target position</param>
        protected void MoveGap(int to)
        {
            if (to == part1len)
            return;

            if (part1len > to)
                Array.Copy(body, to, body, to + gaplen, part1len - to);
            else
                Array.Copy(body , part1len + gaplen, body , part1len, to - part1len);
            part1len= to; 
        }            

        /// <summary>
        /// Internal copy method
        /// </summary>
        /// <param name="dst">The destination of the copy</param>
        /// <param name="index">Index of the target destination</param>
        /// <param name="from">The start offset location</param>
        /// <param name="to">The end offset location</param>
        private void CopyTo(char[] dst, int index, int from, int to)
        {
            // Copy Text between 'from' and 'to' to 'dst+index'

            int beforegap = Math.Max(0, part1len - from) - Math.Max(0, part1len - to);
            int aftergap = Math.Max(0, to - part1len) - Math.Max(0, from - part1len);

            if (beforegap != 0)
                Array.Copy(body, from, dst, index, beforegap);
            if (aftergap != 0)
                Array.Copy(body, part2body + Math.Max(part1len, from), dst, index + beforegap, aftergap);
        }

        /// <summary>
        /// Expand
        /// </summary>
        /// <param name="to"></param>
        /// <param name="moveto"></param>
        private void Expand(int to, int moveto)
        { 
            // Expand size of Text to 'to'
            char[] pos;
            int part2;

            if (to == 0)
                to = size * 2;

            if (to < size) 
                return;

            if (moveto > length)
                moveto= length;

            size = to;
            pos = new char[size];
            part2= size - length;

            if (moveto < part1len) 
            {
                Array.Copy(body, pos, moveto);
                //if ((part1len - moveto) > 0)
                    Array.Copy(body, moveto, pos, part2 + moveto, part1len - moveto);
                //if ((length - part1len) > 0)
                    Array.Copy(body, part2body + part1len, pos, part2 + part1len, length - part1len);
            } else 
            {
                Array.Copy(body, pos, part1len);
                //if ((moveto - part1len) > 0)
                    Array.Copy(body, part2body + part1len, pos, part1len, moveto - part1len);
                //if ((length - moveto) > 0)
                    Array.Copy(body, part2body + moveto, pos, part2 + moveto, length - moveto);
            }
            body = pos;
            Update(moveto);
        }

        /// <summary>
        /// Reduce size of Text to 'to'
        /// If to = 0 then size = size / SHRINK_FACTOR + 1
        /// </summary>
        /// <param name="to"></param>
        private void Shrink(int to = 0)
        { 
            char[] pos;

            if ((to == 0) || (to < length))
            to= size / SHRINK_FACTOR + 1;

            if ( (to > size) || (to < length) ) 
            return;

            size = to;

            pos = new char[size];

            Array.Copy(body, pos, part1len);
            Array.Copy(body, part1len + gaplen, pos, part1len, length - part1len);

            body = pos;

            Update(length);
        }

        /// <summary>
        /// Are we going beyond the buffer limit if we add n characters ?
        /// </summary>
        /// <param name="n">The count of characters to be added</param>
        /// <returns>true if we go beyond the buffer limit, false otherwise</returns>
        private bool HighWaterMark(int n)
        { 
            return (bool)(body == null || gaplen <= n); 
        }

        /// <summary>
        /// Have we reach a the  minimal suitable ratio of content size?
        /// </summary>
        /// <returns>true if yes, false otherwise</returns>
        private bool LowWaterMark()
        { 
            return (bool)(size / 5 > length); 
        }

        /// <summary>
        /// Insert the content of a SourceText from a position up to a position.
        /// </summary>
        /// <param name="text">The SourceText to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public override void Insert(SourceText paste,int from,int to)
        {
            if (!CheckRange(length,from,to))
                return;

            GapSourceText ft;
            char[] buf = null;

            if (!(paste is GapSourceText)) 
            {  // convert the text into a GapText
                int s = paste.Size;
                buf = new char[s];
                paste.CopyInStr(buf, s, 0, s);
                ft = new GapSourceText(buf, s, true);
            } else
                ft = (GapSourceText)paste;

            int shift= ft.length - (to - from);

            if ((to - from) > 0)
                Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));     

            if (HighWaterMark(shift))
                Expand(GrowBy(size + shift), from);
            else
                MoveGap(from);

            ft.CopyTo(body, from, 0, ft.length);

            length    += shift;
            gaplen    -= shift;
            part1len  += ft.length;
            part2body -= shift;
            part2len= length - part1len;
            body2= part2body + part1len;

            if (LowWaterMark())
                Shrink();   
            base.Insert(paste, from, to);
            base.Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, paste.Size), paste);
        }

        /// <summary>
        /// Insert the content of a String from a position up to a position.
        /// </summary>
        /// <param name="text">The String to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public override void Insert(String text, int from, int to)
        {
            if (!CheckRange(length, from, to))
                return;            

            int shift = text.Length - (to - from);

            if ((to - from) > 0)
                Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));     

            if (HighWaterMark(shift))
                Expand(GrowBy(size + shift), from);
            else
                MoveGap(from);

            char[] buffer = text.ToCharArray();
            Array.Copy(buffer, 0, body, from, buffer.Length);

            length += shift;
            gaplen -= shift;
            part1len += buffer.Length;
            part2body -= shift;
            part2len = length - part1len;
            body2 = part2body + part1len;

            if (LowWaterMark())
                Shrink();
            base.Insert(text, from, to);
            base.Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, text.Length), text);
        }

        /// <summary>
        /// Insert the content of an array of characters from a position up to a position.
        /// </summary>
        /// <param name="buffer">The array of characters to insert</param>
        /// <param name="from">The start location</param>
        /// <param name="to">The end location</param>
        public override void Insert(char[] buffer, int from, int to)
        {
            if (!CheckRange(length, from, to))
                return;

            int shift = buffer.Length - (to - from);

            if ((to - from) > 0)
                Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));

            if (HighWaterMark(shift))
                Expand(GrowBy(size + shift), from);
            else
                MoveGap(from);

            Array.Copy(buffer, 0, body, from, buffer.Length);

            length += shift;
            gaplen -= shift;
            part1len += buffer.Length;
            part2body -= shift;
            part2len = length - part1len;
            body2 = part2body + part1len;

            if (LowWaterMark())
                Shrink();
            base.Insert(buffer, from, to);
            base.Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, buffer.Length), buffer);
        }

        /// <summary>
        /// Delete a portion of text
        /// </summary>
        /// <param name="from">The start offset</param>
        /// <param name="to">and the end offset</param>
        public override void Delete(int from,int to)
        {
            int shift= to - from;

            if (!CheckRange(length, from, to))
                return;

            base.Send(new TextChangeInfo(TextChanges.TextAboutDeleted, from, to));     
            
            if (from <= part1len && to >= part1len) // Gap is in between
                part1len= from;
            else  
                MoveGap(from); 
            length -= shift;
            gaplen += shift;
            part2body += shift;
            part2len= length - part1len;
            body2= part2body + part1len;

            if (LowWaterMark())
                Shrink();

            base.Delete(from,to);   
            base.Send(new TextChangeInfo(TextChanges.TextDeleted, from, to));     
        }

        /// <summary>
        /// Copy a portion of source text into a target SourceText instance
        /// </summary>
        /// <param name="target"></param>
        /// <param name="from">Start offste of the portion</param>
        /// <param name="to">End offset of the portion</param>
        public override void Copy(SourceText target, int from, int to)
        {
            if (!CheckRange(length, from, to) || (target == null))
                return;

            if (!(target is GapSourceText))
            { // convert
                int s = to - from;
                char[] buf = new char[s + 1];
                CopyTo(buf, 0, from, to);
                buf[to - from] = '\0';
                target.ReplaceWithStr(buf, s);
                return;
            }

            GapSourceText ft = (GapSourceText)target;
            int nSize = to - from;

            ft.Empty();

            if (ft.size < nSize)
                ft.Expand(nSize, 0);

            CopyTo(ft.body, 0, from, to);

            ft.length = to - from;
            ft.Update(to - from);
        }

        /// <summary>
        /// Copy a portion of text from this SourceText to a given buffer.
        /// </summary>
        /// <param name="buffer">The buffer in to which to copy a portion of text</param>
        /// <param name="length">The buffer's length</param>
        /// <param name="from">The start offset of the portion</param>
        /// <param name="to">The end offset of the portion</param>
        public override void CopyInStr(char[] buffer, int strsize, int from, int to)
        {
            if (!CheckRange(length, from, to) || (buffer == null))
                return;
            to = Math.Min(to, from + strsize);
            CopyTo(buffer, 0, from, to);
        }

        /// <summary>
        /// Copy The content of a given buffer in to this Source Text,
        /// The content of the buffer replaces the content of this SourceText.
        /// </summary>
        /// <param name="buffer">The buffer which contains the text to copy</param>
        /// <param name="count">The count of characters to copy</param>
        public override void ReplaceWithStr(char[] buffer, int count)
        {
            if (count < 0)
                count = buffer.Length;
            Empty();
            if (size < count)
                Expand(count, 0);
            Array.Copy(buffer, body, count);
            length = count;
            Update(count);
        }

        /// <summary>
        /// Get the text of a portion
        /// </summary>
        /// <param name="from">The Start position of the portion of text to retrieve</param>
        /// <param name="to">The End position of the portion of text to retrieve</param>
        /// <returns>The String of the portion</returns>
        public override string GetTextAt(int from, int to)
        {
            if (!CheckRange(length, from, to))
                return null;

            if ((part1len < to) && (part1len >= from))
                // Gap is in between 
                if (part1len > ((to + from) >> 1))
                    MoveGap(to);
                else
                    MoveGap(from);

            if (part1len <= from)
                return new string(body, part2body + from, to - from);
            return new string(body, from, to - from);
        }

        /// <summary>
        /// Save a text portion of this SourceText in another SourceText
        /// </summary>
        /// <param name="from">The start position of the portion</param>
        /// <param name="to">The end position of the portion</param>
        /// <returns>The SourceText containing the portion</returns>
        public override SourceText Save(int from, int to)
        {
            if (!CheckRange(length, from, to))
                return null;

            GapSourceText t = new GapSourceText(to - from);
            Copy(t, from, to);
            return t;
        }

        /// <summary>
        /// Insert one character in this SourceText from a location to another location.
        /// </summary>
        /// <param name="c">The character to insert</param>
        /// <param name="from">The start offset offset the insert location</param>
        /// <param name="to">The end offset of the insertion location.</param>
        public override void Insert(char c, int from, int to)
        {
            int shift = 1 - (to - from);

            if (!CheckRange(length,from,to))
                return;

            if (HighWaterMark(shift))
                Expand(GrowBy(size+shift), from);
            else     
                MoveGap(from);
            body[from] = c;

            length    += shift;
            gaplen    -= shift;
            part1len  += shift;
            part2body -= shift;
            part2len = length - part1len;
            body2 = part2body + part1len;

            if (LowWaterMark())
                Shrink();

            base.Insert(c, from, to);
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, 1), new char[]{c});     
        }

        /// <summary>
        /// Reset this Source Text to an empty content with given size capacity.
        /// </summary>
        /// <param name="initSize">The reset capacity</param>
        public override void Empty(int initSize = 0)
        {
            if (initSize > 0)
            {
                body = new char[size = initSize];
            }
            length = 0;
            Update(0);
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
                i = Math.Min(i, length - 1);
                if (i < part1len)
                    return body[i];
                return body[part2body + i];
            }
            set
            {
                i = Math.Min(i, length - 1);
                if (i < part1len)
                    body[i] = value;
                else
                    body[part2body + i] = value;
            }
        }

        /// <summary>
        /// Get the size of this Source Text
        /// </summary>
        /// <returns>The Size</returns>
        public override int Size
        {
            get 
            { 
                return length; 
            }
        }

        /// <summary>
        /// Write the content of this SourceText into a TextWriter
        /// </summary>
        /// <param name="writer">The TextWriter instance</param>
        public override void Write(TextWriter writer)
        {
            writer.Write(body, 0, part1len);
            int afterGap = part1len + gaplen;
            writer.Write(body, afterGap, length - part1len);
        }

        /// <summary>
        /// Write the content of this SourceText into a StringBuilder
        /// </summary>
        /// <param name="writer">The StringBuilder instance</param>
        public override void Write(StringBuilder stringBuilder)
        {
            stringBuilder.Append(body, 0, part1len);
            int afterGap = part1len + gaplen;
            stringBuilder.Append(body, afterGap, length - part1len);
        }

        /// <summary>
        /// Set it to true if the gap must be showned.
        /// </summary>
        static readonly bool dump_show_gap = false;
        /// <summary>
        /// Dump the The Gar Source text informations and content.
        /// <param name="allChars">Tru eif even non printable chars must be dumped, false otherwise</param>
        /// </summary>
        public override void Dump(bool allChars = true)
        {   
            int i;
            System.Console.Write("size : "); System.Console.Write(size); System.Console.WriteLine();
            System.Console.Write("length  : "); System.Console.Write(length); System.Console.WriteLine();
            System.Console.Write("part1len     : "); System.Console.Write(part1len); System.Console.WriteLine();
            System.Console.Write("gaplen  : "); System.Console.Write(gaplen); System.Console.WriteLine();
            System.Console.Write("part2body-body    : "); System.Console.Write(part2body); System.Console.WriteLine();
            System.Console.WriteLine("body  : ");
            for (i=0 ; i < size ; i++)
            {
                if (dump_show_gap)
                {
                    if (i == part1len)
                        System.Console.Write("[");
                    if (i == (part1len + gaplen))
                        System.Console.Write("]");
                }
                else if (i >= part1len && i <= (part1len + gaplen))
                    continue;//ignore gap content
                if ((body[i]>= ' ') && (body[i] <='~'))
                    System.Console.Write(body[i]);
                else
                    System.Console.Write(allChars ? body[i] : '.');
            }
            System.Console.WriteLine();
        }
    }
}
