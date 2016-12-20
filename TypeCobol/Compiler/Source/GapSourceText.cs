using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
        GapSourceText(char[] buf, int len, bool ic)
        {   
            if (len < 0)
	            len= buf.Length;
            size= Math.Max(INITIAL_SIZE, len);
            if (ic)
	            body = buf;
            else {
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

            if (moveto < part1len) {
	            Array.Copy(body, pos, moveto);
                Array.Copy(body, moveto, body, part2 + moveto, part1len - moveto);
                Array.Copy(body, part2body + part1len, body, part2 + part1len, length - part1len);
            } else {
                Array.Copy(body, pos, part1len);
                Array.Copy(body, part2body + part1len, pos, part1len, moveto - part1len);
                Array.Copy(body, part2body + moveto, body, part2 + moveto, length - moveto);
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

        private bool HighWaterMark(int n)
        { 
            return (bool)(body == null || gaplen <= n); 
        }

        private bool LowWaterMark()
        { 
            return (bool)(size / 5 > length); 
        }

        public override void Insert(SourceText paste,int from,int to)
        {
            if (!CheckRange(length,from,to))
	            return;

            GapSourceText ft;
            char[] buf = null;

            if (!(paste is GapSourceText)) {  // convert the text into a GapText
	            int s = paste.Size;
	            buf = new char[s];
	            paste.CopyInStr(buf, s, 0, s);
	            ft = new GapSourceText(buf, s, true);
            } else
	            ft = (GapSourceText)paste;

            int shift= ft.length - (to - from);

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
            base.Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, paste.Size));
        }

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

        public override void CopyInStr(char[] buffer, int strsize, int from, int to)
        {
            if (!CheckRange(length, from, to) || (buffer == null))
                return;
            to = Math.Min(to, from + strsize);
            CopyTo(buffer, 0, from, to);
        }

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

            if (part1len < from)
                return new string(body, part2body + from, to - from);
            return new string(body, from, to - from);
        }

        public override SourceText Save(int from, int to)
        {
            if (!CheckRange(length, from, to))
                return null;

            GapSourceText t = new GapSourceText(to - from);
            Copy(t, from, to);
            return t;
        }

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
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, 1));     
        }

        public override void Empty(int initSize = 0)
        {
            if (initSize > 0)
            {
                body = new char[size = initSize];
            }
            length = 0;
            Update(0);
        }

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

        public override int Size
        {
            get 
            { 
                return length; 
            }
        }

        public override bool Equals(object text)                                         
        {
            if (!(text is GapSourceText))
	            return false;
            GapSourceText t= (GapSourceText) text; 

            if (length != t.length) 
	            return false;

            MoveGap(length);
            t.MoveGap(t.length);
            return CompareArrays(body, t.body, length);
        }

        public override int GetHashCode()                                         
        {
            int hash;
            int i;

            for (hash= 0, i = 0; i < part1len; i++)
	            hash= (hash << 1) ^ body[i];
            for (i = part1len + gaplen; i < size; i++)
	            hash= (hash << 1) ^ body[i];
            return hash;
        }

        public void Dump()
        {   
            int i;
            System.Console.Write("size : "); System.Console.Write(size); System.Console.WriteLine();
            System.Console.Write("length  : "); System.Console.Write(length); System.Console.WriteLine();
            System.Console.Write("part1len     : "); System.Console.Write(part1len); System.Console.WriteLine();
            System.Console.Write("gaplen  : "); System.Console.Write(gaplen); System.Console.WriteLine();
            System.Console.Write("part2body-body    : "); System.Console.Write(part2body); System.Console.WriteLine();
            System.Console.Write("body  : ");
            for (i=0 ; i < size ; i++){
	        if (i == part1len )
	            System.Console.Write("[");
	        if (i == (part1len + gaplen))
	            System.Console.Write("]");
	        if ((body[i]>= ' ') && (body[i] <='~'))
	            System.Console.Write(body[i]);
	        else
	            System.Console.Write(".");
            }
            System.Console.WriteLine();
        }
    }
}

