using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

        public override void Insert(SourceText paste, int from, int to)
        {
            StringSourceText ct;
            char[] buf = null;

            if (!(paste is StringSourceText)) { // try to convert
	            int s = paste.Size;
	            buf= new char[s];
	            paste.CopyInStr(buf, s, 0, s);
	            ct= new StringSourceText(buf, s);
            } else
	            ct = (StringSourceText)paste; 

            int shift= ct.next - (to - from);

            if (!CheckRange(next, from, to))
	            return;

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
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, paste.Size));     
        }

        public override void Copy(SourceText target, int from, int to)
        {
            if (!CheckRange(next,from,to) || target == null)
	        return;

            if (!(target is StringSourceText)) { // convert
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

        public override void CopyInStr(char[] buffer, int length, int from, int to)
        {
            if (!CheckRange(next, from, to) || buffer == null)
	            return;

            int l = Math.Min(to, from + length) - from;
                Array.Copy(content, from, buffer, 0, l);
            buffer[l]= '\0';
        }

        public override void ReplaceWithStr(char[] buffer, int count)
        {
            if (count == -1)
                count = buffer.Length;
            if (size < count)
                Expand(count);
            Array.Copy(buffer, content, count);
            next = count;
        }

        public override string GetTextAt(int from, int to)
        {
            if (!CheckRange(next, from, to))
                return null;
            return new string(content, from, to - from);
        }

        public override SourceText Save(int from, int to)
        {
            if (!CheckRange(next, from, to))
                return null;
            return new StringSourceText(content, from, to - from);
        }

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
            Send(new TextChangeInfo(TextChanges.TextReplaced, from, to, 1));     
        }

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

        public override void Empty (int initSize)                                         
        {
            next = 0; 
        }

        public override int Size
        {
            get
            {
                return next;
            }
        }

        public override bool Equals(object obj)
        {
            if (!(obj is StringSourceText))
                return false;
            StringSourceText t = (StringSourceText)obj;
            if (next != t.next)
                return false;
            return CompareArrays(content, t.content, next);
        }

        public override int GetHashCode ()
        {
            int hash;
            int i;

            for (hash = 0, i = 0; i < Size; i++)
	            hash= (hash << 1) ^ content[i];
            return hash;
        }

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

        bool HighWaterMark(int n)
        { 
            return (bool)(content == null || next + n >= size); 
        }
        bool LowWaterMark()
        { 
            return (bool)(next < size / 5); 
        }  
    }
}

