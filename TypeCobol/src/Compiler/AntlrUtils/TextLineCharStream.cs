using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Implementation of AntlrInputStream on top of the ITextLine interface
    /// </summary>
    internal class TextLineCharStream : ICharStream
    {
        /// <summary>The data being scanned</summary>
        protected internal ITextLine data;

        /// <summary>How many characters are actually in the buffer</summary>
        protected internal int n;

        /// <summary>0..n-1 index into string of next char</summary>
        protected internal int p = 0;

        /// <summary>What is name or source of this char stream?</summary>
        public string name;

        /// <summary>Reference immutable text line char array</summary>
        public TextLineCharStream(ITextLine line)
        {
            data = line;
            n = line.Length;
        }

        /// <summary>
        /// Reset the stream so that it's in the same state it was
        /// when the object was created *except* the data array is not
        /// touched.
        /// </summary>
        /// <remarks>
        /// Reset the stream so that it's in the same state it was
        /// when the object was created *except* the data array is not
        /// touched.
        /// </remarks>
        public virtual void Reset()
        {
            p = 0;
        }

        public virtual void Consume()
        {
            if (p >= n)
            {
                System.Diagnostics.Debug.Assert(La(1) == IntStreamConstants.Eof);
                throw new InvalidOperationException("cannot consume EOF");
            }
            if (p < n)
            {
                p++;
            }
        }

        public virtual int La(int i)
        {
            if (i == 0)
            {
                return 0;
            }
            // undefined
            if (i < 0)
            {
                i++;
                // e.g., translate LA(-1) to use offset i=0; then data[p+0-1]
                if ((p + i - 1) < 0)
                {
                    return IntStreamConstants.Eof;
                }
            }
            // invalid; no char before first char
            if ((p + i - 1) >= n)
            {
                return IntStreamConstants.Eof;
            }
            return data.Text[p + i - 1];
        }

        public virtual int Lt(int i)
        {
            return La(i);
        }

        /// <summary>
        /// Return the current input symbol index 0..n where n indicates the
        /// last symbol has been read.
        /// </summary>
        /// <remarks>
        /// Return the current input symbol index 0..n where n indicates the
        /// last symbol has been read.  The index is the index of char to
        /// be returned from LA(1).
        /// </remarks>
        public virtual int Index
        {
            get
            {
                return p;
            }
        }

        public virtual int Size
        {
            get
            {
                return n;
            }
        }

        /// <summary>mark/release do nothing; we have entire buffer</summary>
        public virtual int Mark()
        {
            return -1;
        }

        public virtual void Release(int marker)
        {
        }

        /// <summary>
        /// consume() ahead until p==index; can't just set p=index as we must
        /// update line and charPositionInLine.
        /// </summary>
        /// <remarks>
        /// consume() ahead until p==index; can't just set p=index as we must
        /// update line and charPositionInLine. If we seek backwards, just set p
        /// </remarks>
        public virtual void Seek(int index)
        {
            if (index <= p)
            {
                p = index;
                // just jump; don't update stream state (line, ...)
                return;
            }
            // seek forward, consume until p hits index or n (whichever comes first)
            index = Math.Min(index, n);
            while (p < index)
            {
                Consume();
            }
        }

        public virtual string GetText(Interval interval)
        {
            int start = interval.a;
            int stop = interval.b;
            if (stop >= n)
            {
                stop = n - 1;
            }
            int count = stop - start + 1;
            if (start >= n)
            {
                return string.Empty;
            }
            return data.TextSegment(start, stop);
        }

        public virtual string SourceName
        {
            get
            {
                if (string.IsNullOrEmpty(name))
                {
                    return IntStreamConstants.UnknownSourceName;
                }
                return name;
            }
        }

        public override string ToString()
        {
            return data.TextSegment(0, n -1);
        }
    }

}
