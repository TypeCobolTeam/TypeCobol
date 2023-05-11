using System;
using System.Collections.Generic;
using System.Text;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// Class that represents a list of Position
    /// </summary>
    public class PositionList : List<Position>
    {
        /// <summary>
        /// Flag to indicate if deleted positions should be removed.
        /// </summary>
        private bool DoRemove
        {
            get;
            set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="doRemove">True if deleted position should be tracked and removed</param>
        public PositionList(bool doRemove = false)
        {
            DoRemove = doRemove;
        }

        /// <summary>
        /// Insert a given length of characters at the given offset
        /// </summary>
        /// <param name="at">The offset</param>
        /// <param name="len">The length</param>
        public void Insert(int offset, int len)
        {
            len = Math.Max(0, len);
            offset = Math.Max(0, offset);
            foreach (Position m in this)
            {
                if (m.TestFlag(Position.Flags.Locked))
                    continue;//This an locked position which cannot move
                if ((offset > m.Pos || (m.TestFlag(Position.Flags.InclStart) && offset == m.Pos))
                                        && offset < m.Pos + m.Len)
                {
                    m.State = Position.States.Changed;
                    m.Len += len;
                }
                else if (offset < m.Pos || (!m.TestFlag(Position.Flags.InclStart) && offset == m.Pos))
                {
                    m.Pos += len;
                }
            }
        }

        /// <summary>
        /// Delete n characters at the given location
        /// </summary>
        /// <param name="at">The location</param>
        /// <param name="n">length of characters to delete</param>
        public void Delete(int at, int n)
        {
            if (at < 0)
                n += at;
            at = Math.Max(0, at);
            List<Position> toRemove = null; //List of position to remove.

            try
            {
                foreach (Position m in this)
                {

                    if (m.TestFlag(Position.Flags.Locked))
                        continue;

                    if (m.TestFlag(Position.Flags.FixedSize))
                    {
                        if (at <= m.Pos && at + n >= m.Pos + m.Len)
                        {
                            m.State = Position.States.Deleted;
                            m.Len = 0;
                            m.Pos = at;
                            if (DoRemove)
                            {
                                if (toRemove == null)
                                    toRemove = new List<Position>();
                                toRemove.Add(m);
                            }
                        }
                        else if (at + n <= m.Pos)
                        {
                            m.Pos -= n;
                        }

                        else if (at >= m.Pos + m.Len)
                        {//Nothing todo
                        }
                        else
                        {
                            throw new InvalidPositionException("Invalid Delete location", at, n);
                        }
                    }
                    else
                    {
                        if (at >= m.Pos && at + n <= m.Pos + m.Len)
                        { // contained
                            m.State = Position.States.Changed;
                            m.Len -= n;
                        }

                        else if (at < m.Pos && at + n >= m.Pos + m.Len)
                        {
                            m.State = Position.States.Deleted;
                            m.Len = 0;
                            m.Pos = at;
                            if (DoRemove)
                            {
                                if (toRemove == null)
                                    toRemove = new List<Position>();
                                toRemove.Add(m);
                            }
                        }

                        else if (at < m.Pos && at + n > m.Pos)
                        {
                            m.Len = m.Pos + m.Len - (at + n);
                            m.Pos = at;
                            m.State = Position.States.Changed;
                        }

                        else if (at >= m.Pos && at < m.Pos + m.Len)
                        {
                            m.Len = at - m.Pos;
                            m.State = Position.States.Changed;
                        }

                        else if (at + n <= m.Pos)
                        {
                            m.Pos -= n;
                        }

                        else if (at >= m.Pos + m.Len)
                        {//Nothing todo                         
                        }
                        else
                            throw new InvalidPositionException("Invalid Delete location", at, n);
                    }
                }
            }
            finally
            {
                if (toRemove != null)
                {   //Remove possitions to remove
                    foreach (Position m in toRemove)
                    {
                        base.Remove(m);
                    }
                }
            }
        }

        /// <summary>
        /// Propagate a Range changed of n characters at the given location, that is to say mark this position
        /// as has been damaged because it is included in the changed range.
        /// </summary>
        /// <param name="at">The location</param>
        /// <param name="n">The count of characters</param>
        public void RangeChanged(int at, int n)
        {
            n = Math.Max(0,n);
            at = Math.Max(0,at);

            foreach(Position m in this) {
                if (m.TestFlag(Position.Flags.Locked))
                    continue;
                if (at < m.Pos && at + n >= m.Pos + m.Len)
                    m.State = Position.States.Changed;
                else if (at >= m.Pos && at < m.Pos+m.Len)
                    m.State = Position.States.Changed;
                else if (at + n >= m.Pos && at + n < m.Pos+m.Len)
                    m.State = Position.States.Changed;
            }
        }

        /// <summary>
        /// Displays a String representation of this Position List
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder("PositionList{");
            string sep = "";
            foreach (Position m in this)
            {
                sb.Append(sep);
                sb.Append(m.ToString());
                sep = "\n\t";
            }
            sb.Append("}");
            return sb.ToString();
        }

        /// <summary>
        /// Dump the Position List
        /// </summary>
        public void Dump()
        {
            System.Console.WriteLine(ToString());
        }
    }
}
