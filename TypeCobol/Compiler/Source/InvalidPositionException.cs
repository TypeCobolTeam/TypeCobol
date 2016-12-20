using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// Invalid Position Exception
    /// </summary>
    public class InvalidPositionException : Exception
    {
        /// <summary>
        /// The Invalid location
        /// </summary>
        public int At
        {
            get;
            private set;
        }

        /// <summary>
        /// 
        /// </summary>
        public int Length
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="msg">Message</param>
        /// <param name="at">offset of the location</param>
        /// <param name="len">length of the location</param>
        public InvalidPositionException(string msg, int at, int len) : base(msg)
        {
            At = at;
            Length = len;
        }
    }
}

