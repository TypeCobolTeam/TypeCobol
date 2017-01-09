using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Source
{
    /// <summary>
    /// Represents a moveable position inside a source code.
    /// </summary>
    public class Position : IComparable<Position>, ICloneable
    {
        /// <summary>
        /// Constants passed to the listeners of a position
        /// </summary>
        public enum Changes : byte
        {
            Node = 0,
            Deleted = 1,    //Position is deleted
            Length = 2,     // Text in position range changed
            Pos = 3         // Position of the mark changed
        };

        /// <summary>
        /// State of the Position
        /// </summary>
        public enum States : byte
        {
            None = 0,
            Deleted = 1,
            Changed = 2
        };

        public enum Flags : byte
        {
            InclStart = 0x01,//Shall the start position be include in range
            FixedSize = 0x01 << 1,
            Locked = 0x01 << 2, //The position won't move
            Default = 0,
            Last = 3    //Last shift index
        };

        /// <summary>
        /// Position of the mark in the source code
        /// </summary>
        public int Pos
        {
            get; 
            set;
        }

        /// <summary>
        /// The length of the position
        /// </summary>
        public int Len
        {
            get;
            set;
        }

        /// <summary>
        /// The State of the position
        /// </summary>
        public States State
        {
            get;
            set;
        }

        /// <summary>
        /// Internal flags
        /// </summary>
        private byte flags;

        public void SetFlag(Flags f)
        { 
            flags |= (byte)((uint)f & 0xFF); 
        }
        public void ResetFlag(Flags f)
        {
            flags &= (byte)(~((uint)f & 0xFF)); 
        }
        public bool TestFlag(Flags f)
        { 
            return (bool)(((byte)flags & (byte)f) != 0); 
        }
        public void InvertFlag(Flags f)
        {
            flags ^= (byte)((byte)flags & (byte)((ushort)f & 0xFF)); 
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="pos">The position</param>
        /// <param name="len">The length</param>
        /// <param name="state">The state</param>
        /// <param name="flags">flags</param>
        public Position(int pos = 0, int len = 0, States state = States.None, Flags flags = Flags.Default)
        {
            Pos = pos;
            Len = len;
            State = state;
            if ((flags & Flags.InclStart) == Flags.InclStart)
                SetFlag(Flags.InclStart);
            if ((flags & Flags.FixedSize) == Flags.FixedSize)
                SetFlag(Flags.FixedSize);
            if ((flags & Flags.Locked) == Flags.Locked)
                SetFlag(Flags.Locked);
        }

        /// <summary>
        /// Copy Constructor
        /// </summary>
        /// <param name="p">The position to copy from</param>
        public Position(Position p)
        {
            Pos = p.Pos;
            Len = p.Len;
            State = p.State;
        }

        /// <summary>
        /// Comparison method
        /// </summary>
        /// <param name="other">The Position to compare to</param>
        /// <returns> the result of the Pos value comparison</returns>
        public int CompareTo(Position other)
        {
            return Pos - other.Pos;
        }

        /// <summary>
        /// Determine if the Position has changed from a given location
        /// </summary>
        /// <param name="start">The starting offset of the location to be comapred with</param>
        /// <param name="l">The length of the location</param>
        /// <returns>True if this position has changed from the given location, false otherwise</returns>
        public bool HasChanged(int start, int l)
        {
            return (State != States.None || Pos != start || Len != l);
        }

        /// <summary>
        /// String reprsentation of the Position
        /// </summary>
        /// <returns>The string representation</returns>
        public override string ToString()
        {
            return "Position(Pos = " + Pos + ", len = " + Len + ", State : " + State.ToString() +")";
        }

        public object Clone()
        {
            return new Position(Pos, Len, State);
        }
    }
}
