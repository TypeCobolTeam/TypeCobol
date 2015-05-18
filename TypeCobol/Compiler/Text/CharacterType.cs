using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Text
{
    public enum CharacterType
    {
        // Common character type
        BasicCobolChar, 
        // EBCDIC character types
        SingleByteChar, 
        DBCSShiftOut, 
        DBCSShiftIn, 
        DBCSChar, 
        // Non EBCDIC character type
        UnicodeChar,
        // Special character types
        NewLine, 
        EndOfFile
    }
}
