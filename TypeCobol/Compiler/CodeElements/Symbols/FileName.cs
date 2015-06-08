using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Must be declared by a file control entry in the ENVIRONMENT DIVISION.
    /// Then must be identified by an FD or SD entry in the DATA DIVISION. 
    /// A file-name must conform to the rules for a COBOL user-defined name, must contain at least one alphabetic character, and must be unique within this program.
    /// </summary>
    public class FileName : Symbol
    {
        public FileName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.FileName)
        { }
    }
}
