using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The program-name of a program is specified in the PROGRAM-ID paragraph of the program's IDENTIFICATION DIVISION. 
    /// A program-name can be referenced only by the CALL statement, the CANCEL statement, the SET statement, or the END PROGRAM marker.
    /// </summary>
    public class ProgramName : Symbol
    {
        public ProgramName(Token userDefinedWordOrAlphanumericLiteral) :
            base(userDefinedWordOrAlphanumericLiteral, SymbolType.ProgramName)
        { }
    }
}
