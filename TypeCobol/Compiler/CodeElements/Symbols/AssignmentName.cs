using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// assignment-name-1 Identifies the external data file. 
    /// It can be specified as a name or as an alphanumeric literal. 
    /// assignment-name-1 is not the name of a data item, and assignment-name-1 cannot be contained in a data item. 
    /// It is just a character string. It cannot contain an underscore character.
    /// </summary>
    public class AssignmentName : Symbol
    {
        public AssignmentName(Token userDefinedWordOrAlphanumericLiteral) :
            base(userDefinedWordOrAlphanumericLiteral, SymbolType.AssignmentName)
        { }
    }
}
