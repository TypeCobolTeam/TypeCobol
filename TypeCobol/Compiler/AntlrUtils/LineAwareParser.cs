using Antlr4.Runtime;
using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    public abstract class LineAwareParser : Antlr4.Runtime.Parser
    {
        public LineAwareParser(ITokenStream input) : base(input)
        { }

        public bool IsNextTokenOnTheSameLine()
        {
            Token previousToken = (Token)_input.Lt(-1);
            Token currentToken = (Token)CurrentToken;
            
            
            return previousToken != null && currentToken != null &&
                   previousToken.TokensLine == currentToken.TokensLine;
        }
    }
}
