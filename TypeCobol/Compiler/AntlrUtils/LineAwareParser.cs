using Antlr4.Runtime;
using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    public abstract class LineAwareParser : Antlr4.Runtime.Parser
    {
        protected LineAwareParser(ITokenStream input) : base(input)
        { }

        public bool IsNextTokenOnTheSameLine()
        {            
            Token currentToken = (Token)CurrentToken;
            Token nextToken = (Token)_input.Lt(2);

            return nextToken != null && currentToken != null &&
                   nextToken.TokensLine == currentToken.TokensLine;
        }
    }
}
