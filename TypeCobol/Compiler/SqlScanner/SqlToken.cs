using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.SqlScanner
{
    public class SqlToken : Token
    {
        public SqlToken()
        {
            
        }

        public SqlToken(SqlTokenType tokenType, int startIndex, int stopIndex, ITokensLine tokensLine)
            : base((TokenType)tokenType, startIndex, stopIndex, tokensLine)
        {
        }

        public override string ToString()
        {
            return Text;
        }
    }
}
