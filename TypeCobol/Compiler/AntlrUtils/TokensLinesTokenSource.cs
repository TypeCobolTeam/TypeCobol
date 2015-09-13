using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Implementation of the Antlr ITokenSource interface on top of a ITokensLinesIterator
    /// </summary>
    public class TokensLinesTokenSource : ITokenSource
    {
        private string sourceFileName;
        private ITokensLinesIterator tokensIterator;       

        public TokensLinesTokenSource(string sourceFileName, ITokensLinesIterator tokensIterator)
        {
            this.sourceFileName = sourceFileName;
            this.tokensIterator = tokensIterator;
        }

        public ITokensLinesIterator TokensIterator
        {
            get { return tokensIterator; }
        }

        public int Column
        {
            get 
            {
                return tokensIterator.ColumnIndex;
            }
        }

        public ICharStream InputStream
        {
            get 
            {
                Token currentToken = tokensIterator.CurrentToken;
                if (currentToken != null)
                {
                    ITextLine currentTextLine = currentToken.TokensLine;
                    return new TextLineCharStream(currentTextLine);
                }
                else
                {
                    return null;
                }
            }
        }

        public int Line
        {
            get
            {
                return tokensIterator.LineIndex + 1;
            }
        }

        public IToken NextToken()
        {
            Token nextToken = tokensIterator.NextToken();
            nextToken.SetAntlrSource(this);
            return nextToken;
        }

        public string SourceName
        {
            get 
            { 
                return sourceFileName; 
            }
        }

        private ITokenFactory _tokenFactory = new CobolTokenFactory();

        public ITokenFactory TokenFactory
        {
            get
            {
                return _tokenFactory;
            }
            set
            {
                _tokenFactory = value;
            }
        }

        private class CobolTokenFactory : ITokenFactory
        {
            public IToken Create(int type, string text)
            {
                if (text == null) text = String.Empty;
                Token token = new Token((TokenType)type, 0, text.Length - 1, TokensLine.CreateVirtualLineForInsertedToken(-1, text));
                token.SetAntlrSource(null);
                SetTokenLiteralValue(token, text);
                return token;
            }

            public IToken Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
            {
                if (text == null) text = String.Empty;
                Token token = new Token((TokenType)type, 0, text.Length - 1, TokensLine.CreateVirtualLineForInsertedToken(line -1, text));
                token.SetAntlrSource(source.Item1);
                token.Channel = channel;
                SetTokenLiteralValue(token, text);
                return token;
            }

            private void SetTokenLiteralValue(Token token, string text)
            {
               if(token.TokenFamily == TokenFamily.AlphanumericLiteral)
               {
                   token.LiteralValue = new AlphanumericLiteralValue(text);
               }
               else if(token.TokenType == TokenType.IntegerLiteral)
               {
                   string sign = null;
                   if(text[0] == '+' || text[0] == '-')
                   {
                       sign = text.Substring(0,1);
                       text = text.Substring(1);
                   }
                   token.LiteralValue = new IntegerLiteralValue(sign, text);
               }
               else if (token.TokenType == TokenType.DecimalLiteral || token.TokenType == TokenType.FloatingPointLiteral)
               {
                   throw new NotImplementedException();
               }
            }
        }
    }    
}
