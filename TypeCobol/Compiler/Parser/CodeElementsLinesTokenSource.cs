using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Implementation of the Antlr ITokenSource interface on top of a ISearchableReadOnlyList<ICodeElementsLine>
    /// </summary>
    public class CodeElementsLinesTokenSource : ITokenSource
    {
        private string sourceFileName;
        private ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines;       

        public CodeElementsLinesTokenSource(string sourceFileName, ISearchableReadOnlyList<ICodeElementsLine> codeElementsLines)
        {
            this.sourceFileName = sourceFileName;
            this.codeElementsLines = codeElementsLines;
            this.codeElementsLinesEnumerator = codeElementsLines.GetEnumerator();
        }

        IEnumerator<ICodeElementsLine> codeElementsLinesEnumerator;
        int currentCodeElementIndexInLine = -1;
        CodeElement currentToken = null;

        public int Column
        {
            get 
            {
                if (currentToken != null)
                {
                    return currentToken.Column;
                }
                else
                {
                    return 0;
                }
            }
        }

        public ICharStream InputStream
        {
            get 
            {
                if (currentToken != null)
                {
                    return currentToken.InputStream;
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
                if (currentToken != null)
                {
                    return currentToken.Line;
                }
                else
                {
                    return 0;
                }
            }
        }

        public IToken NextToken()
        {
            if(currentCodeElementIndexInLine >= 0)
            {
                currentCodeElementIndexInLine++;
                if(currentCodeElementIndexInLine < codeElementsLinesEnumerator.Current.CodeElements.Count)
                {
                    currentToken = codeElementsLinesEnumerator.Current.CodeElements[currentCodeElementIndexInLine];
                    return currentToken;
                }
                else
                {
                    currentCodeElementIndexInLine = -1;
                }
            }
            
            if (currentCodeElementIndexInLine < 0)
            {
                while(codeElementsLinesEnumerator.MoveNext())
                {
                    if(codeElementsLinesEnumerator.Current.CodeElements != null &&
                        codeElementsLinesEnumerator.Current.CodeElements.Count > 0)
                    {
                        currentCodeElementIndexInLine = 0;
                        currentToken = codeElementsLinesEnumerator.Current.CodeElements[currentCodeElementIndexInLine];
                        return currentToken;
                    }
                }
            }

            return Token.END_OF_FILE;
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
