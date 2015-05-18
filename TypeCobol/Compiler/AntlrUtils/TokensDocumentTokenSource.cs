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
    /// Implementation of the Antlr ITokenSource interface on top of a TokensDocument
    /// </summary>
    public class TokensDocumentTokenSource : ITokenSource
    {
        private ITextDocument textDocument;
        private ITokensLinesIterator tokensIterator;       

        public TokensDocumentTokenSource(ITextDocument textDocument, ITokensLinesIterator tokensIterator)
        {
            this.textDocument = textDocument;
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
                int currentOffset = tokensIterator.Offset;
                return new TextDocumentCharStream(textDocument, currentOffset); 
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
            nextToken.SetAntlrSource(Tuple.Create<ITokenSource, ICharStream>(this, this.InputStream));
            return nextToken;
        }

        public string SourceName
        {
            get 
            { 
                return textDocument.FileName; 
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
            protected internal static readonly Tuple<ITokenSource, ICharStream> EmptySource = Tuple.Create<ITokenSource, ICharStream>(null, null); 

            public IToken Create(int type, string text)
            {
                if (text == null) text = String.Empty;
                Token token = new Token((TokenType)type, 0, text.Length - 1, new TextLine(0, -1, text));
                token.SetAntlrSource(EmptySource);
                SetTokenLiteralValue(token, text);
                return token;
            }

            public IToken Create(Tuple<ITokenSource, ICharStream> source, int type, string text, int channel, int start, int stop, int line, int charPositionInLine)
            {
                if (text == null) text = String.Empty;
                Token token = new Token((TokenType)type, 0, text.Length - 1, new TextLine(line - 1, -1, text));
                token.SetAntlrSource(source);
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
