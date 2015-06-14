using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Parser
{
    internal class TracingCobolParser : CobolCodeElementsParser
    {
        public TracingCobolParser(ITokenStream input) : base(input)
        {
            ResetTraces();
        }

        /// <summary>
        /// Shortcut method : current line index in the main document being parsed
        /// </summary>
        /// <returns></returns>
        protected int GetCurrentLineIndexInMainDocument()
        {
            ITokensLinesIterator tokensLinesIterator = ((TokensDocumentTokenSource)((ITokenStream)InputStream).TokenSource).TokensIterator;
            return tokensLinesIterator.LineIndexInMainDocument;
        }

        /// <summary>
        /// Registers all tokens consumed while matching a parser rule.
        /// Reset this list between two invocations of a root parser rule.
        /// </summary>
        public IList<Token> ConsumedTokens { get; private set; }
        
        /// <summary>
        /// Line index in the main document where the first consumed token is starting
        /// </summary>
        public int FirstTokenLineIndexInMainDocument { get; private set; }

        /// <summary>
        /// Line index in the main document where the last consumed token is starting
        /// </summary>
        public int LastTokenLineIndexInMainDocument { get; private set; }

        /// <summary>
        /// Call this method to reset the traces between two invocations of a root parser rule
        /// </summary>
        public void ResetTraces()
        {
            ConsumedTokens = new List<Token>();
            FirstTokenLineIndexInMainDocument = -1;
            LastTokenLineIndexInMainDocument = -1;
        }

        /// <summary>
        /// OVERRIDE the base parser Consume method
        /// </summary>
        public override IToken Consume()
        {
            Token consumedToken = (Token)base.Consume();

            // Trace token consumption
            if (consumedToken.TokenType != TokenType.EndOfFile)
            {
                ConsumedTokens.Add(consumedToken);
                if(FirstTokenLineIndexInMainDocument < 0)
                {
                    FirstTokenLineIndexInMainDocument = GetCurrentLineIndexInMainDocument();
                }
                LastTokenLineIndexInMainDocument = GetCurrentLineIndexInMainDocument();
            }

            return consumedToken;
        }
    }
}
