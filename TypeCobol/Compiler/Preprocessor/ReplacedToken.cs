using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Token placeholder used to implement the REPLACE and COPY REPLACING compiler directives
    /// in the most common case when a single source token is replaced by a single replacement token.    
    /// </summary>
    public class ReplacedToken : Token
    {
        /// <summary>
        /// Parameter of the REPLACE directive or REPLACING clause of the COPY directive which replaces the original token
        /// </summary>
        public Token ReplacementToken { get; private set; }

        /// <summary>
        /// Original tokens in the source text which was replaced
        /// </summary>
        public Token OriginalToken { get; private set; }

        /// <summary>
        /// Create a token placeholder
        /// </summary>
        public ReplacedToken(Token replacementToken, Token originalToken)
            : base(replacementToken.TokenType, originalToken.StartIndex, originalToken.StopIndex, originalToken.UsesVirtualSpaceAtEndOfLine, originalToken.TokensLine)
        {
            ReplacementToken = replacementToken;
            OriginalToken = originalToken;

            HasError = replacementToken.HasError;
            UsesDelimiters = replacementToken.UsesDelimiters;
            HasOpeningDelimiter = replacementToken.HasOpeningDelimiter;
            HasClosingDelimiter = replacementToken.HasClosingDelimiter;
            ExpectedClosingDelimiter = replacementToken.ExpectedClosingDelimiter;
            LiteralValue = replacementToken.LiteralValue;
        }

        public override string Text
        {
            get
            {
                return ReplacementToken.Text;
            }
        }
    }

    /// <summary>
    /// Token placeholder used to implement the REPLACE and COPY REPLACING compiler directives
    /// when the variable part of a partial Cobol word is replaced by a prefix or suffix.    
    /// </summary>
    public class ReplacedPartialCobolWord : ReplacedToken
    {
        /// <summary>
        /// Parameter of the REPLACE directive or REPLACING clause of the COPY directive which replaces the original token
        /// </summary>
        public Token PartialReplacementToken { get; private set; }

        /// <summary>
        /// Create a token placeholder.
        /// Sample arguments :
        /// originalPartialCobolWordToken = :PREFIX:-USER-NAME (token type = PartialCobolWord)
        /// partialReplacementToken = CONTRACT (token type = UserDefinedWord)
        /// generatedReplacementToken = CONTRACT-USER-NAME (token type = UserDefinedWord)
        /// </summary>
        public ReplacedPartialCobolWord(Token generatedReplacementToken, Token partialReplacementToken, Token originalPartialCobolWord)
            : base(generatedReplacementToken, originalPartialCobolWord)
        {
            PartialReplacementToken = partialReplacementToken;
        }
    }

    /// <summary>
    /// Token placeholder used to implement the REPLACE and COPY REPLACING compiler directives
    /// in the less common case when a list of source tokens are replaced by a list of replacement tokens.
    /// </summary>
    public class ReplacedTokenGroup : Token
    {
        /// <summary>
        /// Parameter of the REPLACE directive or REPLACING clause of the COPY directive which replaces the original token
        /// </summary>
        public Token ReplacementToken { get; private set; }

        /// <summary>
        /// Original tokens in the source text which were replaced
        /// </summary>
        public IList<Token> OriginalTokens { get; private set; }

        /// <summary>
        /// Create a token placeholder
        /// </summary>
        public ReplacedTokenGroup(Token replacementToken, IList<Token> originalTokens) :
            base(replacementToken.TokenType, originalTokens[0].StartIndex, originalTokens[originalTokens.Count - 1].StopIndex, originalTokens[originalTokens.Count - 1].UsesVirtualSpaceAtEndOfLine, originalTokens[0].TokensLine)
        {
            ReplacementToken = replacementToken;
            OriginalTokens = originalTokens;

            HasError = replacementToken.HasError;
            UsesDelimiters = replacementToken.UsesDelimiters;
            HasOpeningDelimiter = replacementToken.HasOpeningDelimiter;
            HasClosingDelimiter = replacementToken.HasClosingDelimiter;
            ExpectedClosingDelimiter = replacementToken.ExpectedClosingDelimiter;
            LiteralValue = replacementToken.LiteralValue;
        }

        public override string Text
        {
            get
            {
                return ReplacementToken.Text;
            }
        }
    }
}
