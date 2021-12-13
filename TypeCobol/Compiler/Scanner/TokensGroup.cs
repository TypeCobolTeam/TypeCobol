using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
    
    /// <summary>
    /// Replace a list of tokens all located on the same source line by a single token.
    /// Used to mark the limits of compiler directives and replaced token groups.
    /// </summary>
    public class TokensGroup : Token
    {
        // *** Values for tokens group properties ***
        // 
        // -- Position in the source text 
        //    => original tokens
        //
        // internal ITextLine TextLine
        // public int Line
        // 
        // public int StartIndex
        // public int StopIndex
        // public int Column
        // public int EndColumn
        // public bool UsesVirtualSpaceAtEndOfLine
        // public int Length
        // 
        // public bool IsContinuedOnTheNextLine
        // public ContinuationToken ContinuationToken
        // 
        // -- Type, text, channel, value
        //    => specific implementation in a derived class
        // 
        // public int Type
        // public TokenType TokenType
        // public TokenFamily TokenFamily
        // 
        // public int Channel
        // public bool HasError
        // 
        // public string Text
        //       
        // -- Delimiters and LiteralValue
        //    => not used
        //    
        // public bool UsesDelimiters 
        // public bool HasOpeningDelimiter
        // public bool HasClosingDelimiter
        // public char ExpectedClosingDelimiter
        // public object LiteralValue
        // 
        // *******

        public TokensGroup(TokenType tokenType, IList<Token> originalTokens) :
            base(tokenType, originalTokens[0].StartIndex, originalTokens[originalTokens.Count - 1].StopIndex, originalTokens[originalTokens.Count - 1].UsesVirtualSpaceAtEndOfLine, originalTokens[0].TokensLine)
        {
            OriginalTokens = originalTokens;
        }

        /// <summary>
        /// Original tokens in the source text which were replaced
        /// </summary>
        public IList<Token> OriginalTokens { get; private set; }

        /// <summary>
        /// Debug string : concat the description of all OriginalTokens
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            foreach(Token token in OriginalTokens)
            {
                sb.Append(token.ToString());
            }
            return sb.ToString();
        }

        /// <summary>
        /// Text returned to the parser :
        /// - Token.SourceText if the token is not a PartialCobolWord
        /// - The concatenation of ToString() of all OriginalTokens.
        /// </summary>
        public override string SourceText
        {
            get
            {
                if (this.TokenType != TokenType.PartialCobolWord)
                    return base.SourceText;
                else 
                {
                    StringBuilder sb = new StringBuilder();
                    foreach (Token token in OriginalTokens)
                    {
                        sb.Append(token.SourceText);
                    }
                    String t = sb.ToString();
                    return t;
                }
            }
        }

    }
}
