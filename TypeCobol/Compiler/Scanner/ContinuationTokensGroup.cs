#nullable enable

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// When a compiler directive or a replaced token group spans multiple line,
    /// create multiple TokenGroups, one for each line : a specific TokensGroup for the first line
    /// and then multiple ContinuationTokenGroups for the following lines, all pointing
    /// to the first specific TokensGroup containing the useful information.
    /// </summary>
    public class ContinuationTokensGroup : TokensGroup
    {
        public ContinuationTokensGroup(TokensGroup firstTokenGroup, IList<Token> originalTokens) :
            base(TokenType.CONTINUATION_TOKEN_GROUP, originalTokens)
        {
            FirstTokenGroup = firstTokenGroup;
        }

        /// <summary>
        /// First TokensGroup of a multiline token group, which contains all the useful information
        /// </summary>
        public TokensGroup FirstTokenGroup { get; private set; }

        /// <summary>
        /// Debug string : append continuation information
        /// </summary>
        public override string ToString()
        {
            return "+++ CONTINUATION OF " + FirstTokenGroup.TokenType.ToString() + " (" + base.ToString() + ") +++";
        }
    }
}
