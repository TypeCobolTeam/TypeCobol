using System.Collections.Generic;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// View of a source document after the lexical analysis stage as lines of tokens
    /// </summary>
    public class TokensDocument : IFirstStepDocumentSnapshot<ICobolTextLine,ITokensLine>
    {
        public TokensDocument(TextSourceInfo textSourceInfo, DocumentVersion<ICobolTextLine> textLinesVersion, DocumentVersion<ITokensLine> tokensLinesVersion, DocumentVersion<ITokensLine> previousVersion, ISearchableReadOnlyList<ITokensLine> tokensLines)
        {
            TextSourceInfo = textSourceInfo;
            SourceLinesVersion = textLinesVersion;
            CurrentVersion = tokensLinesVersion;
            PreviousVersion = previousVersion;
            Lines = tokensLines;
        }

        /// <summary>
        /// Informations on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; }

        /// <summary>
        /// Document version identifier for the list of cobol text lines used as a basis to compute the current step
        /// </summary>
        public DocumentVersion<ICobolTextLine> SourceLinesVersion { get; }

        /// <summary>
        /// Document version identifier for the current document
        /// </summary>
        public DocumentVersion<ITokensLine> CurrentVersion { get; }

        /// <summary>
        /// Previous document version for this snapshot
        /// </summary>
        public DocumentVersion<ITokensLine> PreviousVersion { get; }

        /// <summary>
        /// Lines of the source text file viewed as lists of tokens and error messages
        /// </summary>
        public ISearchableReadOnlyList<ITokensLine> Lines { get; }

        /// <summary>
        /// Iterator over all the source tokens 
        /// </summary>
        public IEnumerable<Token> SourceTokens
        {
            get
            {
                TokensLinesIterator tokenSource = new TokensLinesIterator(TextSourceInfo.Name, Lines, null, Token.CHANNEL_SourceTokens);
                Token token = null;
                do
                {
                    token = (Token)tokenSource.NextToken();
                    yield return token;
                } while (token.Type != (int)TokenType.EndOfFile);
            }
        }
    }
}
