using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// View of a source document after the lexical analysis stage as lines of tokens
    /// </summary>
    public class TokensDocument : IFirstStepDocumentSnapshot<ICobolTextLine,ITokensLine>
    {
        public TokensDocument(TextSourceInfo textSourceInfo, DocumentVersion<ICobolTextLine> textLinesVersion, DocumentVersion<ITokensLine> tokensLinesVersion, ISearchableReadOnlyList<ITokensLine> tokensLines)
        {
            TextSourceInfo = textSourceInfo;
            SourceLinesVersion = textLinesVersion;
            CurrentVersion = tokensLinesVersion;
            Lines = tokensLines;
        }

        /// <summary>
        /// Informations on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// Document version identifier for the list of cobol text lines used as a basis to compute the current step
        /// </summary>
        public DocumentVersion<ICobolTextLine> SourceLinesVersion { get; private set; }

        /// <summary>
        /// Document version identifier for the current document
        /// </summary>
        public DocumentVersion<ITokensLine> CurrentVersion { get; private set; }
        
        /// <summary>
        /// Lines of the source text file viewed as lists of tokens and error messages
        /// </summary>
        public ISearchableReadOnlyList<ITokensLine> Lines { get; private set; }

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

        // --- Debugging tools ---

        public string GetDebugString()
        {
            StringBuilder sbResult = new StringBuilder();

            int i = 0;
            foreach (var line in Lines)
            {
                sbResult.AppendLine("-- Line " + (i + 1) + " --");
                foreach (Token token in line.SourceTokens)
                {
                    sbResult.AppendLine(token.ToString());
                }
                foreach (Diagnostic diagnostic in line.ScannerDiagnostics)
                {
                    sbResult.AppendLine(diagnostic.ToString());
                }
                i++;
            }

            return sbResult.ToString();
        }
    }
}
