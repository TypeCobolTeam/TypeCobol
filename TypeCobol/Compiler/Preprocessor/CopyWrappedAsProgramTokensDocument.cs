using System.Collections.Generic;
using System.Diagnostics;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    public class CopyWrappedAsProgramTokensDocument : ProcessedTokensDocument
    {
        private static IEnumerable<Token> BeginProgramSkeleton(ITokensLine targetLine, string copyName)
        {
            if (targetLine == null) yield break;

            //IDENTIFICATION DIVISION.
            //PROGRAM-ID. <copyName>.
            yield return new Token(TokenType.IDENTIFICATION, 0, 0, targetLine);
            yield return new Token(TokenType.DIVISION, 0, 0, targetLine);
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);
            yield return new Token(TokenType.PROGRAM_ID, 0, 0, targetLine);
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);
            yield return new Token(TokenType.UserDefinedWord, 0, 0, targetLine) { LiteralValue = new AlphanumericLiteralTokenValue(copyName) };
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);

            //DATA DIVISION.
            //WORKING-STORAGE SECTION.
            yield return new Token(TokenType.DATA, 0, 0, targetLine);
            yield return new Token(TokenType.DIVISION, 0, 0, targetLine);
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);
            yield return new Token(TokenType.WORKING_STORAGE, 0, 0, targetLine);
            yield return new Token(TokenType.SECTION, 0, 0, targetLine);
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);

            //Always add an anonymous 01 root level. Depending of the content of the copy, this level may be populated
            //with children or not. We remove it if it is empty at the end of Node phase
            yield return new Token(TokenType.LevelNumber, 0, 0, targetLine) { LiteralValue = new IntegerLiteralTokenValue(null, "01") };
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);
        }

        private static IEnumerable<Token> EndProgramSkeleton(ITokensLine targetLine, string copyName)
        {
            if (targetLine == null) yield break;

            //END PROGRAM <copyName>.
            yield return new Token(TokenType.END, 0, 0, targetLine);
            yield return new Token(TokenType.PROGRAM, 0, 0, targetLine);
            yield return new Token(TokenType.UserDefinedWord, 0, 0, targetLine) { LiteralValue = new AlphanumericLiteralTokenValue(copyName) };
            yield return new Token(TokenType.PeriodSeparator, 0, 0, targetLine);
        }

        private static Token GenerateReplacement(Token partialCobolWordToken)
        {
            Debug.Assert(partialCobolWordToken.TokenType == TokenType.PartialCobolWord);

            //basic replacement mechanic, remove the ':' from the tag.
            //NOTE : PartialCobolWord have their ScanStateSnapshot embedded so we can use it to replace with number literal after OCCURS if need be...
            string replacedTokenText = partialCobolWordToken.NormalizedText.Replace(":", string.Empty);
            return ReplaceTokensLinesIterator.GenerateReplacementToken(partialCobolWordToken, replacedTokenText);
        }

        public CopyWrappedAsProgramTokensDocument(TokensDocument previousStepSnapshot, DocumentVersion<IProcessedTokensLine> processedTokensLinesVersion, ISearchableReadOnlyList<IProcessedTokensLine> processedTokensLines, TypeCobolOptions compilerOptions, List<MissingCopy> missingCopies)
            : base(previousStepSnapshot, processedTokensLinesVersion, processedTokensLines, compilerOptions, missingCopies)
        {

        }

        public override ITokensLinesIterator GetProcessedTokensIterator()
        {
            ITokensLinesIterator iterator = base.GetProcessedTokensIterator();

            //no matter what, replace remaining PartialCobolWords
            iterator = new AutoReplacePartialWordsTokensLinesIterator(iterator, GenerateReplacement);

            //if it's the first time we iterate over tokens, wrap tokens of the copy into a fake program
            if (CurrentVersion.IsInitial)
            {
                var beforeTokens = BeginProgramSkeleton(iterator.FirstLine, TextSourceInfo.Name);
                var afterTokens = EndProgramSkeleton(iterator.LastLine, TextSourceInfo.Name);
                iterator = new WrapperTokensLinesIterator(beforeTokens, iterator, afterTokens);
            }

            return iterator;
        }
    }
}
