using System.Collections.Generic;
using System.Diagnostics;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Special ProcessedTokensDocument that wraps original lines of a copy into a fake program
    /// to allow full parsing and analysis on copies.
    /// </summary>
    public class CopyWrappedAsProgramTokensDocument : ProcessedTokensDocument
    {
        private static ISearchableReadOnlyList<CodeElementsLine> Wrap(ISearchableReadOnlyList<CodeElementsLine> originalLines, TextSourceInfo textSourceInfo)
        {
            var columnsLayout = textSourceInfo.ColumnsLayout;
            var start = columnsLayout == ColumnsLayout.CobolReferenceFormat ? 7 : 0;
            var indent = new string(' ', start);

            var lines = new ImmutableList<CodeElementsLine>.Builder(ImmutableList<CodeElementsLine>.Empty);

            //Begin fake program skeleton
            lines.Add(IdentificationDivision());
            lines.Add(ProgramId());
            lines.Add(DataDivision());
            lines.Add(WorkingStorageSection());

            //Always add an anonymous 01 root level. Depending of the content of the copy, this level may be populated
            //with children or not. We remove it if it is empty at the end of Node phase.
            lines.Add(RootLevel());

            //Original content of the copy
            lines.AddRange(originalLines);

            //End fake program skeleton
            lines.Add(EndProgram());

            return lines.ToImmutable();

            CodeElementsLine IdentificationDivision()
            {
                string text = $"{indent}IDENTIFICATION DIVISION.";//0,13|14,14|15,22|23,23
                var line = new CodeElementsLine(new TextLineSnapshot(0, text, null), columnsLayout);
                line.SourceTokens.Add(new Token(TokenType.IDENTIFICATION, start, start + 13, line));
                line.SourceTokens.Add(new Token(TokenType.DIVISION, start + 15, start + 22, line));
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 23, start + 23, line));
                line.PreprocessingState = ProcessedTokensLine.PreprocessorState.Ready;
                return line;
            }

            CodeElementsLine ProgramId()
            {
                string programName = System.IO.Path.GetFileNameWithoutExtension(textSourceInfo.Name);
                int nameLength = programName.Length;
                string text = $"{indent}PROGRAM-ID. {programName}.";//0,9|10,10|11,11|12,12+nameLength-1|12+nameLength,12+nameLength
                var line = new CodeElementsLine(new TextLineSnapshot(0, text, null), columnsLayout);
                line.SourceTokens.Add(new Token(TokenType.PROGRAM_ID, start, start + 9, line));
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 10, start + 10, line));
                line.SourceTokens.Add(new Token(TokenType.UserDefinedWord, start + 12, start + 11 + nameLength, line));
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 12 + nameLength, start + 12 + nameLength, line));
                line.PreprocessingState = ProcessedTokensLine.PreprocessorState.Ready;
                return line;
            }

            CodeElementsLine DataDivision()
            {
                string text = $"{indent}DATA DIVISION.";//0,3|4,4|5,12|13,13
                var line = new CodeElementsLine(new TextLineSnapshot(0, text, null), columnsLayout);
                line.SourceTokens.Add(new Token(TokenType.DATA, start, start + 3, line));
                line.SourceTokens.Add(new Token(TokenType.DIVISION, start + 5, start + 12, line));
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 13, start + 13, line));
                line.PreprocessingState = ProcessedTokensLine.PreprocessorState.Ready;
                return line;
            }

            CodeElementsLine WorkingStorageSection()
            {
                string text = $"{indent}WORKING-STORAGE SECTION.";//0,14|15,15|16,22|23,23
                var line = new CodeElementsLine(new TextLineSnapshot(0, text, null), columnsLayout);
                line.SourceTokens.Add(new Token(TokenType.WORKING_STORAGE, start, start + 14, line));
                line.SourceTokens.Add(new Token(TokenType.SECTION, start + 16, start + 22, line));
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 23, start + 23, line));
                line.PreprocessingState = ProcessedTokensLine.PreprocessorState.Ready;
                return line;
            }

            CodeElementsLine RootLevel()
            {
                string text = $"{indent}01.";//0,1|2,2
                var line = new CodeElementsLine(new TextLineSnapshot(0, text, null), columnsLayout);
                line.SourceTokens.Add(new Token(TokenType.LevelNumber, start, start + 1, line) { LiteralValue = new IntegerLiteralTokenValue(null, "01") });
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 2, start + 2, line));
                line.PreprocessingState = ProcessedTokensLine.PreprocessorState.Ready;
                return line;
            }

            CodeElementsLine EndProgram()
            {
                int nameLength = textSourceInfo.Name.Length;
                string text = $"{indent}END PROGRAM {textSourceInfo.Name}.";//0,2|3,3|4,10|11,11|12,12+nameLength-1|12+nameLength,12+nameLength
                var line = new CodeElementsLine(new TextLineSnapshot(originalLines.Count - 1, text, null), columnsLayout);
                line.SourceTokens.Add(new Token(TokenType.END, start, start + 2, line));
                line.SourceTokens.Add(new Token(TokenType.PROGRAM, start + 4, start + 10, line));
                line.SourceTokens.Add(new Token(TokenType.UserDefinedWord, start + 12, start + 11 + nameLength, line));
                line.SourceTokens.Add(new Token(TokenType.PeriodSeparator, start + 12 + nameLength, start + 12 + nameLength, line));
                line.PreprocessingState = ProcessedTokensLine.PreprocessorState.Ready;
                return line;
            }
        }

        private static Token GenerateReplacement(Token partialCobolWordToken)
        {
            Debug.Assert(partialCobolWordToken.TokenType == TokenType.PartialCobolWord);

            //basic replacement mechanic, remove the ':' from the tag.
            //NOTE: PartialCobolWord have their ScanStateSnapshot embedded so we can use it to replace with number literal after OCCURS if need be...
            //NOTE: Does not handle '::-item' or 'item-::' partial names as '::' will turn into empty string and will produce invalid data names.
            string replacedTokenText = partialCobolWordToken.NormalizedText.Replace(":", string.Empty);
            return ReplaceTokensLinesIterator.GenerateReplacementToken(partialCobolWordToken, replacedTokenText);
        }

        public CopyWrappedAsProgramTokensDocument(TokensDocument previousStepSnapshot, DocumentVersion<IProcessedTokensLine> processedTokensLinesVersion, ISearchableReadOnlyList<CodeElementsLine> processedTokensLines, TypeCobolOptions compilerOptions, List<MissingCopy> missingCopies)
            : base(
                previousStepSnapshot,
                processedTokensLinesVersion,
                //Wrap original copy lines into a fake program if it is the first parsing
                processedTokensLinesVersion.IsInitial ? Wrap(processedTokensLines, previousStepSnapshot.TextSourceInfo) : processedTokensLines,
                compilerOptions,
                missingCopies)
        {
            
        }

        public override ITokensLinesIterator GetProcessedTokensIterator()
        {
            //Auto-replace all remaining PartialCobolWords
            return new AutoReplacePartialWordsTokensLinesIterator(base.GetProcessedTokensIterator(), GenerateReplacement);
        }
    }
}
