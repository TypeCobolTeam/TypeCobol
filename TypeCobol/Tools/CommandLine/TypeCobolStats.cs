using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Symbols;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Tools.CommandLine
{
    /// <summary>
    /// Compute stats for all Cobol syntax constructs over a set of files in a directory
    /// </summary>
    public static class TypeCobolStats
    {
        public static void Main(string[] args)
        {
            // Directory where source programs and COPY files are stored
            string sourcePath = @"D:\Users\Laurent\OneDrive\Dev\Visual Studio 2012\Projects\TypeCobol\TypeCobol.Test\Samples\EI Cobol samples\EI-Production";
            string[] programExtensions = { "*.PGM" };
            string[] copyExtensions = { "*.CPY" };

            // Source file format for the samples
            DocumentFormat docFormat = new DocumentFormat(Encoding.GetEncoding("iso8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);

            // Initialize compilation project
            TypeCobolOptions compilerOptions = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("samples", sourcePath, programExtensions.Concat(copyExtensions).ToArray(),
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength, docFormat.ColumnsLayout, compilerOptions);

            // Initialize statistics vars

            Stopwatch chrono = new Stopwatch();
            chrono.Start();

            // 1. Program analysis after preprocessing

            // - total number of lines per program (including expanded COPY directives)
            long[] linesCountDistributionCategories = { 500, 1000, 1500, 2000, 3000, 5000, 7500, 10000, 15000, 20000, 30000, 50000, int.MaxValue };
            StatCounter<CobolTextLineType> linesCounter = new StatCounter<CobolTextLineType>(linesCountDistributionCategories);

            // - total number of tokens per program (after preprocessing)
            long[] tokensCountDistributionCategories = { 1500, 3000, 4500, 6000, 9000, 15000, 22500, 30000, 45000, 60000, 90000, 150000, int.MaxValue };
            StatCounter<TokenType> tokensCounter = new StatCounter<TokenType>(tokensCountDistributionCategories);

            // - number of copies per program
            long[] copiesCountDistributionCategories = { 0, 5, 10, 15, 20, 30, 50, int.MaxValue };
            StatCounter<CopyDirectiveType> copiesCounter = new StatCounter<CopyDirectiveType>(copiesCountDistributionCategories);

            // - number of replaced tokens per program
            long[] replacedTokensCountDistributionCategories = { 50, 100, 150, 200, 300, 500, 1000, 2000, 5000, 10000, 20000, int.MaxValue };
            StatCounter<TokenType> replacedTokensCounter = new StatCounter<TokenType>(tokensCountDistributionCategories);

            // - number of code elements per program
            long[] codeElementsCountDistributionCategories = { 100, 200, 300, 400, 500, 750, 1000, 1500, 2000, 5000, 10000, int.MaxValue };
            StatCounter<CodeElementType> codeElementCounter = new StatCounter<CodeElementType>(codeElementsCountDistributionCategories);

            // 2. Program files before preprocessing

            // - number of lines per program file (before preprocessing)
            long[] linesCountPerProgramFileDistributionCategories = { 25, 50, 100, 150, 200, 300, 500, 1000, 1500, 2000, 3000, int.MaxValue };
            StatCounter<CobolTextLineType> linesPerProgramFileCounter = new StatCounter<CobolTextLineType>(linesCountPerProgramFileDistributionCategories);

            // - number of tokens per program file (before preprocessing)
            long[] tokensCountPerProgramFileDistributionCategories = { 1500, 3000, 4500, 6000, 9000, 15000, 22500, 30000, 45000, 60000, 90000, 150000, int.MaxValue };
            StatCounter<TokenType> tokensPerProgramFileCounter = new StatCounter<TokenType>(tokensCountPerProgramFileDistributionCategories);

            // - number of compiler directives per program file
            long[] compilerDirectivesPerProgramFileCountDistributionCategories = { 0, 5, 10, 15, 20, 30, 50, 75, 100, 200, int.MaxValue };
            StatCounter<CompilerDirectiveType> compilerDirectivesPerProgramFileCounter = new StatCounter<CompilerDirectiveType>(compilerDirectivesPerProgramFileCountDistributionCategories);

            // 3. Copy files before preprocessing

            // - number of references to each copy file
            IDictionary<string, int> copyFileReferenceCount = new Dictionary<string, int>();

            // - number of lines per copy file (before preprocessing)
            long[] linesCountPerCopyFileDistributionCategories = { 25, 50, 100, 150, 200, 300, 500, 1000, 1500, 2000, 3000, int.MaxValue };
            StatCounter<CobolTextLineType> linesPerCopyFileCounter = new StatCounter<CobolTextLineType>(linesCountPerCopyFileDistributionCategories);

            // - number of tokens per copy file (before preprocessing)
            long[] tokensCountPerCopyFileDistributionCategories = { 1500, 3000, 4500, 6000, 9000, 15000, 22500, 30000, 45000, 60000, 90000, 150000, int.MaxValue };
            StatCounter<TokenType> tokensPerCopyFileCounter = new StatCounter<TokenType>(tokensCountPerCopyFileDistributionCategories);

            // - number of compiler directives per copy file
            long[] compilerDirectivesPerCopyFileCountDistributionCategories = { 0, 5, 10, 15, 20, 30, 50, 75, 100, 200, int.MaxValue };
            StatCounter<CompilerDirectiveType> compilerDirectivesPerCopyFileCounter = new StatCounter<CompilerDirectiveType>(compilerDirectivesPerCopyFileCountDistributionCategories);
                        
            // 4. Language models

            // - language model to predict the next word in a program
            LanguageModel languageModelForProgram = new LanguageModel();

            // - language model to predict the next word in a copy
            LanguageModel languageModelForCopy = new LanguageModel();

            // -- Compile and compute stats --

            // Iterate over all programs in the source directory
            foreach (string programExtension in programExtensions)
            {
                foreach (string filePath in Directory.EnumerateFiles(sourcePath, programExtension))
                {
                    string textName = Path.GetFileNameWithoutExtension(filePath);
                    Console.Write(textName + " : compilation ... ");
                    int programCopiesNotFound = 0;

                    try
                    {
                        // Compile program
                        FileCompiler fileCompiler = new FileCompiler(null, textName, project.SourceFileProvider, project, ColumnsLayout.CobolReferenceFormat, compilerOptions.Clone(), false);
                        fileCompiler.CompileOnce();
                        CompilationUnit compilationResult = fileCompiler.CompilationResultsForProgram;
                        programCopiesNotFound = 0;

                        // Compute stats
                        Console.Write(" OK, compute stats ... ");

                        // STATS for PROGRAM

                        linesCounter.OnBeginProgram();
                        tokensCounter.OnBeginProgram();
                        copiesCounter.OnBeginProgram();
                        replacedTokensCounter.OnBeginProgram();
                        codeElementCounter.OnBeginProgram();
                        linesPerProgramFileCounter.OnBeginProgram();
                        tokensPerProgramFileCounter.OnBeginProgram();
                        compilerDirectivesPerProgramFileCounter.OnBeginProgram();
                        languageModelForProgram.OnBeginProgram();

                        // Iterate over program file lines 
                        foreach (var line in compilationResult.CodeElementsDocumentSnapshot.Lines)
                        {
                            // + count lines
                            linesCounter.OnElement((int)line.Type);
                            linesPerProgramFileCounter.OnElement((int)line.Type);

                            // Use symbol information known at parsing time for the tokens to build a language model
                            if (line.CodeElements != null)
                            {
                                foreach (var codeElement in line.CodeElements)
                                {
                                    if (codeElement.SymbolInformationForTokens.Count > 0)
                                    {
                                        languageModelForProgram.AddSymbolInformationForTokens(codeElement.SymbolInformationForTokens);
                                    }
                                }
                            }
                            if(line.ImportedDocuments != null)
                            {
                                var symbolInformationForTokens = new Dictionary<Token,SymbolInformation>();
                                foreach(var copyDirective in line.ImportedDocuments.Keys)
                                {
                                    if (copyDirective.TextNameSymbol != null)
                                    {
                                        symbolInformationForTokens.Add(copyDirective.TextNameSymbol, new SymbolInformation(copyDirective.TextNameSymbol, SymbolRole.ExternalName, SymbolType.TextName));
                                    }
                                    if (copyDirective.LibraryNameSymbol != null)
                                    {
                                        symbolInformationForTokens.Add(copyDirective.TextNameSymbol, new SymbolInformation(copyDirective.LibraryNameSymbol, SymbolRole.ExternalName, SymbolType.LibraryName));
                                    }
                                }
                                languageModelForProgram.AddSymbolInformationForTokens(symbolInformationForTokens);
                            }

                            // Iterate over tokens on this line
                            foreach (var token in line.SourceTokens)
                            {
                                // + count tokens and build language model
                                tokensPerProgramFileCounter.OnElement((int)token.TokenType);
                                languageModelForProgram.OnToken(token);
                            }

                            // Iterate over compiler directives on this line
                            if (line.HasCompilerDirectives)
                            {
                                foreach (var token in line.TokensWithCompilerDirectives)
                                {
                                    CompilerDirectiveToken compilerDirectiveToken = token as CompilerDirectiveToken;
                                    if (compilerDirectiveToken != null)
                                    {
                                        compilerDirectivesPerProgramFileCounter.OnElement((int)compilerDirectiveToken.CompilerDirective.Type);
                                    }
                                }
                            }

                            // Iterate over COPY directives on this line
                            if (line.ImportedDocuments != null)
                            {                                
                                foreach (CopyDirective copyDirective in line.ImportedDocuments.Keys)
                                {
                                    // + count COPY directives
                                    CopyDirectiveType copyDirectiveType = CopyDirectiveType.Copy;
                                    if (copyDirective.InsertSuffixChar)
                                    {
                                        copyDirectiveType = CopyDirectiveType.CopyReplacingRemarks;
                                    }
                                    else if (copyDirective.RemoveFirst01Level)
                                    {
                                        copyDirectiveType = CopyDirectiveType.CopyRemarks;
                                    }
                                    else if (copyDirective.ReplaceOperations != null && copyDirective.ReplaceOperations.Count > 0)
                                    {
                                        copyDirectiveType = CopyDirectiveType.CopyReplacing;
                                    }
                                    copiesCounter.OnElement((int)copyDirectiveType);
                                    
                                    var importedDocument = line.ImportedDocuments[copyDirective];
                                    if (importedDocument == null)
                                    {
                                        // + count missing COPY files for this program
                                        programCopiesNotFound++;
                                    }
                                    else
                                    {
                                        // + count references to copy files
                                        // AND check if copy file has already been analyzed
                                        string copyFileReference = copyDirective.LibraryName + ":" + copyDirective.TextName;
                                        if (copyFileReferenceCount.ContainsKey(copyFileReference))
                                        {
                                            copyFileReferenceCount[copyFileReference] = copyFileReferenceCount[copyFileReference] + 1;

                                            // Iterate over copy file lines 
                                            foreach (var copyLine in importedDocument.SourceDocument.Lines)
                                            {
                                                // + count lines inside COPY file
                                                linesCounter.OnElement((int)copyLine.Type);
                                                linesPerCopyFileCounter.OnElement((int)copyLine.Type);
                                            }
                                        }
                                        else
                                        {
                                            copyFileReferenceCount.Add(copyFileReference, 1);

                                            // STATS FOR COPY

                                            linesPerCopyFileCounter.OnBeginProgram();
                                            tokensPerCopyFileCounter.OnBeginProgram();
                                            compilerDirectivesPerCopyFileCounter.OnBeginProgram();
                                            languageModelForCopy.OnBeginProgram();

                                            // Iterate over copy file lines 
                                            foreach (var copyLine in importedDocument.SourceDocument.Lines)
                                            {
                                                // + count lines inside COPY file
                                                linesCounter.OnElement((int)copyLine.Type);
                                                linesPerCopyFileCounter.OnElement((int)copyLine.Type);

                                                // Use symbol information known at parsing time for the tokens to build a language model
                                                if (copyLine.ImportedDocuments != null)
                                                {
                                                    var symbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
                                                    foreach (var copyDirective2 in line.ImportedDocuments.Keys)
                                                    {
                                                        if (copyDirective2.TextNameSymbol != null)
                                                        {
                                                            symbolInformationForTokens.Add(copyDirective2.TextNameSymbol, new SymbolInformation(copyDirective2.TextNameSymbol, SymbolRole.ExternalName, SymbolType.TextName));
                                                        }
                                                        if (copyDirective2.LibraryNameSymbol != null)
                                                        {
                                                            symbolInformationForTokens.Add(copyDirective2.TextNameSymbol, new SymbolInformation(copyDirective2.LibraryNameSymbol, SymbolRole.ExternalName, SymbolType.LibraryName));
                                                        }
                                                    }
                                                    languageModelForCopy.AddSymbolInformationForTokens(symbolInformationForTokens);
                                                }

                                                // Iterate over tokens on this line
                                                foreach (var token in copyLine.SourceTokens)
                                                {
                                                    // + count tokens and build language model
                                                    tokensPerCopyFileCounter.OnElement((int)token.TokenType);
                                                    languageModelForCopy.OnToken(token);
                                                }

                                                // Iterate over compiler directives on this line
                                                if (copyLine.HasCompilerDirectives)
                                                {
                                                    foreach (var token in copyLine.TokensWithCompilerDirectives)
                                                    {
                                                        CompilerDirectiveToken compilerDirectiveToken = token as CompilerDirectiveToken;
                                                        if (compilerDirectiveToken != null)
                                                        {
                                                            compilerDirectivesPerCopyFileCounter.OnElement((int)compilerDirectiveToken.CompilerDirective.Type);
                                                        }
                                                    }
                                                }
                                            }

                                            linesPerCopyFileCounter.OnEndProgram();
                                            tokensPerCopyFileCounter.OnEndProgram();
                                            compilerDirectivesPerCopyFileCounter.OnEndProgram();
                                        }
                                    }
                                }
                            }                            

                            // Iterate over code elements on this line
                            if (line.CodeElements != null)
                            {
                                foreach (var codeElement in line.CodeElements)
                                {
                                    codeElementCounter.OnElement((int)codeElement.Type);
                                }
                            }
                        }

                        // Iterate over tokens AFTER preprocessing
                        ITokensLinesIterator processedTokensIterator = compilationResult.ProcessedTokensDocumentSnapshot.ProcessedTokens;
                        Token processedToken = null;
                        while ((processedToken = processedTokensIterator.NextToken()) != Token.END_OF_FILE)
                        {
                            tokensCounter.OnElement((int)processedToken.TokenType);
                            ReplacedToken replacedToken = processedToken as ReplacedToken;
                            if(replacedToken != null)
                            {
                                replacedTokensCounter.OnElement((int)replacedToken.OriginalToken.TokenType);                                
                            }
                            else if (processedToken is ReplacedTokenGroup)
                            {
                                replacedTokensCounter.OnElement((int)TokenType.ContinuationTokenGroup);
                            }
                        }

                        linesCounter.OnEndProgram();
                        tokensCounter.OnEndProgram();
                        copiesCounter.OnEndProgram();
                        replacedTokensCounter.OnEndProgram();
                        codeElementCounter.OnEndProgram();
                        linesPerProgramFileCounter.OnEndProgram();
                        tokensPerProgramFileCounter.OnEndProgram();
                        compilerDirectivesPerProgramFileCounter.OnEndProgram();
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("ERROR :");
                        Console.WriteLine(e.Message);
                    }
                    finally
                    {
                        Console.Write("FINISHED");
                        if(programCopiesNotFound == 0)
                        {
                            Console.WriteLine();
                        }
                        else
                        {
                            Console.WriteLine(" (" + programCopiesNotFound + " missing COPY)");
                        }
                    }
                }
            }

            // Compute language models
            languageModelForProgram.ComputeProbabilities();
            languageModelForCopy.ComputeProbabilities();

            // Write results files

            string resultFilePath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "TypeCobolStats_");
            string countersFile = resultFilePath + "counters.txt";
            string languageModelForProgramFile = resultFilePath + "modelForProgram.txt";
            string languageModelForCopyFile = resultFilePath + "modelForCopy.txt";

            Console.WriteLine("");

            Console.WriteLine("Writing statistics results to " + countersFile);
            using (StreamWriter writer = new StreamWriter(countersFile))
            {
                writer.WriteLine("1. Program analysis after preprocessing");
                writer.WriteLine();
                WriteTitle(writer, "Total number of lines per program (including expanded COPY directives)");
                linesCounter.DisplayResults(writer);
                WriteTitle(writer, "Total number of tokens per program (after preprocessing)");
                tokensCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of copies per program");
                copiesCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of replaced tokens per program");
                replacedTokensCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of code elements per program");
                codeElementCounter.DisplayResults(writer);

                writer.WriteLine("2. Program files before preprocessing");
                writer.WriteLine();
                WriteTitle(writer, "Number of lines per program file (before preprocessing)");
                linesPerProgramFileCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of tokens per program file (before preprocessing)");
                tokensPerProgramFileCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of compiler directives per program file");
                compilerDirectivesPerProgramFileCounter.DisplayResults(writer);

                writer.WriteLine("3. Copy files before preprocessing");
                writer.WriteLine();
                WriteTitle(writer, "Number of references to each copy file");
                // copyFileReferenceCount = new Dictionary<string, int>()
                WriteTitle(writer, "Number of lines per copy file (before preprocessing)");
                linesPerCopyFileCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of tokens per copy file (before preprocessing)");
                tokensPerCopyFileCounter.DisplayResults(writer);
                WriteTitle(writer, "Number of compiler directives per copy file");
                compilerDirectivesPerCopyFileCounter.DisplayResults(writer);
            }
            Console.WriteLine("Done");


            Console.WriteLine("Writing language model for program to " + languageModelForProgramFile);
            using (StreamWriter writer = new StreamWriter(languageModelForProgramFile))
            {
                languageModelForProgram.DisplayResults(writer);
            }
            Console.WriteLine("Done");

            Console.WriteLine("Writing language model for copy to " + languageModelForCopyFile);
            using (StreamWriter writer = new StreamWriter(languageModelForCopyFile))
            {
                languageModelForCopy.DisplayResults(writer);
            }

            chrono.Stop();
            Console.WriteLine("Done in " + Math.Round(chrono.ElapsedMilliseconds/(double)1000,3) + " sec");
        }

        private static void WriteTitle(StreamWriter writer, string title)
        {
            writer.WriteLine("----------------------");
            writer.WriteLine(title);
            writer.WriteLine("----------------------");
            writer.WriteLine();
        }
    }    

    enum CopyDirectiveType
    {
        Copy,
        CopyReplacing,
        CopyRemarks,
        CopyReplacingRemarks
    }

    class StatCounter<E>
    {
        private int[] elementTypes;
        private int maxElementType;

        private int programCount;

        private long[] currentProgramCounters;

        private long[] totalCounters;
        private long[] totalMins;
        private long[] totalMaxs;

        private long currentProgramCounter;

        private long totalCounter;
        private long totalMin;
        private long totalMax;

        private long[] distributionBoundaries;
        private long[] totalDistributionCounters;

        public StatCounter(long[] distributionBoundaries)
        {
            elementTypes = (int[])Enum.GetValues(typeof(E));
            maxElementType = elementTypes.Max();

            currentProgramCounters = new long[maxElementType + 1];

            totalCounters = new long[maxElementType + 1];
            totalMins = new long[maxElementType + 1];
            for (int elementTypeIndex = 0; elementTypeIndex <= maxElementType; elementTypeIndex++)
            {
                totalMins[elementTypeIndex] = long.MaxValue;
            }
            totalMaxs = new long[maxElementType + 1];

            totalMin = long.MaxValue;

            this.distributionBoundaries = distributionBoundaries;
            totalDistributionCounters = new long[distributionBoundaries.Length];
        }

        public void OnBeginProgram()
        {
            programCount++;
            currentProgramCounter = 0;
            for (int elementTypeIndex = 0; elementTypeIndex <= maxElementType; elementTypeIndex++)
            {
                currentProgramCounters[elementTypeIndex] = 0;
            }
        }

        public void OnElement(int elementTypeIndex)
        {
            currentProgramCounters[elementTypeIndex]++;
            currentProgramCounter++;
        }

        public void OnEndProgram()
        {
            for (int elementTypeIndex = 0; elementTypeIndex <= maxElementType; elementTypeIndex++)
            {
                totalCounters[elementTypeIndex] += currentProgramCounters[elementTypeIndex];
                if (currentProgramCounters[elementTypeIndex] < totalMins[elementTypeIndex]) totalMins[elementTypeIndex] = currentProgramCounters[elementTypeIndex];
                if (currentProgramCounters[elementTypeIndex] > totalMaxs[elementTypeIndex]) totalMaxs[elementTypeIndex] = currentProgramCounters[elementTypeIndex];
            }

            totalCounter += currentProgramCounter;
            if (currentProgramCounter < totalMin) totalMin = currentProgramCounter;
            if (currentProgramCounter > totalMax) totalMax = currentProgramCounter;

            for (int i = 0; i < distributionBoundaries.Length; i++)
            {
                if (currentProgramCounter <= distributionBoundaries[i])
                {
                    totalDistributionCounters[i]++;
                    break;
                }
            }
        }

        public void DisplayResults(TextWriter writer)
        {
            writer.WriteLine("Statistics for " + typeof(E).Name + " on " + programCount + " programs :");
            writer.WriteLine(" - Average number per program : " + (totalCounter / programCount));
            writer.WriteLine(" - Minimum number per program : " + totalMin);
            writer.WriteLine(" - Maximum number per program : " + totalMax);
            writer.WriteLine(" - Distribution of counts per program : ");
            for (int i = 0; i < distributionBoundaries.Length; i++)
            {
                writer.WriteLine("   . " + (i == 0 ? 0 : distributionBoundaries[i - 1]) + " - " + (distributionBoundaries[i] == long.MaxValue ? "+++" : distributionBoundaries[i].ToString()) + " : " + ((totalDistributionCounters[i] * 10000) / programCount / (float)100).ToString() + " %");
            }
            writer.WriteLine(" - Details per element type : ");
            foreach (var elementTypeIndex in elementTypes.OrderByDescending(elementTypeIndex => elementTypeIndex >=0 ? totalCounters[elementTypeIndex] : 0))
            {
                if (elementTypeIndex >= 0)
                {
                    float elementTypePercentage = ((totalCounters[elementTypeIndex] * 10000 / totalCounter) / (float)100);
                    if (elementTypePercentage >= 0.1)
                    {
                        writer.WriteLine("   . " + Enum.GetName(typeof(E), elementTypeIndex) + " : " + elementTypePercentage + " %");
                        writer.WriteLine("      + Average number per program : " + (totalCounters[elementTypeIndex] / programCount));
                        writer.WriteLine("      + Minimum number per program : " + totalMins[elementTypeIndex]);
                        writer.WriteLine("      + Maximum number per program : " + totalMaxs[elementTypeIndex]);
                    }
                }
            }
            writer.WriteLine();
        }
    }

    class LanguageModel
    {
        private long TotalCount;
        public IDictionary<TokenType, WordProbabilitiesAfterElementStartingWord> WordProbabilitiesAfterElementStartingWord = new Dictionary<TokenType, WordProbabilitiesAfterElementStartingWord>();

        private TokenType lastElementStartingWord = TokenType.InvalidToken;
        private TokenType lastWord = TokenType.InvalidToken;

        private IDictionary<Token, SymbolInformation> symbolInformationForTokens;

        public void OnBeginProgram()
        {
            lastElementStartingWord = TokenType.InvalidToken;
            lastWord = TokenType.InvalidToken;
            symbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
        }
        
        public void AddSymbolInformationForTokens(IDictionary<Token, SymbolInformation> additionalSymbolInformationForTokens)
        {
            foreach (var pair in additionalSymbolInformationForTokens)
            {
                symbolInformationForTokens[pair.Key] = pair.Value;
            }
        }

        public void OnToken(Token token)
        {
            TokenType tokenType = token.TokenType;
            TokenFamily tokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            if (tokenFamily != TokenFamily.Whitespace && tokenFamily != TokenFamily.Comments && tokenType != TokenType.CompilerDirective &&
                tokenType != TokenType.EJECT && tokenType != TokenType.SKIP1 && tokenType != TokenType.SKIP2 && tokenType != TokenType.SKIP3)
            {
                RegisterToken(token);
                lastWord = tokenType;
                if (tokenFamily == TokenFamily.CompilerDirectiveStartingKeyword || tokenFamily == TokenFamily.CodeElementStartingKeyword || tokenFamily == TokenFamily.StatementStartingKeyword || tokenFamily == TokenFamily.StatementEndingKeyword)
                {
                    lastElementStartingWord = tokenType;
                }                                
            }
        }

        private void RegisterToken(Token token)
        {
            TotalCount++;
            WordProbabilitiesAfterElementStartingWord wordProbabilities = null;
            if (!WordProbabilitiesAfterElementStartingWord.TryGetValue(lastElementStartingWord, out wordProbabilities))
            {
                wordProbabilities = new WordProbabilitiesAfterElementStartingWord(lastElementStartingWord);
                WordProbabilitiesAfterElementStartingWord.Add(lastElementStartingWord, wordProbabilities);
            }

            SymbolInformation symbolInfo = null;
            symbolInformationForTokens.TryGetValue(token, out symbolInfo);
            wordProbabilities.OnWords(lastWord, token.TokenType, symbolInfo);
        }

        public void ComputeProbabilities()
        {
            foreach (var wordProbabilities in WordProbabilitiesAfterElementStartingWord.Values)
            {
                wordProbabilities.ComputeWordProbabilities();
            }
        }

        public IList<WordProbability> NextWordsProbability(TokenType lastElementStartingWordType, TokenType lastWordType)
        {
            WordProbabilitiesAfterElementStartingWord wordProbabilities = null;
            if (WordProbabilitiesAfterElementStartingWord.TryGetValue(lastElementStartingWord, out wordProbabilities))
            {
                return wordProbabilities.NextWordsProbability(lastWordType);
            }
            else
            {
                return null;
            }
        }

        public void DisplayResults(TextWriter writer)
        {
            double top1Prediction = 0;
            double top3Prediction = 0;
            double top5Prediction = 0;
            foreach (var wordProbabilities in WordProbabilitiesAfterElementStartingWord.Values.OrderByDescending(wordProbabilities => wordProbabilities.TotalCount))
            {
                double elementStartingWordProbability = wordProbabilities.TotalCount / (double)TotalCount;
                writer.WriteLine(Enum.GetName(typeof(TokenType), wordProbabilities.ElementStartingWordType) + "\t" + Math.Round(elementStartingWordProbability * 100,2 ));

                double top1PredictionAfterFirstWord = 0;
                double top3PredictionAfterFirstWord = 0;
                double top5PredictionAfterFirstWord = 0;
                foreach (var nextWordProbabilities in wordProbabilities.WordProbabilities.Values.OrderByDescending(nextWordProbabilities => nextWordProbabilities.TotalCount))
                {
                    double firstWordProbability = nextWordProbabilities.TotalCount/ (double)wordProbabilities.TotalCount;
                    writer.WriteLine("\t" + Enum.GetName(typeof(TokenType), nextWordProbabilities.CurrentWordType) + "\t" + Math.Round(firstWordProbability * 100, 2));

                    double top1PredictionForSecondWord = 0;
                    double top3PredictionForSecondWord = 0;
                    double top5PredictionForSecondWord = 0;
                    int topIndex = 0;
                    foreach (var wordProbability in nextWordProbabilities.NextWords)
                    {
                        topIndex++;
                        double secondWordProbability = wordProbability.Probability;
                        if(topIndex <= 1)
                        {
                            top1PredictionForSecondWord += secondWordProbability;
                        }
                        if (topIndex <= 3)
                        {
                            top3PredictionForSecondWord += secondWordProbability;
                        }
                        if (topIndex <= 5)
                        {
                            top5PredictionForSecondWord += secondWordProbability;
                        }

                        string symbolTypes = String.Empty;
                        if(wordProbability.SymbolTypes != null)
                        {
                            symbolTypes = "(";
                            bool isFirst = true;
                            foreach(var symbolType in wordProbability.SymbolTypes)
                            {
                                if(isFirst)
                                {
                                    isFirst = false;
                                }
                                else
                                {
                                    symbolTypes += ",";
                                }
                                symbolTypes += Enum.GetName(typeof(SymbolType), symbolType);
                            }
                            symbolTypes += ")";
                        }
                        writer.WriteLine("\t\t" + Enum.GetName(typeof(TokenType), wordProbability.WordType)  + symbolTypes + "\t" + Math.Round(secondWordProbability * 100, 2));
                    }
                    top1PredictionAfterFirstWord += firstWordProbability * top1PredictionForSecondWord;
                    top3PredictionAfterFirstWord += firstWordProbability * top3PredictionForSecondWord;
                    top5PredictionAfterFirstWord += firstWordProbability * top5PredictionForSecondWord;
                }
                top1Prediction += elementStartingWordProbability * top1PredictionAfterFirstWord;
                top3Prediction += elementStartingWordProbability * top3PredictionAfterFirstWord;
                top5Prediction += elementStartingWordProbability * top5PredictionAfterFirstWord;
            }
            writer.WriteLine("--- Model performances ---");
            writer.WriteLine("Next word is top 1 suggestion  " + Math.Round(top1Prediction * 100, 2) +"% of the time");
            writer.WriteLine("Next word in top 3 suggestions " + Math.Round(top3Prediction * 100, 2) + "% of the time");
            writer.WriteLine("Next word in top 5 suggestions " + Math.Round(top5Prediction * 100, 2) + "% of the time");
        }
    }

    class WordProbabilitiesAfterElementStartingWord
    {
        public TokenType ElementStartingWordType;

        public long TotalCount;
        public IDictionary<TokenType, NextWordProbabilities> WordProbabilities;

        public WordProbabilitiesAfterElementStartingWord(TokenType elementStartingWordType)
        {
            ElementStartingWordType = elementStartingWordType;
            WordProbabilities = new Dictionary<TokenType, NextWordProbabilities>();
        }

        public void OnWords(TokenType firstWord, TokenType secondWord, SymbolInformation secondWordSymbolInfo)
        {
            TotalCount++;
            NextWordProbabilities nextWordProbabilities = null;
            if (!WordProbabilities.TryGetValue(firstWord, out nextWordProbabilities))
            {
                nextWordProbabilities = new NextWordProbabilities(firstWord);
                WordProbabilities.Add(firstWord, nextWordProbabilities);
            }
            nextWordProbabilities.OnNextWord(secondWord, secondWordSymbolInfo);
        }

        public void ComputeWordProbabilities()
        {
            foreach (var nextWordProbabilities in WordProbabilities.Values)
            {
                nextWordProbabilities.ComputeNextWords();
            }
        }

        public IList<WordProbability> NextWordsProbability(TokenType lastWordType)
        {
            NextWordProbabilities wordProbabilities = null;
            if (WordProbabilities.TryGetValue(lastWordType, out wordProbabilities))
            {
                return wordProbabilities.NextWords;
            }
            else
            {
                return null;
            }
        }
    }
       
    class NextWordProbabilities
    {
        public TokenType CurrentWordType;

        public long TotalCount;
        private IDictionary<TokenType, long> NextWordCounts;
        private IDictionary<TokenType, IList<SymbolType>> NextWordSymbolTypes;

        public IList<WordProbability> NextWords;

        public NextWordProbabilities(TokenType currentTokenType)
        {
            CurrentWordType = currentTokenType;
            NextWordCounts = new Dictionary<TokenType, long>();            
        }

        public void OnNextWord(TokenType nextWordType, SymbolInformation nextWordSymbolInfo)
        {
            TotalCount++;
            if(NextWordCounts.ContainsKey(nextWordType))
            {
                NextWordCounts[nextWordType] = NextWordCounts[nextWordType] + 1;
            }
            else
            {
                NextWordCounts.Add(nextWordType, 1);
            }
            if(nextWordSymbolInfo != null)
            {
                if(NextWordSymbolTypes == null)
                {
                    NextWordSymbolTypes = new Dictionary<TokenType, IList<SymbolType>>();
                }
                IList<SymbolType> candidateSymbolTypes = null;
                if(!NextWordSymbolTypes.TryGetValue(nextWordType, out candidateSymbolTypes))
                {
                    candidateSymbolTypes = new List<SymbolType>();
                    NextWordSymbolTypes.Add(nextWordType, candidateSymbolTypes);
                }
                if(nextWordSymbolInfo.CandidateSymbolTypes != null)
                {
                    foreach(var symbolType in nextWordSymbolInfo.CandidateSymbolTypes)
                    {
                        if (!candidateSymbolTypes.Contains(symbolType))
                        {
                            candidateSymbolTypes.Add(symbolType);
                        }
                    }
                }
                else
                {
                    if (!candidateSymbolTypes.Contains(nextWordSymbolInfo.Type))
                    {
                        candidateSymbolTypes.Add(nextWordSymbolInfo.Type);
                    }
                }
            }
        }

        public void ComputeNextWords()
        {
            NextWords = new List<WordProbability>();
            foreach(var tokenType in NextWordCounts.Keys.OrderByDescending(tokenType => NextWordCounts[tokenType]))
            {
                long nextWordCount = NextWordCounts[tokenType];
                double probability = nextWordCount / (double)TotalCount;
                if(probability >= 0.05)
                {
                    SymbolType[] symbolTypes = null;
                    if(NextWordSymbolTypes != null)
                    {
                        if (NextWordSymbolTypes.ContainsKey(tokenType))
                        {
                            symbolTypes = NextWordSymbolTypes[tokenType].ToArray();
                        }
                    }
                    NextWords.Add(new WordProbability(tokenType, symbolTypes, probability));
                }
                else
                {
                    break;
                }
            }
            NextWordCounts = null;
        }
    }

    class WordProbability
    {
        public TokenType WordType;
        public SymbolType[] SymbolTypes;
        public double Probability;

        public WordProbability(TokenType wordType, SymbolType[] symbolTypes, double probability)
        {
            WordType = wordType;
            SymbolTypes = symbolTypes;
            Probability = probability;
        }
    }
}
