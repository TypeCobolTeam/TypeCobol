using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServices.CodeAnalysis.Statistics
{
    /// <summary>
    /// Class used to generate statistics about the syntax of a set of Cobol programs
    /// </summary>
    class StatsGenerator
    {
        public static void GenerateStatisticsForPrograms(CompilationProject project, IEnumerable<string> textNames, TextWriter console, string countersFile, string languageModelForProgramFile, string languageModelForCopyFile)
        {
            // Initialize statistics vars

            // 1. Program analysis after preprocessing

            // - total number of lines per program (including expanded COPY directives)
            long[] linesCountDistributionCategories = { 500, 1000, 1500, 2000, 3000, 5000, 7500, 10000, 15000, 20000, 30000, 50000, int.MaxValue };
            StatsCounter<CobolTextLineType> linesCounter = new StatsCounter<CobolTextLineType>(linesCountDistributionCategories);

            // - total number of tokens per program (after preprocessing)
            long[] tokensCountDistributionCategories = { 1500, 3000, 4500, 6000, 9000, 15000, 22500, 30000, 45000, 60000, 90000, 150000, int.MaxValue };
            StatsCounter<TokenType> tokensCounter = new StatsCounter<TokenType>(tokensCountDistributionCategories);

            // - number of copies per program
            long[] copiesCountDistributionCategories = { 0, 5, 10, 15, 20, 30, 50, int.MaxValue };
            StatsCounter<CopyDirectiveType> copiesCounter = new StatsCounter<CopyDirectiveType>(copiesCountDistributionCategories);

            // - number of replaced tokens per program
            long[] replacedTokensCountDistributionCategories = { 50, 100, 150, 200, 300, 500, 1000, 2000, 5000, 10000, 20000, int.MaxValue };
            StatsCounter<TokenType> replacedTokensCounter = new StatsCounter<TokenType>(tokensCountDistributionCategories);

            // - number of code elements per program
            long[] codeElementsCountDistributionCategories = { 100, 200, 300, 400, 500, 750, 1000, 1500, 2000, 5000, 10000, int.MaxValue };
            StatsCounter<CodeElementType> codeElementCounter = new StatsCounter<CodeElementType>(codeElementsCountDistributionCategories);

            // 2. Program files before preprocessing

            // - number of lines per program file (before preprocessing)
            long[] linesCountPerProgramFileDistributionCategories = { 25, 50, 100, 150, 200, 300, 500, 1000, 1500, 2000, 3000, int.MaxValue };
            StatsCounter<CobolTextLineType> linesPerProgramFileCounter = new StatsCounter<CobolTextLineType>(linesCountPerProgramFileDistributionCategories);

            // - number of tokens per program file (before preprocessing)
            long[] tokensCountPerProgramFileDistributionCategories = { 1500, 3000, 4500, 6000, 9000, 15000, 22500, 30000, 45000, 60000, 90000, 150000, int.MaxValue };
            StatsCounter<TokenType> tokensPerProgramFileCounter = new StatsCounter<TokenType>(tokensCountPerProgramFileDistributionCategories);

            // - number of compiler directives per program file
            long[] compilerDirectivesPerProgramFileCountDistributionCategories = { 0, 5, 10, 15, 20, 30, 50, 75, 100, 200, int.MaxValue };
            StatsCounter<CompilerDirectiveType> compilerDirectivesPerProgramFileCounter = new StatsCounter<CompilerDirectiveType>(compilerDirectivesPerProgramFileCountDistributionCategories);

            // 3. Copy files before preprocessing

            // - number of references to each copy file
            IDictionary<string, int> copyFileReferenceCount = new Dictionary<string, int>();

            // - number of lines per copy file (before preprocessing)
            long[] linesCountPerCopyFileDistributionCategories = { 25, 50, 100, 150, 200, 300, 500, 1000, 1500, 2000, 3000, int.MaxValue };
            StatsCounter<CobolTextLineType> linesPerCopyFileCounter = new StatsCounter<CobolTextLineType>(linesCountPerCopyFileDistributionCategories);

            // - number of tokens per copy file (before preprocessing)
            long[] tokensCountPerCopyFileDistributionCategories = { 1500, 3000, 4500, 6000, 9000, 15000, 22500, 30000, 45000, 60000, 90000, 150000, int.MaxValue };
            StatsCounter<TokenType> tokensPerCopyFileCounter = new StatsCounter<TokenType>(tokensCountPerCopyFileDistributionCategories);

            // - number of compiler directives per copy file
            long[] compilerDirectivesPerCopyFileCountDistributionCategories = { 0, 5, 10, 15, 20, 30, 50, 75, 100, 200, int.MaxValue };
            StatsCounter<CompilerDirectiveType> compilerDirectivesPerCopyFileCounter = new StatsCounter<CompilerDirectiveType>(compilerDirectivesPerCopyFileCountDistributionCategories);

            // 4. Language models

            // - language model to predict the next word in a program
            LanguageModelGenerator languageModelForProgram = new LanguageModelGenerator();

            // - language model to predict the next word in a copy
            LanguageModelGenerator languageModelForCopy = new LanguageModelGenerator();

            // -- Compile and compute stats --

            foreach(string textName in textNames)
            { 
                    console.Write(textName + " : compilation ... ");

                    int programCopiesNotFound = 0;
                    try
                    {
                        // Compile program
                        FileCompiler fileCompiler = new FileCompiler(null, textName, project.SourceFileProvider, project, project.ColumnsLayout, project.CompilationOptions.Clone(), null, false, project);
                        fileCompiler.CompileOnce();
                        CompilationUnit compilationResult = fileCompiler.CompilationResultsForProgram;
                        programCopiesNotFound = 0;

                        // Compute stats
                        console.Write(" OK, compute stats ... ");

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
                            if (line.ImportedDocuments != null)
                            {
                                var symbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
                                foreach (var copyDirective in line.ImportedDocuments.Keys)
                                {
                                    if (copyDirective.TextNameSymbol != null)
                                    {
                                        symbolInformationForTokens.Add(copyDirective.TextNameSymbol, new ExternalName(new AlphanumericValue(copyDirective.TextNameSymbol), SymbolType.TextName));
                                    }
                                    if (copyDirective.LibraryNameSymbol != null)
                                    {
                                        symbolInformationForTokens.Add(copyDirective.TextNameSymbol, new ExternalName(new AlphanumericValue(copyDirective.LibraryNameSymbol), SymbolType.LibraryName));
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
#if EUROINFO_RULES
                                if (compilationResult.CompilerOptions.UseEuroInformationLegacyReplacingSyntax)
                                {
                                    if (copyDirective.InsertSuffixChar)
                                    {
                                        copyDirectiveType = CopyDirectiveType.CopyReplacingRemarks;
                                    }
                                    else if (copyDirective.RemoveFirst01Level)
                                    {
                                        copyDirectiveType = CopyDirectiveType.CopyRemarks;
                                    }
                                }
                                    
#endif
                                    if (copyDirective.ReplaceOperations != null && copyDirective.ReplaceOperations.Count > 0)
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
                                                            symbolInformationForTokens.Add(copyDirective2.TextNameSymbol, new ExternalName(new AlphanumericValue(copyDirective2.TextNameSymbol), SymbolType.TextName));
                                                        }
                                                        if (copyDirective2.LibraryNameSymbol != null)
                                                        {
                                                            symbolInformationForTokens.Add(copyDirective2.TextNameSymbol, new ExternalName(new AlphanumericValue(copyDirective2.LibraryNameSymbol), SymbolType.LibraryName));
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
                        ITokensLinesIterator processedTokensIterator = compilationResult.ProcessedTokensDocumentSnapshot.GetProcessedTokensIterator();
                        Token processedToken = null;
                        while ((processedToken = processedTokensIterator.NextToken()) != Token.END_OF_FILE)
                        {
                            tokensCounter.OnElement((int)processedToken.TokenType);
                            ReplacedToken replacedToken = processedToken as ReplacedToken;
                            if (replacedToken != null)
                            {
                                replacedTokensCounter.OnElement((int)replacedToken.OriginalToken.TokenType);
                            }
                            else if (processedToken is ReplacedTokenGroup)
                            {
                                replacedTokensCounter.OnElement((int)TokenType.CONTINUATION_TOKEN_GROUP);
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
                        console.WriteLine("ERROR :");
                        console.WriteLine(e.Message);
                    }
                    finally
                    {
                        console.Write("FINISHED");
                        if (programCopiesNotFound == 0)
                        {
                            console.WriteLine();
                        }
                        else
                        {
                            console.WriteLine(" (" + programCopiesNotFound + " missing COPY)");
                        }
                    }
                }

                // Compute language models
                languageModelForProgram.ComputeProbabilities();
                languageModelForCopy.ComputeProbabilities();

                // Write results files
                
            console.WriteLine("");

            console.WriteLine("Writing statistics results to " + countersFile);
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
            console.WriteLine("Done");
            
            console.WriteLine("Writing language model for program to " + languageModelForProgramFile);
            using (StreamWriter writer = new StreamWriter(languageModelForProgramFile))
            {
                languageModelForProgram.WriteModelFile(writer, console);
            }

            console.WriteLine("Writing language model for copy to " + languageModelForCopyFile);
            using (StreamWriter writer = new StreamWriter(languageModelForCopyFile))
            {
                languageModelForCopy.WriteModelFile(writer, console);
            }
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
}
