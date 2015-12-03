using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Incrementally parse CodeElements from a set of pre-processed tokens changes
    /// </summary>
    static class CodeElementsParserStep
    {
        /// <summary>
        /// Initial parsing of a complete document
        /// </summary>
        internal static void ParseDocument(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> documentLines, TypeCobolOptions compilerOptions)
        {
            ParseProcessedTokensLinesChanges(textSourceInfo, documentLines, null, null, compilerOptions);
        }

        /// <summary>
        /// Incremental parsing of a set of processed tokens lines changes
        /// </summary>
        internal static IList<DocumentChange<ICodeElementsLine>> ParseProcessedTokensLinesChanges(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> documentLines, IList<DocumentChange<IProcessedTokensLine>> processedTokensLinesChanges, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions)
        {
            // Collect all changes applied to the processed tokens lines during the incremental scan
            IList<DocumentChange<ICodeElementsLine>> codeElementsLinesChanges = new List<DocumentChange<ICodeElementsLine>>();

            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // TO DO : Implement the incremental parsing with COPY & REPLACE
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            // TO DO -- Remove this block of code after implementing incremental parsing of CodeElements
            // Here, we do something incorrect in a multithread environment 
            // => we update the content of IMMUTABLE lines to reset all previously parsed CodeElements
            foreach(CodeElementsLine immutableLineShouldNotBeChanged in documentLines)
            {
                if (immutableLineShouldNotBeChanged.CodeElements != null) immutableLineShouldNotBeChanged.CodeElements.Clear();
                if (immutableLineShouldNotBeChanged.ParserDiagnostics != null) immutableLineShouldNotBeChanged.ParserDiagnostics.Clear();
            }
            // -- END

            // Create a token iterator on top of pre-processed tokens lines
            ITokensLinesIterator tokensIterator = ProcessedTokensDocument.GetProcessedTokensIterator(textSourceInfo, documentLines);

            // Create an Antlr compatible token source on top a the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                textSourceInfo.Name,
                tokensIterator);

            // Init parser
            ITokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            TracingCobolParser cobolParser = new TracingCobolParser(tokenStream);
            // -> activate full ambiguities detection
            //parser.Interpreter.PredictionMode = PredictionMode.LlExactAmbigDetection; 
            IAntlrErrorStrategy cobolErrorStrategy = new CobolErrorStrategy();
            cobolParser.ErrorHandler = cobolErrorStrategy;

            // Register all parse errors in a list in memory
            DiagnosticSyntaxErrorListener errorListener = new DiagnosticSyntaxErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Prepare to analyze the parse tree
            ParseTreeWalker walker = new ParseTreeWalker();
            CodeElementBuilder codeElementBuilder = new CodeElementBuilder();

            // TO DO -- Iterate only over the code elements which need to be refreshed
            do
            {
                // TO DO -- Seek just before the next code element starting token
                // tokensIterator.SeekToToken(codeElementStartingToken);

                // !! Do not reset the Antlr BufferedTokenStream position while iterating on adjacent code elements
                //tokenStream.SetTokenSource(tokenSource);

                // Reset parsing error diagnostics
                //cobolErrorStrategy.Reset(cobolParser);
                errorListener.Diagnostics.Clear();

                // Reset parser traces (consumed tokens)
                cobolParser.ResetTraces();

                // Try to parse a code element starting with the current token
                CobolCodeElementsParser.CodeElementContext codeElementParseTree = cobolParser.codeElement();

                // If the parse tree is not empty
                if (codeElementParseTree.Start.Type > 0)
                {
                    // Get the first line that was parsed                
                    CodeElementsLine codeElementsLine = ((CodeElementsLine)((Token)codeElementParseTree.Start).TokensLine);

                    // Visit the parse tree to build a first class object representing the code elements
                    walker.Walk(codeElementBuilder, codeElementParseTree);
                    CodeElement codeElement = codeElementBuilder.CodeElement;
                    if (codeElement != null)
                    {
                        // Attach consumed tokens and main document line numbers information to the code element
                        codeElement.ConsumedTokens = cobolParser.ConsumedTokens;

                        // Add code element to the list                    
                        codeElementsLine.AddCodeElement(codeElement);
                        foreach (ParserDiagnostic d in errorListener.Diagnostics)
                        {
                            codeElement.Diagnostics.Add(d);
                        }
                        foreach (Diagnostic d in codeElement.Diagnostics)
                        {
                            codeElementsLine.AddParserDiagnostic(d);
                        }
                    }
                    else
                    {
                        foreach (ParserDiagnostic d in errorListener.Diagnostics)
                        {
                            codeElementsLine.AddParserDiagnostic(d);
                        }
                    }
                }
            }
            while (tokenStream.La(1) >= 0);

            return codeElementsLinesChanges;
        }

        /// <summary>
        /// Iterator over the tokens contained in the parameter "lines" after
        /// - COPY directives text imports
        /// - REPLACE directive token remplacements
        /// </summary>
        public static ITokensLinesIterator GetProcessedTokensIterator(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<IProcessedTokensLine> lines)
        {
            ITokensLinesIterator copyIterator = new CopyTokensLinesIterator(textSourceInfo.Name, lines, Token.CHANNEL_SourceTokens);
            ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(copyIterator);
            return replaceIterator;
        }
    }
}
