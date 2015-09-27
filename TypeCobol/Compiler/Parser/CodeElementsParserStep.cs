using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Incrementally parse CodeElements from a set of pre-processed tokens changes
    /// </summary>
    static class CodeElementsParserStep
    {
        public void OnNext(TokensChangedEvent tokensChangedEvent)
        {

            try
            {
                // Create a token iterator on top of pre-processed tokens lines
                ITokensLinesIterator tokensIterator = ProcessedTokensDocument.GetTokensIterator();

                // Create an Antlr compatible token source on top a the token iterator
                TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                    ProcessedTokensDocument.TokensDocument.TextSourceInfo.Name,
                    tokensIterator);

                // Init parser
                ITokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
                TracingCobolParser cobolParser = new TracingCobolParser(tokenStream);
                // -> activate full ambiguities detection
                //parser.Interpreter.PredictionMode = PredictionMode.LlExactAmbigDetection; 
                IAntlrErrorStrategy cobolErrorStrategy = new CobolErrorStrategy();
                cobolParser.ErrorHandler = cobolErrorStrategy;

                // Reset the erors list
                CodeElementsInError = new List<CodeElement>();
                Diagnostics = new List<Diagnostic>();//TODO remove this

                // Register all parse errors in a list in memory
                DiagnosticSyntaxErrorListener errorListener = new DiagnosticSyntaxErrorListener();
                cobolParser.RemoveErrorListeners();
                cobolParser.AddErrorListener(errorListener);

                // Prepare to analyze the parse tree
                ParseTreeWalker walker = new ParseTreeWalker();
                CodeElementBuilder codeElementBuilder = new CodeElementBuilder();

                // TO DO -- Iterate over the code elements which need to be refreshed
                CodeElements = new List<CodeElement>();
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

                    // Visit the parse tree to build a first class object representing the code elements
                    walker.Walk(codeElementBuilder, codeElementParseTree);
                    CodeElement codeElement = codeElementBuilder.CodeElement;
                    if (codeElement != null)
                    {
                        // Attach consumed tokens and main document line numbers information to the code element
                        codeElement.ConsumedTokens = cobolParser.ConsumedTokens;
                        codeElement.FirstTokenLineIndexInMainDocument = cobolParser.FirstTokenLineIndexInMainDocument;
                        codeElement.LastTokenLineIndexInMainDocument = cobolParser.LastTokenLineIndexInMainDocument;

                        // Add code element to the list
                        CodeElements.Add(codeElement);
                        if (codeElement.Diagnostics.Count > 0)
                        {
                            CodeElementsInError.Add(codeElement);
                            Diagnostics.AddRange(codeElement.Diagnostics); //TODO remove
                            //Console.WriteLine("Added CodeElement with " + codeElement.Diagnostics.Count + " error(s) ; \"old style\" errors=" + errorListener.Diagnostics.Count);
                        }
                    }

                    // Register compiler directive parse errors
                    foreach (ParserDiagnostic parserDiag in errorListener.Diagnostics)
                    {
                        Diagnostics.Add(parserDiag);
                    }
                }
                while (tokenStream.La(1) >= 0);

                // Trigger ParseNodeChanged event
                CodeElementChangedEvent parseEvent = new CodeElementChangedEvent();
                parseNodeChangedEventsSource.OnNext(parseEvent);
            }
            catch (Exception ex)
            {
                // Register and forward errors
                LastException = ex;
                parseNodeChangedEventsSource.OnError(ex);
            }
        }

        public void OnCompleted()
        {
            // to do
        }

        public void OnError(Exception e)
        {
            // to do
        }
    }
}
