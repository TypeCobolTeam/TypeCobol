using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.TypeChecker;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Incremental parsing of a ProcessedTokensDocument
    /// </summary>
    public class SyntaxDocument : IObserver<TokensChangedEvent>
    {
        /// <summary>
        /// Underlying ProcessedTokensDocument
        /// </summary>
        public ProcessedTokensDocument ProcessedTokensDocument { get; private set; }

        /// <summary>
        /// Compiler options directing the parser operations
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// List of code elements found in the parse tree
        /// </summary>
        public IList<CodeElement> CodeElements { get; private set; }

        /// <summary>
        /// List of errors found while parsing the program
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }

        public SyntaxDocument(ProcessedTokensDocument processedTokensDocument, TypeCobolOptions compilerOptions)
        {
            ProcessedTokensDocument = processedTokensDocument;
            CompilerOptions = compilerOptions;
        }

        // -- Should incrementally parse a set of tokens changes --

        public void OnNext(TokensChangedEvent tokensChangedEvent)
        {
            // Create a token iterator on top of pre-processed tokens lines
            ITokensLinesIterator tokensIterator = ProcessedTokensDocument.GetTokensIterator();

            // Create an Antlr compatible token source on top a the token iterator
            TokensDocumentTokenSource tokenSource = new TokensDocumentTokenSource(
                ProcessedTokensDocument.TokensDocument.TextDocument,
                tokensIterator);

            // Init parser
            CommonTokenStream tokenStream = new CommonTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            TracingCobolParser cobolParser = new TracingCobolParser(tokenStream);
            // -> activate full ambiguities detection
            //parser.Interpreter.PredictionMode = PredictionMode.LlExactAmbigDetection; 
            IAntlrErrorStrategy cobolErrorStrategy = new CobolErrorStrategy();
            cobolParser.ErrorHandler = cobolErrorStrategy;

            // Reset the erors list
            Diagnostics = new List<Diagnostic>();

            // Register all parse errors in a list in memory
            DiagnosticSyntaxErrorListener errorListener = new DiagnosticSyntaxErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Prepare to analyze the parse tree
            ParseTreeWalker walker = new ParseTreeWalker();
            CodeElementBuilder codeElementBuilder = new CodeElementBuilder();

            // TO DO -- Iterate over the code elements which need to be refreshed
            CodeElements = new List<CodeElement>();
            CodeElement codeElement = null;
            do
            {
                // TO DO -- Seek just before the next code element starting token
                // tokensIterator.SeekToToken(codeElementStartingToken);

                // !! Do not reset the Antlr BufferedTokenStream position while iterating on adjacent code elements
                //tokenStream.SetTokenSource(tokenSource);

                // Reset parsing error diagnostics
                cobolErrorStrategy.Reset(cobolParser);
                errorListener.Diagnostics.Clear();

                // Reset parser traces (consumed tokens)
                cobolParser.ResetTraces();

                // Try to parse a code element starting with the current token
                CobolParser.CodeElementContext codeElementParseTree = cobolParser.codeElement();

                // Visit the parse tree to build a first class object representing the compiler directive
                walker.Walk(codeElementBuilder, codeElementParseTree);
                codeElement = codeElementBuilder.CodeElement;
                if (codeElement != null)
                {
                    // Attach consumed tokens and main document line numbers information to the code element
                    codeElement.ConsumedTokens = cobolParser.ConsumedTokens;
                    codeElement.FirstTokenLineIndexInMainDocument = cobolParser.FirstTokenLineIndexInMainDocument;
                    codeElement.LastTokenLineIndexInMainDocument = cobolParser.LastTokenLineIndexInMainDocument;

                    // Register compiler directive parse errors
                    bool errorFoundWhileParsingCodeElement = errorListener.Diagnostics.Count > 0 || codeElementBuilder.Diagnostics.Count > 0;
                    if (errorFoundWhileParsingCodeElement)
                    {
                        foreach (ParserDiagnostic parserDiag in errorListener.Diagnostics)
                        {
                            Diagnostics.Add(parserDiag);
                        }
                        foreach (Diagnostic codeElementDiag in codeElementBuilder.Diagnostics)
                        {
                            Diagnostics.Add(codeElementDiag);
                        }
                    }
                }
            }
            while (codeElement != null);

            // Trigger ParseNodeChanged event
            ParseNodeChangedEvent parseEvent = new ParseNodeChangedEvent();
            parseNodeChangedEventsSource.OnNext(parseEvent);
        }

        public void OnCompleted()
        {
            // to do
        }

        public void OnError(Exception e)
        {
            // to do
        }

        // --- Implement IObservable<ParseNodeChangedEvent>

        private ISubject<ParseNodeChangedEvent> parseNodeChangedEventsSource = new Subject<ParseNodeChangedEvent>();

        public IObservable<ParseNodeChangedEvent> ParseNodeChangedEventsSource
        {
            get { return parseNodeChangedEventsSource; }
        }
    }
}

 
