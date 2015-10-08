using System;
using System.Collections.Generic;
using System.Reactive.Concurrency;
using System.Reactive.Linq;
using System.Text;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Partial compilation pipeline : from file to preprocessor, used for COPY text
    /// </summary>
    public class CompilationDocument
    {
        /// <summary>
        /// Cobol source file
        /// </summary>
        public CobolFile CobolFile { get; private set; }

        /// <summary>
        /// Raw source text
        /// </summary>
        public ITextDocument TextDocument { get; private set; }

        /// <summary>
        /// True if the current source text in TextDocument is different from the CobolFile content 
        /// </summary>
        public bool HasSourceTextChangesToSave { get; private set; }

        /// <summary>
        /// View of a source document as lines of tokens after the lexical analysis stage 
        /// </summary>
		public TokensDocument TokensDocument { get; private set; }

        /// <summary>
        /// View of a source document after COPY and REPLACE processing
        /// </summary>
        public ProcessedTokensDocument ProcessedTokensDocument { get; private set; }

        /// <summary>
        /// List of CompilationDocuments imported by COPY directives in the current document
        /// </summary>
        public IList<CompilationDocument> CopyReferences { get; private set; }        
        
        /// <summary>
        /// Used to find the secondary files imported by the COPY statement 
        /// </summary>
        protected SourceFileProvider sourceFileProvider;

        /// <summary>
        /// Compiler options directing the scanner and preprocessor operations
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// List of error messages produced while analyzing this document
        /// </summary>
        public IEnumerable<Diagnostic> Diagnostics 
        { 
            get 
            {
                IList<Diagnostic> diagnostics = new List<Diagnostic>();
                foreach(ITokensLine line in TokensDocument.TokensLines)
                {
                    if (line.ScannerDiagnostics != null && line.ScannerDiagnostics.Count > 0)
                    {
                        foreach (Diagnostic diagnostic in line.ScannerDiagnostics)
                        {
                            diagnostics.Add(diagnostic);
                        }
                    }
                }
                return diagnostics;
            } 
        }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public CompilationDocument(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            CompilerOptions = compilerOptions;

            // 1. Find the Cobol source file
            CobolFile sourceFile;
            if(sourceFileProvider.TryGetFile(libraryName, textName, out sourceFile))
            {
                CobolFile = sourceFile;
            }
            else
            {
                throw new Exception(String.Format("Could not find a Cobol source file named {0}", textName));
            }

            // 2. Load it in a new text document in memory
            TextDocument = new ReadOnlyTextDocument(sourceFile.Name, sourceFile.Encoding, columnsLayout, sourceFile.ReadChars());

            // 3. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, compilerOptions);

            // 4. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Load a Cobol source file in an existing text document
        /// </summary>
        public CompilationDocument(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ITextDocument textDocument, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            CompilerOptions = compilerOptions;

            // 1. Find the Cobol source file
            CobolFile sourceFile;
            if (sourceFileProvider.TryGetFile(libraryName, textName, out sourceFile))
            {
                CobolFile = sourceFile;
            }
            else
            {
                throw new Exception(String.Format("Could not find a Cobol source file named {0}", textName));
            }

            // 2. Load it in a new text document in memory
            TextDocument = textDocument;
            textDocument.LoadChars(sourceFile.ReadChars());

            // 3. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, CompilerOptions);

            // 4. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Initialize the compilation document from an existing text document, not yet associated with a Cobol file
        /// </summary>
        public CompilationDocument(ITextDocument textDocument, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            TextDocument = textDocument;
            CompilerOptions = compilerOptions;

            // 1. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, CompilerOptions);

            // 2. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Initialize the compilation document from an existing text document, already initialized from a Cobol file
        /// </summary>
        public CompilationDocument(ITextDocument textDocument, CobolFile cobolFile, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            CobolFile = cobolFile;
            TextDocument = textDocument;
            CompilerOptions = compilerOptions;

            // 1. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, CompilerOptions);

            // 2. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Configure on which thread and at which frequency the preprocessor analyzes document changes
        /// </summary>
        public void SetupDocumentProcessingPipeline(IScheduler backgroundParsingScheduler, int bufferingDelayMillisecond)
        {
            // Scanner always operates on the same thread as TextDocument notifications
            TextDocument.TextChangedEventsSource.Subscribe(TokensDocument);

            // Mono thread configuration : Preprocessor operates on the same thread as TextDocument notifications
            if (backgroundParsingScheduler == null)
            {
                TokensDocument.TokensChangedEventsSource.Subscribe(ProcessedTokensDocument);
            }
            // Multi-thread configuration : Preprocessor operates independently on a background thread
            else
            {
                TokensDocument.TokensChangedEventsSource.Buffer(TimeSpan.FromMilliseconds(bufferingDelayMillisecond)).Where(evts => evts.Count > 0).
                    ObserveOn(backgroundParsingScheduler).Select(eventsList => TokensChangedEvent.Flatten(eventsList)).Subscribe(ProcessedTokensDocument);
            }
        }

        /// <summary>
        /// Scan and preprocess document changes
        /// </summary>
        public virtual void StartDocumentProcessing()
        {
            // Start compilation process
            TextDocument.StartSendingChangeEvents();
        }

        /// <summary>
        /// Monitor changes to the source Cobol file and automatically update TextDocument contents after each file update
        /// </summary>
        public virtual void StartContinuousFileProcessing()
        {
            // Reload text document each time an external change is applied to the cobol file
            ObserverTextDocument observerTextDocument = new ObserverTextDocument(CobolFile, TextDocument);
            CobolFile.CobolFileChangedEventsSource.Subscribe(observerTextDocument);

            // Start compilation process
            TextDocument.StartSendingChangeEvents();
            CobolFile.StartMonitoringExternalChanges();
        }

        /// <summary>
        /// Reload the entire file content in the text document each time the file is updated
        /// </summary>
        private class ObserverTextDocument : IObserver<CobolFileChangedEvent>
        {
            private CobolFile cobolFile;
            private ITextDocument textDocument;

            public ObserverTextDocument(CobolFile cobolFile, ITextDocument textDocument)
            {
                this.cobolFile = cobolFile;
                this.textDocument = textDocument;
            }

            public void OnNext(CobolFileChangedEvent value)
            {
                if (value.Type == CobolFileChangeType.FileChanged)
                {
                    textDocument.LoadChars(cobolFile.ReadChars());
                }
                else
                {
                    throw new InvalidOperationException("File change type " + value.Type + " is not supported in this configuration");
                }
            }

            public void OnCompleted()
            {
                // Do nothing
            }

            public void OnError(Exception error)
            {
                // Do nothing
            }
        }
    }

    /// <summary>
    /// Class used to group all the properties needed to load a CompilationDocument from a file
    /// </summary>
    public class DocumentFormat
    {
        public DocumentFormat(Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength, ColumnsLayout columnsLayout)
        {
            Encoding = encoding; 
            EndOfLineDelimiter = endOfLineDelimiter;
            if (endOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
            {
                if (columnsLayout == ColumnsLayout.FreeTextFormat)
                {
                    throw new ArgumentException("With free text format, fixed length lines are not allowed");
                }
                else if (columnsLayout == ColumnsLayout.CobolReferenceFormat && fixedLineLength < 72)
                {
                    throw new ArgumentException("With Cobol reference format, fixed length lines must be at least 72 characters long");
                }
                FixedLineLength = fixedLineLength;
            }
            ColumnsLayout = columnsLayout;
        }

        /// <summary>
        /// Encoding used to read or write the Cobol file
        /// </summary>
        public Encoding Encoding { get; protected set; }

        /// <summary>
        /// Tells the compiler how to split a character stream in consecutive lines
        /// </summary>
        public EndOfLineDelimiter EndOfLineDelimiter { get; private set; }

        /// <summary>
        /// Used only in case the text is formatted with fixed length line
        /// </summary>
        public int FixedLineLength { get; private set; }

        /// <summary>
        /// Tells the compiler how to interpret the column position of each character in the source text
        /// </summary>
        public ColumnsLayout ColumnsLayout { get; private set; }

        // Most often used document formats
        public static DocumentFormat ZOsReferenceFormat = new DocumentFormat(IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat);
        public static DocumentFormat RDZReferenceFormat = new DocumentFormat(Encoding.GetEncoding(1252) , EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat);
        public static DocumentFormat FreeTextFormat = new DocumentFormat(Encoding.GetEncoding(1252), EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
        public static DocumentFormat FreeUTF8Format = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
    }
}
