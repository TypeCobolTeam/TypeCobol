using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Generator;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Complete compilation pipeline : from file to syntax tree and code model, used for programs or classes
    /// </summary>
    public class CompilationUnit : CompilationDocument
    {
        /// <summary>
        /// Syntax tree produced by the parsing phase
        /// </summary>
        public SyntaxDocument SyntaxDocument     { get; private set; }

        /// <summary>
        /// Object oriented model of the Cobol program
        /// </summary>
        public SemanticsDocument SemanticsDocument      { get; private set; }

        /// <summary>     
        /// List of all external programs called from the current program 
        /// </summary>
        public IList<CompilationUnit> CallReferences { get; private set; }

        /// <summary>
        /// TextDocument where the target Cobol code will be generated
        /// </summary>
        public ITextDocument GeneratedTextDocument { get; private set; }

        /// <summary>
        /// True if the current generated text in GeneratedTextDocument is different from the GeneratedCobolFile content 
        /// </summary>
        public bool HasGeneratedTextChangesToSave { get; private set; }

        /// <summary>
        /// CobolFile used to save the results of the Cobol code generation
        /// </summary>
        public CobolFile GeneratedCobolFile { get; private set; }

        /// <summary>
        /// Compiler options 
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public CompilationUnit(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, TypeCobolOptions compilerOptions) :
            base(libraryName, textName, sourceFileProvider, documentProvider, columnsLayout, compilerOptions)
        {
            CompilerOptions = compilerOptions;

            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Load a Cobol source file in an existing text document
        /// </summary>
        public CompilationUnit(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ITextDocument textDocument, TypeCobolOptions compilerOptions) :
            base(libraryName, textName, sourceFileProvider, documentProvider, textDocument, compilerOptions)
        {
            CompilerOptions = compilerOptions;

            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Initialize the compilation unit from an existing text document, not yet associated with a Cobol file
        /// </summary>
        public CompilationUnit(ITextDocument textDocument, Encoding encodingForHexadecimalAlphanumericLiterals, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions) :
            base(textDocument, encodingForHexadecimalAlphanumericLiterals, sourceFileProvider, documentProvider, compilerOptions)
        {
            CompilerOptions = compilerOptions;

            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Initialize the compilation unit from an existing text document, already initialized from a Cobol file
        /// </summary>
        public CompilationUnit(ITextDocument textDocument, CobolFile cobolFile, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions) :
            base(textDocument, cobolFile, sourceFileProvider, documentProvider, compilerOptions)
        {
            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Configure the compilation unit to only parse the source and build a code model, without exécuting the code generation phase
        /// </summary>
        public void SetupCodeAnalysisPipeline(IScheduler backgroundParsingScheduler, int bufferingDelayMillisecond)
        {
            base.SetupDocumentProcessingPipeline(backgroundParsingScheduler, bufferingDelayMillisecond); 

            // Parser always operates on the same thread as Preprocessor
            ProcessedTokensDocument.TokensChangedEventsSource.Subscribe(SyntaxDocument);

            // TypeChecker alway operates on the same thread as Parser
            SyntaxDocument.ParseNodeChangedEventsSource.Subscribe(SemanticsDocument);
        }

        /// <summary>
        /// Configure the compilation unit to parse the source, build a code model, and then generate Cobol code
        /// </summary>
        public void SetupTextGenerationPipeline(IScheduler backgroundParsingScheduler, int bufferingDelayMillisecond, IScheduler textGenerationScheduler)
        {
            SetupCodeAnalysisPipeline(backgroundParsingScheduler, bufferingDelayMillisecond);

            // TypeCobolGenerator always operates on the same thread as TypeChecker
            TypeCobolGenerator generator = new TypeCobolGenerator(SemanticsDocument, GeneratedTextDocument);

            // Mono thread configuration : Generated code is written to the TextDocument in the same thread
            if (textGenerationScheduler == null)
            {
                SemanticsDocument.CodeModelChangedEventsSource.Subscribe(generator);
            }
            // Multi-thread configuration : generator could need to update the TextDocument on the UI thread
            else
            {
                SemanticsDocument.CodeModelChangedEventsSource.
                    ObserveOn(textGenerationScheduler).Subscribe(generator);
            }
        } 
    }
}
