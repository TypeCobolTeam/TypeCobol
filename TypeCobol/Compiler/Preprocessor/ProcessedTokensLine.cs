using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Line of tokens after preprocessor execution
    /// </summary>
    public class ProcessedTokensLine : TokensLine, IProcessedTokensLine
    {
        internal ProcessedTokensLine(ITextLine textLine, ColumnsLayout columnsLayout) : base(textLine, columnsLayout)
        {
            PreprocessingState = PreprocessorState.NeedsCompilerDirectiveParsing;
        }

        // --- Computed line properties after preprocessor execution ---

        internal enum PreprocessorState
        {
            NeedsCompilerDirectiveParsing,
            NeedsCopyDirectiveProcessing,
            Ready
        }

        /// <summary>
        /// True if the preprocessor has not treated this line yet
        /// and all the following properties have not been set
        /// </summary>
        internal PreprocessorState PreprocessingState { get; set; }

        /// <summary>
        /// Tokens produced after parsing the compiler directives.
        /// If a compiler directive is found, several tokens of the source text are grouped 
        /// into one single CompilerDirectiveToken (which can be continued on several lines).
        /// </summary>
        public IList<Token> TokensWithCompilerDirectives { 
            get 
            {
                if (PreprocessingState <= PreprocessorState.NeedsCompilerDirectiveParsing)
                {
                    throw new InvalidOperationException("Compiler directives on this line have not been parsed yet");
                }
                if (tokensWithCompilerDirectives == null)
                {
                    return SourceTokens;
                }
                else
                {
                    return tokensWithCompilerDirectives;
                }
            } 
        }

        // Only needed if several source tokens must be grouped into a single CompilerDirectiveToken
        private IList<Token> tokensWithCompilerDirectives;

        /// <summary>
        /// True if compiler directives have been recognized on the current line
        /// (true on each line for multiline compiler directives)
        /// </summary>
        public bool HasCompilerDirectives { get { return tokensWithCompilerDirectives != null; } }
        
        /// <summary>
        /// Compiler listing control directive found on the current line
        /// *CBL or *CONTROL, EJECT, SKIP1 or SKIP2 or SKIP3, TITLE
        /// (these compiler directives can't span several lines, and you can only write one of them per line)
        /// </summary>
        public CompilerDirective CompilerListingControlDirective { get; private set; }
        
        /// <summary>
        /// Imported compilation documents for each COPY directive found (starting) on this line
        /// </summary>
        public IDictionary<CopyDirective, ImportedTokensDocument> ImportedDocuments { get; private set; }
        
        /// <summary>
        /// Last REPLACE compiler directive found on this this line
        /// </summary>
        public ReplaceDirective ReplaceDirective { get; private set; }

        internal TokensGroup InsertCompilerDirectiveTokenOnFirstLine(IList<Token> tokensOnFirstLineBeforeCompilerDirective, CompilerDirective compilerDirective, bool hasError, IList<Token> compilerDirectiveTokensOnFirstLine, IList<Token> tokensOnFirstLineAfterCompilerDirective, bool hasDirectiveTokenContinuedOnNextLine)
        {
            // Register compiler listing control directive
            if( compilerDirective.Type == CompilerDirectiveType.ASTERISK_CBL ||
                compilerDirective.Type == CompilerDirectiveType.ASTERISK_CONTROL ||
                compilerDirective.Type == CompilerDirectiveType.EJECT ||
                compilerDirective.Type == CompilerDirectiveType.SKIP1 ||
                compilerDirective.Type == CompilerDirectiveType.SKIP2 ||
                compilerDirective.Type == CompilerDirectiveType.SKIP3 ||
                compilerDirective.Type == CompilerDirectiveType.TITLE)
            {
                CompilerListingControlDirective = compilerDirective;
            }

            // Register COPY directives
            // Prepare dictionary for COPY imported documents
            if( compilerDirective.Type == CompilerDirectiveType.COPY ||
                compilerDirective.Type == CompilerDirectiveType.EXEC_SQL_INCLUDE)
            {
                if(ImportedDocuments == null)
                {
                    ImportedDocuments = new Dictionary<CopyDirective, ImportedTokensDocument>(1);
                }
                ImportedDocuments.Add((CopyDirective)compilerDirective, null);
            }

            // Register REPLACE compiler directive
            if(compilerDirective.Type == CompilerDirectiveType.REPLACE)
            {
                ReplaceDirective = (ReplaceDirective)compilerDirective;
            }

            // Initialize tokensWithCompilerDirectives
            // (first compiler directive found on this line)
            if (!HasCompilerDirectives)
            {
                // Initialize tokens list
                tokensWithCompilerDirectives = new List<Token>();

                // Keep tokens before compiler directive
                if (tokensOnFirstLineBeforeCompilerDirective != null)
                {
                    foreach (Token token in tokensOnFirstLineBeforeCompilerDirective)
                    {
                        tokensWithCompilerDirectives.Add(token);
                    }
                }  
            }
            // Update tokensWithCompilerDirectives
            // (several compiler directives on the same line)
            else
            {
                // Reset tokens list
                IList<Token> previousTokens = tokensWithCompilerDirectives;
                tokensWithCompilerDirectives = new List<Token>();
                
                // Keep tokens before compiler directive
                Token firstTokenOfCompilerDirective = compilerDirectiveTokensOnFirstLine[0];
                foreach (Token token in previousTokens)
                {
                    if(token == firstTokenOfCompilerDirective)
                    {
                        break;
                    }
                    tokensWithCompilerDirectives.Add(token);
                } 
            }

            // Build a CompilerDirectiveToken wrapping all matched tokens on the first line
            CompilerDirectiveToken directiveToken = new CompilerDirectiveToken(
                compilerDirective,
                compilerDirectiveTokensOnFirstLine,
                hasError);

            // Add the compilerDirectiveToken
            tokensWithCompilerDirectives.Add(directiveToken);

            // Keep tokens after compiler directive
            if (tokensOnFirstLineAfterCompilerDirective != null)
            {
                foreach (Token token in tokensOnFirstLineAfterCompilerDirective)
                {
                    tokensWithCompilerDirectives.Add(token);
                }
            }

            // Register line continuation properties
            HasDirectiveTokenContinuedOnNextLine = HasDirectiveTokenContinuedOnNextLine || hasDirectiveTokenContinuedOnNextLine;

            // Return potentially continued compiler directive token
            if (tokensOnFirstLineAfterCompilerDirective == null)
            {
                return directiveToken;
            }
            else
            {
                return null;
            }
        }

        internal TokensGroup InsertCompilerDirectiveTokenOnNextLine(TokensGroup continuedTokensGroupOnPreviousLine, IList<Token> compilerDirectiveTokensOnNextLine, IList<Token> tokensOnFirstLineAfterCompilerDirective, bool hasDirectiveTokenContinuedOnNextLine)
        {
            // Initialize tokens list
            tokensWithCompilerDirectives = new List<Token>();

            // Build a ContinuationTokensGroup wrapping all matched tokens on the next line
            ContinuationTokensGroup continuationToken = null;
            try
            {
                continuationToken = new ContinuationTokensGroup(
                    continuedTokensGroupOnPreviousLine,
                    compilerDirectiveTokensOnNextLine);
            }
            catch(Exception e)
            {
                string error = e.Message;
            }

            // Add the ContinuationTokensGroup
            tokensWithCompilerDirectives.Add(continuationToken);

            // Keep tokens after compiler directive
            if (tokensOnFirstLineAfterCompilerDirective != null)
            {
                foreach (Token token in tokensOnFirstLineAfterCompilerDirective)
                {
                    tokensWithCompilerDirectives.Add(token);
                }
            }

            // Register line continuation properties
            HasDirectiveTokenContinuationFromPreviousLine = true;
            HasDirectiveTokenContinuedOnNextLine = HasDirectiveTokenContinuedOnNextLine || hasDirectiveTokenContinuedOnNextLine;

            // Return potentially continued compiler directive token
            if (tokensOnFirstLineAfterCompilerDirective == null)
            {
                return continuationToken;
            }
            else
            {
                return null;
            }
        }

        /// <summary>
        /// True if the first compiler directive token on the next line continues the last compiler directive token of this line
        /// </summary>
        public bool HasDirectiveTokenContinuedOnNextLine { get; private set; }

        /// <summary>
        /// True if the first compiler directive token on this line continues the last compiler directive token of the previous line
        /// </summary>
        public bool HasDirectiveTokenContinuationFromPreviousLine { get; private set; }

        /// <summary>
        /// Error and warning messages produced while scanning the raw source text line
        /// (before text manipulation phase)
        /// </summary>
        public IList<Diagnostic> PreprocessorDiagnostics { get; private set; }

        public override IEnumerable<Diagnostic> AllDiagnostics()
        {
            // Start with diagnostic from TokensLine
            foreach (var diagnostic in base.AllDiagnostics())
            {
                yield return diagnostic;
            }

            // Add preprocessor diagnostics
            if (PreprocessorDiagnostics != null)
            {
                foreach (var preprocessorDiagnostic in PreprocessorDiagnostics)
                {
                    yield return preprocessorDiagnostic;
                }
            }
        }

        internal override void ResetDiagnostics()
        {
            base.ResetDiagnostics();
            PreprocessorDiagnostics = null;
        }

        /// <summary>
        /// Lazy initialization of diagnostics list
        /// </summary>
        internal void AddDiagnostic(Diagnostic diag)
        {
            if(PreprocessorDiagnostics == null)
            {
                PreprocessorDiagnostics = new List<Diagnostic>();
            }
            PreprocessorDiagnostics.Add(diag);
        }

        // --- Incremental compilation process ---

        protected void CopyProcessedTokensLineProperties(ProcessedTokensLine previousLineVersion)
        {
            this.CompilerListingControlDirective = previousLineVersion.CompilerListingControlDirective;
            this.ImportedDocuments = previousLineVersion.ImportedDocuments;
            this.HasDirectiveTokenContinuationFromPreviousLine = previousLineVersion.HasDirectiveTokenContinuationFromPreviousLine;
            this.HasDirectiveTokenContinuedOnNextLine = previousLineVersion.HasDirectiveTokenContinuedOnNextLine;
            this.PreprocessorDiagnostics = previousLineVersion.PreprocessorDiagnostics;
            this.PreprocessingState = previousLineVersion.PreprocessingState;
            this.ReplaceDirective = previousLineVersion.ReplaceDirective;
            this.tokensWithCompilerDirectives = previousLineVersion.tokensWithCompilerDirectives;

            CompilationStep = Concurrency.CompilationStep.Preprocessor;
        }
    }
}
