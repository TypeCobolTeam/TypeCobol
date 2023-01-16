using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Line of code elements after parser execution
    /// </summary>
    public class CodeElementsLine : ProcessedTokensLine, ICodeElementsLine
    {
        public CodeElementsLine(ITextLine textLine, ColumnsLayout columnsLayout) : base(textLine, columnsLayout)
        { }

        internal CodeElementsLine(CodeElementsLine previousLineVersion, CompilationStep compilationStep) : base(previousLineVersion.textLine, previousLineVersion.ColumnsLayout)
        {
            switch(compilationStep)
            {
                // In the preprocessor, reuse the previoulsy scanned tokens
                case CompilationStep.Preprocessor:
                    CopyTokensLineProperties(previousLineVersion);
                    break;
                // In the parser, reuse also the previoulsy processed compiler directives
                case CompilationStep.CodeElementsParser:
                    CopyTokensLineProperties(previousLineVersion);
                    CopyProcessedTokensLineProperties(previousLineVersion);
                    break;
                default:
                    // Nothing more to do in the previous steps
                    break;
            }
        }

        // --- Computed line properties after code elements parser execution ---

        /// <summary>
        /// True if a code element starts on the current line
        /// </summary>
        public bool HasCodeElements { get { return CodeElements != null; } }

        /// <summary>
        /// Code elements STARTING on this line 
        /// </summary>
        public IList<CodeElement> CodeElements { get; private set; }

        /// <summary>
        /// Always use this method add new code element to this line (never call directly CodeElements.Add)
        /// </summary>
        internal void AddCodeElement(CodeElement codeElement)
        {
            // Lazy list instantiation
            if(CodeElements == null)
            {
                // In most cases, there will be no more than a single code element per line
                CodeElements = new List<CodeElement>(1);
            }
                    
            CodeElements.Add(codeElement);
        }

        /// <summary>
        /// Called when a line is reused between two incremental parsing phases
        /// </summary>
        internal void ResetCodeElements()
        {
            CodeElements = null;
            _ParserDiagnostics = null;
        }

        /// <summary>
        /// Error and warning messages produced while parsing the source text line
        /// </summary>
        public IList<Diagnostic> ParserDiagnostics
        {
            get { return _ParserDiagnostics; }
        }

        private IList<Diagnostic> _ParserDiagnostics;
        /// <summary>
        /// Lazy initialization of diagnostics list
        /// </summary>
        internal void AddParserDiagnostic(Diagnostic diag)
        {
            // Lazy list instantiation
            if (_ParserDiagnostics == null)
            {
                _ParserDiagnostics = new List<Diagnostic>();
            }

            _ParserDiagnostics.Add(diag);
        }

        /// <summary>
        /// Reset all diagnostics for the current line
        /// </summary>
        internal override void ResetDiagnostics()
        {
            base.ResetDiagnostics();
            //TODO ProcessingDiagnostics from compiler directives ?
            _ParserDiagnostics = null;
            if (HasCodeElements)
            {
                foreach (var codeElement in CodeElements)
                {
                    codeElement.Diagnostics = null; //Delete all diagnostics on every codeelement of this line
                }
            }
        }

        public override IEnumerable<Diagnostic> AllDiagnostics()
        {
            // Start with diagnostic from ProcessedTokensLine
            foreach (var diagnostic in base.AllDiagnostics())
            {
                yield return diagnostic;
            }

            // Add processing diagnostics (compiler directives are parsed during Preprocessor step and processed during CodeElement step)
            if (HasCompilerDirectives)
            {
                foreach (var token in TokensWithCompilerDirectives)
                {
                    if (token is CompilerDirectiveToken compilerDirectiveToken && compilerDirectiveToken.CompilerDirective.ProcessingDiagnostics != null)
                    {
                        foreach (var processingDiagnostic in compilerDirectiveToken.CompilerDirective.ProcessingDiagnostics)
                        {
                            yield return processingDiagnostic;
                        }
                    }
                }
            }

            // CodeElement parsing diagnostics
            if (ParserDiagnostics != null)
            {
                foreach (var parserDiagnostic in ParserDiagnostics)
                {
                    yield return parserDiagnostic;
                }
            }

            // Diagnostics on CodeElement themselves
            if (HasCodeElements)
            {
                foreach (var codeElement in CodeElements)
                {
                    if (codeElement.Diagnostics != null)
                    {
                        foreach (var codeElementDiagnostic in codeElement.Diagnostics)
                        {
                            yield return codeElementDiagnostic;
                        }
                    }
                }
            }
        }

        internal void ShiftUp() => Shift(-1);

        internal void ShiftDown() => Shift(+1);

        private void Shift(int offset)
        {
            //Update line index
            LineIndex += offset;

            //Shift diagnostics
            foreach (var diagnostic in AllDiagnostics())
            {
                diagnostic.Shift(offset);
            }
        }
    }
}
