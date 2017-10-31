using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Preprocessor;
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
        internal void ResetDiagnostics()
        {
            _ParserDiagnostics = null;
            if (CodeElements != null)
            {
                foreach (var codeElement in CodeElements)
                {
                    codeElement.Diagnostics = null; //Delete all diganostics on every codeelement of this line
                }
            }
        }

        internal void UpdateDiagnositcsLine()
        {
            //Update ParserDiag lines index
            if (_ParserDiagnostics != null)
            {
                foreach (var parserDiagnostic in _ParserDiagnostics)
                {
                    parserDiagnostic.Line = LineIndex + 1;
                }
            }

            //Update CodeElements Diags lines index
            if (CodeElements != null)
            {
                foreach (var codeElement in CodeElements)
                {
                    if (codeElement.Diagnostics != null)
                    {
                        foreach (var codeElementDiagnostic in codeElement.Diagnostics)
                        {
                            codeElementDiagnostic.Line = LineIndex + 1;
                        }
                    }
                }
            }
        }
    }
}
