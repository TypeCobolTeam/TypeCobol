using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
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
        /// True if a code element was parsed from tokens on this line and continues with tokens on the next line
        /// </summary>
        public bool HasCodeElementContinuedOnNextLine { get; private set; }

        /// <summary>
        /// True if a code element was parsed from tokens on the previous line and continues with tokens on this line
        /// </summary>
        public bool HasCodeElementContinuationFromPreviousLine { get; private set;  }

        /// <summary>
        /// Error and warning messages produced while parsing the source text line
        /// </summary>
        public IList<ParserDiagnostic> ParserDiagnostics { get; private set; }

        /// <summary>
        /// Lazy initialization of diagnostics list
        /// </summary>
        internal void AddDiagnostic(ParserDiagnostic diag)
        {
            // Lazy list instantiation
            if (ParserDiagnostics == null)
            {
                ParserDiagnostics = new List<ParserDiagnostic>();
            }

            ParserDiagnostics.Add(diag);
        }
    }
}
