using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// View of a source document as a complete Cobol Program or Class after parsing
    /// </summary>
    public class ProgramClassDocument
    {
        public ProgramClassDocument(CodeElementsDocument previousStepSnapshot, int programClassVersion, Program program, Class classObj, IList<ParserDiagnostic> diagnostics)
        {
            TextSourceInfo = previousStepSnapshot.TextSourceInfo;
            PreviousStepSnapshot = previousStepSnapshot;
            CurrentVersion = programClassVersion;
            Program = program;
            Class = classObj;
            Diagnostics = diagnostics;
        }

        /// <summary>
        /// Informations on the source file on disk, or the buffer in memory
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// Snapshot of the code elements document which was used to compute the current step
        /// </summary>
        public IDocumentSnapshot<ICodeElementsLine> PreviousStepSnapshot { get; private set; }

        /// <summary>
        /// Numeric version identifier for the current document
        /// </summary>
        public int CurrentVersion { get; private set; }

        /// <summary>
        /// Object model of a Cobol program built after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// </summary> 
        public Program Program { get; private set; }

        /// <summary>
        /// Object model of a Cobol class built after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// </summary> 
        public Class Class { get; private set; }

        /// <summary>
        /// Errors found while parsing Program or Class
        /// </summary>
        public IList<ParserDiagnostic> Diagnostics { get; private set; }
    }
}

 
