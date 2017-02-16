﻿using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The EXEC statement enables the developer to embed statements written
    /// in another programming languages in a Cobol Program.
    /// The Cobol compiler must delegate the analysis and translation of these
    /// embedded statements to secondary compilers, implemented as preprocessors
    /// in older versions of IBM Cobol, or coprocessors in more recent versions
    /// (with better integration in the Cobol compiler).
    /// </summary>
    public class ExecStatement : StatementElement
    {
        public ExecStatement() : base(CodeElementType.ExecStatement, StatementType.ExecStatement)
        { }
        
        /// <summary>
        /// Secondary compiler used to translate CodeLines
        /// </summary>
        public ExternalName ExecTranslatorName { get; set; }

        /// <summary>
        /// Source code to be analyzed by the secondary compiler
        /// </summary>
        public AlphanumericValue[] CodeLines { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) CodeLines)
                   && this.ContinueVisitToChildren(astVisitor, ExecTranslatorName);
        }
    }
}
