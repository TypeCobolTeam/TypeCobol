using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Text;

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
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.AreaAOrB;
        /// <summary>
        /// Secondary compiler used to translate CodeLines
        /// </summary>
        [CanBeNull]
        public ExternalName ExecTranslatorName { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, ExecTranslatorName);
        }
    }

    public class ExecStatementText : CodeElement
    {
        public ExecStatementText() : base(CodeElementType.ExecStatementText) { }
        public override CodeElementStartingAreaType StartingArea => CodeElementStartingAreaType.AreaAOrB;
        /// <summary>
        /// Source code to be analyzed by the secondary compiler
        /// </summary>
        public AlphanumericValue CodeLine { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, CodeLine);
        }
    }

    public class ExecStatementEnd : CodeElementEnd
    {
        public ExecStatementEnd() : base(CodeElementType.ExecStatementEnd) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }

}
