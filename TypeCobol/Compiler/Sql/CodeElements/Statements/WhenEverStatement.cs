using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public enum ExceptionConditionType
    {
        NotFound,
        Sqlerror,
        Sqlwarning
    }
    public enum NextStatementType
    {
        Continue,
        Goto
    }

    public class WhenEverStatement : SqlStatementElement
    {
        public SymbolReference HostLabel { get; }
        public SyntaxProperty<ExceptionConditionType> ExceptionCondition { get; }
        public SyntaxProperty<NextStatementType> NextStatementType { get; }

        public WhenEverStatement(SyntaxProperty<ExceptionConditionType> exceptionCondition, SyntaxProperty<NextStatementType> nextStatementType, SymbolReference hostLabel) : base(CodeElementType.WhenEverStatement, StatementType.WhenEverStatement)
        {
            ExceptionCondition = exceptionCondition;
            NextStatementType = nextStatementType;
            HostLabel = hostLabel;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, ExceptionCondition,
                                                         NextStatementType, HostLabel);
        }
    }
}
