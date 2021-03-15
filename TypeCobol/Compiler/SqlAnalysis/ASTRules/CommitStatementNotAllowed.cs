using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.SqlNodes;

namespace TypeCobol.Compiler.SqlAnalysis.ASTRules
{
    internal class CommitStatementNotAllowed : SqlAbstractRuleChecker<Commit>
    {
        public CommitStatementNotAllowed(Action<Violation> addViolation) : base("cobol:SqlCommitRollback", addViolation)
        {
        }

        public override void Check(Commit item)
        {
            var token = item.CodeElement.ConsumedTokens[0]; 
            Violation v = new Violation(_ruleId, Severity.Error, token.Line, token.StartIndex, token.StopIndex, "SQL Commit statement is not allowed in EXEC SQL statement");
            _addViolationAction(v);
        }
    }
}
