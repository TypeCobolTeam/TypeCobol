using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.SqlAnalysis.ASTRules;
using TypeCobol.Compiler.SqlCodeElements;
using TypeCobol.Compiler.SqlNodes;

namespace TypeCobol.Compiler.SqlAnalysis
{
    /// <summary>
    /// SQL AST Checker class
    /// </summary>
    internal class SqlASTChecker : AbstractAstWithSqlVisitor
    {
        private Action<Violation> addViolation;
        private readonly CommitStatementNotAllowed _commitStmtNotAllowed;

        public SqlASTChecker(Action<Analysis.Violation> addViolation)
        {
            this.addViolation = addViolation;
            _commitStmtNotAllowed = new CommitStatementNotAllowed(addViolation);
        }

        public override bool Visit(Exec exec)
        {
            if (!exec.HasBeenParsed)
            {// Parse the SQL code if not have been already parsed.
                exec.Parse(addViolation as Action<Diagnostic>);
            }            
            return true;
        }

        public override bool Visit([NotNull] Commit commit)
        {
            _commitStmtNotAllowed.Check(commit);
            return true;
        }
    }
}
