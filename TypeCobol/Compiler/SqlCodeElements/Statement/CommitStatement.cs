using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.SqlScanner;

namespace TypeCobol.Compiler.SqlCodeElements.Statement
{
    /// <summary>
    /// SQL COMMIT Statement Code Element.
    /// </summary>
    public class CommitStatement : SqlStatementElement
    {
        public CommitStatement() : base(CodeElementType.CommitStatement, StatementType.CommitStatement)
        {
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor);
        }
    }
}
