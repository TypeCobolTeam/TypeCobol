using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL SELECT Statement Code Element.
    /// </summary>
    public class SelectStatement : SqlStatementElement
    {
        private readonly FullSelect _fullSelect;
        public SelectStatement(FullSelect fullSelect) : base(CodeElementType.SelectStatement, StatementType.SelectStatement)
        {
            this._fullSelect = fullSelect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(_fullSelect);
        }
    }
} 
