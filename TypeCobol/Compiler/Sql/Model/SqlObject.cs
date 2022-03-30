using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.Model
{
    /// <summary>
    /// Base class for all SQL objects built during CodeElement phase.
    /// </summary>
    public abstract class SqlObject : IVisitable
    {
        //TODO How to set and add tokens automatically for all SQL objects ?
        public IList<Token> ConsumedTokens { get; internal set; }

        public bool AcceptVisitor(ISqlVisitor visitor)
        {
            bool continueVisit = visitor.BeginSqlObject(this) && VisitSqlObject(visitor);
            visitor.EndSqlObject(this);
            return continueVisit;
        }

        protected abstract bool VisitSqlObject([NotNull] ISqlVisitor visitor);
    }

 
}
