using JetBrains.Annotations;

namespace TypeCobol.Compiler.Sql.Model
{
    /// <summary>
    /// Base class for all SQL objects built during CodeElement phase.
    /// </summary>
    public abstract class SqlObject : IVisitable
    {
        //TODO Add a method to get tokens corresponding to this SQL object

        public bool AcceptVisitor(ISqlVisitor visitor)
        {
            bool continueVisit = visitor.BeginSqlObject(this) && VisitSqlObject(visitor);
            visitor.EndSqlObject(this);
            return continueVisit;
        }

        protected abstract bool VisitSqlObject([NotNull] ISqlVisitor visitor);
    }
}
