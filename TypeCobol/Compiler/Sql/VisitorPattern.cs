using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql
{
    public interface IVisitable
    {
        bool AcceptVisitor([NotNull] ISqlVisitor visitor);
    }

    public interface ISqlVisitor
    {
        bool BeginSqlObject([NotNull] SqlObject sqlObject);
        void EndSqlObject([NotNull] SqlObject sqlObject);

        bool Visit([NotNull] FullSelect fullSelect);
        bool Visit([NotNull] SubSelect subSelect);
        bool Visit([NotNull] SelectClause selectClause);
        bool Visit([NotNull] FromClause fromClause);
    }

    /// <summary>
    /// Base class for SQL visitor, implementations of Visit methods
    /// not doing anything by default.
    /// </summary>
    public abstract class AbstractSqlVisitor : ISqlVisitor
    {
        public virtual bool BeginSqlObject(SqlObject sqlObject) => true;
        public virtual void EndSqlObject(SqlObject sqlObject) { }

        public virtual bool Visit(FullSelect fullSelect) => true;
        public virtual bool Visit(SubSelect subSelect) => true;
        public virtual bool Visit(SelectClause selectClause) => true;
        public virtual bool Visit(FromClause fromClause) => true;
    }

    /// <summary>
    /// Useful methods for visitor pattern.
    /// </summary>
    public static class VisitorPatternExtensions
    {
        public static bool ContinueVisit([NotNull] this ISqlVisitor visitor, IEnumerable<IVisitable> sqlObjects)
        {
            bool continueVisit = true;
            if (sqlObjects != null)
            {
                foreach (var sqlObject in sqlObjects)
                {
                    if (!continueVisit) break;
                    if (sqlObject == null) continue;
                    continueVisit = sqlObject.AcceptVisitor(visitor);
                }
            }

            return continueVisit;
        }

        public static bool ContinueVisit([NotNull] this ISqlVisitor visitor, params IVisitable[] sqlObjects)
        {
            return ContinueVisit(visitor, (IEnumerable<IVisitable>) sqlObjects);
        }

        public static bool ContinueVisit([NotNull] this ISqlVisitor visitor, params IEnumerable<IVisitable>[] sqlObjectCollections)
        {
            return ContinueVisit(visitor, sqlObjectCollections.SelectMany(sqlObjectCollection => sqlObjectCollection));
        }
    }
}
