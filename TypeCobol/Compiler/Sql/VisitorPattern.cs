using System.Collections.Generic;
using System.Linq;

namespace TypeCobol.Compiler.Sql
{
    public interface IVisitable
    {
        bool AcceptVisitor(ISqlVisitor visitor);
    }

    public interface ISqlVisitor
    {
        //TODO add Visit methods for all SQL objects
    }

    /// <summary>
    /// Base class for SQL visitor, implementations of Visit methods
    /// not doing anything by default.
    /// </summary>
    public abstract class AbstractSqlVisitor : ISqlVisitor
    {

    }

    /// <summary>
    /// Useful methods for visitor pattern.
    /// </summary>
    public static class VisitorPatternExtensions
    {
        public static bool ContinueVisit(this ISqlVisitor visitor, IEnumerable<IVisitable> sqlObjects)
        {
            bool continueVisit = true;
            if (sqlObjects != null)
            {
                foreach (var sqlObject in sqlObjects)
                {
                    if (!continueVisit) break;
                    continueVisit = sqlObject.AcceptVisitor(visitor);
                }
            }

            return continueVisit;
        }

        public static bool ContinueVisit(this ISqlVisitor visitor, params IVisitable[] sqlObjects)
        {
            return ContinueVisit(visitor, (IEnumerable<IVisitable>) sqlObjects);
        }

        public static bool ContinueVisit(this ISqlVisitor visitor, params IEnumerable<IVisitable>[] sqlObjects)
        {
            return ContinueVisit(visitor, sqlObjects.SelectMany(sqlObject => sqlObject));
        }
    }
}
