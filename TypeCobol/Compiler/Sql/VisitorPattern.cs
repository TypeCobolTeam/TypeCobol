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
        bool Visit([NotNull] TableViewCorrelationName tableViewCorrelationName);
        bool Visit([NotNull] SavePointClause savePointClause);
        bool Visit([NotNull] Selection selection);
        bool Visit([NotNull] StarSelection starSelection);
        bool Visit([NotNull] SingleTableReference singleTableReference);
        bool Visit([NotNull] CorrelationClause correlationClause);     
        bool Visit([NotNull] ExpressionSelection expressionSelection);
        bool Visit([NotNull] UnpackedRowSelection unpackedRowSelection);
        bool Visit([NotNull] TableOrViewAllColumnsSelection tableOrViewAllColumnsSelection);
        bool Visit([NotNull] SqlExpression sqlExpression);
        bool Visit([NotNull] SqlConstant sqlConstant);
        bool Visit([NotNull] DatetimeConstant datetimeConstant);
        bool Visit([NotNull] ConnectionTarget connectionTarget);
        bool Visit([NotNull] ConnectionAuthorization connectionAuthorization);
        bool Visit([NotNull] GetDiagnosticInformation getDiagnosticInformation);
        bool Visit([NotNull] StatementInformation statementInformation);
        bool Visit([NotNull] SingleInformationAssignment singleInformationAssignment);
        bool Visit([NotNull] CompositeInformationAssignment compositeInformationAssignment );
        bool Visit([NotNull] InformationAssignment informationAssignment);
        bool Visit([NotNull] ConditionInformation conditionInformation);
        bool Visit([NotNull] CombinedInformation combinedInformation);
        bool Visit([NotNull] CombinedInformationItem combinedInformationItem);
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
        public virtual bool Visit(SavePointClause savePointClause) => true;
        public virtual bool Visit(TableViewCorrelationName tableViewCorrelationName) => true;
        public virtual bool Visit(Selection selection) => true;
        public virtual bool Visit(StarSelection starSelection) => true;
        public virtual bool Visit(ExpressionSelection expressionSelection) => true;
        public virtual bool Visit(UnpackedRowSelection unpackedRowSelection) => true;
        public virtual bool Visit(TableOrViewAllColumnsSelection tableOrViewAllColumnsSelection) => true;
        public virtual bool Visit(SingleTableReference singleTableReference) => true;
        public virtual bool Visit(CorrelationClause correlationClause) => true;
        public virtual bool Visit(SqlExpression sqlExpression) => true;
        public virtual bool Visit(SqlConstant sqlConstant) => true;
        public virtual bool Visit(DatetimeConstant datetimeConstant) => true;
        public virtual bool Visit(ConnectionTarget connectionTarget) => true;
        public virtual bool Visit(ConnectionAuthorization connectionAuthorization) => true;
        public virtual bool Visit(GetDiagnosticInformation getDiagnosticInformation) => true;
        public virtual bool Visit(StatementInformation statementInformation) => true;
        public virtual bool Visit(ConditionInformation conditionInformation) => true;
        public virtual bool Visit(CombinedInformation combinedInformation) => true;
        public virtual bool Visit(SingleInformationAssignment singleInformationAssignment) => true;
        public virtual bool Visit(InformationAssignment informationAssignment) => true;
        public virtual bool Visit(CompositeInformationAssignment compositeInformationAssignment) => true;
        public virtual bool Visit(CombinedInformationItem combinedInformationItem) => true;
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
