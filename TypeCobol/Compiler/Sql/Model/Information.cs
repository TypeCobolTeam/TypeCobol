using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    /// <summary>
    /// Information retrieved by a GET DIAGNOSTICS statement.
    /// </summary>
    public abstract class GetDiagnosticInformation : SqlObject
    {
        public abstract InformationType Type { get; }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }

    public enum InformationType
    {
        Statement,
        Condition,
        Combined
    }

    public class StatementInformation : GetDiagnosticInformation
    {
        public StatementInformation(List<InformationAssignment> assignments)
        {
            Assignments = assignments;
        }

        public override InformationType Type => InformationType.Statement;

        public List<InformationAssignment> Assignments { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Assignments), Assignments, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this) &&
                   visitor.ContinueVisit(Assignments);
        }
    }

    public class InformationAssignment : SqlObject
    {
        public InformationAssignment(SqlVariable storage, SymbolReference itemName)
        {
            Storage = storage;
            ItemName = itemName;
        }

        public SqlVariable Storage { get; }

        public SymbolReference ItemName { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Storage), Storage, indentLevel);
            DumpProperty(output, nameof(ItemName), ItemName, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Storage);
        }
    }

    public class ConditionInformation : GetDiagnosticInformation
    {
        public ConditionInformation(SqlVariable diagnosticIdVariable, SqlConstant diagnosticIdLiteral,
            List<InformationAssignment> assignments)
        {
            DiagnosticIdVariable = diagnosticIdVariable;
            DiagnosticIdLiteral = diagnosticIdLiteral;
            Assignments = assignments;
        }

        public override InformationType Type => InformationType.Condition;

        public SqlVariable DiagnosticIdVariable { get; }
        public SqlConstant DiagnosticIdLiteral { get; }
        public List<InformationAssignment> Assignments { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(DiagnosticIdVariable), DiagnosticIdVariable, indentLevel);
            DumpProperty(output, nameof(DiagnosticIdLiteral), DiagnosticIdLiteral, indentLevel);
            DumpProperty(output, nameof(Assignments), Assignments, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && 
                   visitor.Visit(this) &&
                   visitor.ContinueVisit(Assignments) &&
                   visitor.ContinueVisit(DiagnosticIdVariable, DiagnosticIdLiteral);
        }
    }

    public class CombinedInformation : GetDiagnosticInformation
    {
        public CombinedInformation(SqlVariable variable, List<CombinedInformationItem> items)
        {
            Variable = variable;
            Items = items;
        }

        public override InformationType Type => InformationType.Combined;
        public SqlVariable Variable { get; }
        public List<CombinedInformationItem> Items { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Variable), Variable, indentLevel);
            DumpProperty(output, nameof(Items), Items, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) &&
                   visitor.Visit(this) &&
                   visitor.ContinueVisit(Variable) && visitor.ContinueVisit(Items);
        }
    }

    public enum CombinedInformationItemType
    {
        Statement,
        Condition,
        Connection
    }

    public class CombinedInformationItem : SqlObject
    {
        public CombinedInformationItem(CombinedInformationItemType type, SqlVariable diagnosticIdVariable,
            SqlConstant diagnosticIdLiteral)
        {
            Type = type;
            DiagnosticIdVariable = diagnosticIdVariable;
            DiagnosticIdLiteral = diagnosticIdLiteral;
        }

        public CombinedInformationItemType Type { get; }
        public SqlVariable DiagnosticIdVariable { get; }
        public SqlConstant DiagnosticIdLiteral { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Type), Type, indentLevel);
            DumpProperty(output, nameof(DiagnosticIdVariable), DiagnosticIdVariable, indentLevel);
            DumpProperty(output, nameof(DiagnosticIdLiteral), DiagnosticIdLiteral, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(DiagnosticIdVariable, DiagnosticIdLiteral);
        }
    }
}
