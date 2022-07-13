using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public abstract class Information : SqlObject
    {
        public abstract InformationType Type { get; }
    }

    public enum InformationType
    {
        Statement,
        Condition,
        Combined
    }

    public class StatementInformation : Information
    {
        public StatementInformation(List<InformationAssignment> assignments)
        {
            Assignments = assignments;
        }

        public override InformationType Type => InformationType.Statement;

        public List<InformationAssignment> Assignments { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Type), Type, indentLevel);
            DumpProperty(output, nameof(Assignments), Assignments, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Assignments);
        }
    }

    public abstract class InformationAssignment : SqlObject
    {
        protected InformationAssignment(SqlVariable storage)
        {
            Storage = storage;
        }

        public SqlVariable Storage { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Storage), Storage, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Storage);
        }
    }

    public class SingleInformationAssignment : InformationAssignment
    {
        public SingleInformationAssignment(SymbolReference itemName, SqlVariable storage) : base(storage)
        {
            ItemName = itemName;
        }

        public SymbolReference ItemName { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(ItemName), ItemName, indentLevel);
            DumpProperty(output, nameof(Storage), Storage, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Storage);
        }
    }

    public class CompositeInformationAssignment : InformationAssignment
    {
        public CompositeInformationAssignment(List<SymbolReference> itemNames, SqlVariable storage) : base(storage)
        {
            ItemNames = itemNames;
        }

        public List<SymbolReference> ItemNames { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(ItemNames), ItemNames, indentLevel);
            DumpProperty(output, nameof(Storage), Storage, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Storage);
        }
    }

    public class ConditionInformation : Information
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
            DumpProperty(output, nameof(Type), Type, indentLevel);
            DumpProperty(output, nameof(DiagnosticIdVariable), DiagnosticIdVariable, indentLevel);
            DumpProperty(output, nameof(DiagnosticIdLiteral), DiagnosticIdLiteral, indentLevel);
            DumpProperty(output, nameof(Assignments), Assignments, indentLevel);

        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Assignments) &&
                   visitor.ContinueVisit(DiagnosticIdVariable, DiagnosticIdLiteral);
        }
    }

    public class CombinedInformation : Information
    {
        public CombinedInformation(SqlVariable storage, List<CombinedInformationItem> items)
        {
            Storage = storage;
            Items = items;
        }

        public override InformationType Type => InformationType.Combined;
        public SqlVariable Storage { get; }
        public List<CombinedInformationItem> Items { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Type), Type, indentLevel);
            DumpProperty(output, nameof(Storage), Storage, indentLevel);
            DumpProperty(output, nameof(Items), Items, indentLevel);

        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) &&
                   visitor.ContinueVisit(Storage);
        }
    }

    public enum CombinedInformationItemType
    {
        Statement,
        Condition,
        Connection
    }

    public abstract class CombinedInformationItem : SqlObject
    {
        public abstract CombinedInformationItemType Type { get; }
    }

    public class StatementInformationItem : CombinedInformationItem
    {
        public override CombinedInformationItemType Type => CombinedInformationItemType.Statement;

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Type), Type, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }

    public class ConditionInformationItem : CombinedInformationItem
    {
        public ConditionInformationItem(SqlVariable diagnosticIdVariable, SqlConstant diagnosticIdLiteral)
        {
            DiagnosticIdVariable = diagnosticIdVariable;
            DiagnosticIdLiteral = diagnosticIdLiteral;
        }

        public override CombinedInformationItemType Type => CombinedInformationItemType.Condition;

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

    public class ConnectionInformationItem : CombinedInformationItem
    {
        public ConnectionInformationItem(SqlVariable diagnosticIdVariable, SqlConstant diagnosticIdLiteral)
        {
            DiagnosticIdVariable = diagnosticIdVariable;
            DiagnosticIdLiteral = diagnosticIdLiteral;
        }

        public override CombinedInformationItemType Type => CombinedInformationItemType.Connection;

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