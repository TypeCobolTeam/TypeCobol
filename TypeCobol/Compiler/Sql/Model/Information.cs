using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public abstract class Information : SqlObject
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
            DumpProperty(output, nameof(Assignments), Assignments, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this) &&
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
            base.DumpContent(output, indentLevel);
            DumpProperty(output, nameof(ItemName), ItemName, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this);
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
            base.DumpContent(output, indentLevel);
            DumpProperty(output, nameof(ItemNames), ItemNames, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this);

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
            base.DumpContent(output, indentLevel);
            DumpProperty(output, nameof(Storage), Storage, indentLevel);
            DumpProperty(output, nameof(Items), Items, indentLevel);

        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) &&
                   visitor.Visit(this) &&
                   visitor.ContinueVisit(Storage) && visitor.ContinueVisit(Items);
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
            base.DumpContent(output, indentLevel);
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