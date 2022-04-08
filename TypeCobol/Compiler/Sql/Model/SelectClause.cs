using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public enum SelectionModifier
    {
        All,
        Distinct
    }

    public enum SelectionType
    {
        Star,
        Expression,
        UnpackedRow,
        TableOrViewAllColumns
    }

    public abstract class Selection : SqlObject
    {
        public abstract SelectionType Type { get; }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }

    public class StarSelection : Selection
    {
        public override SelectionType Type => SelectionType.Star;

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this);
        }
    }

    public class ExpressionSelection : Selection
    {
        public override SelectionType Type => SelectionType.Expression;

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this);
        }
    }

    public class UnpackedRowSelection : Selection
    {
        public override SelectionType Type => SelectionType.UnpackedRow;

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this);
        }
    }

    public class TableOrViewAllColumnsSelection : Selection
    {
        public TableViewCorrelationName TableOrViewOrCorrelationName { get; }

        public override SelectionType Type => SelectionType.TableOrViewAllColumns;

        public TableOrViewAllColumnsSelection(TableViewCorrelationName tableOrViewOrCorrelationName)
        {
            this.TableOrViewOrCorrelationName = tableOrViewOrCorrelationName;
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(TableOrViewOrCorrelationName), TableOrViewOrCorrelationName, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) &&
                   visitor.Visit(this) &&
                   visitor.ContinueVisit(TableOrViewOrCorrelationName);
        }
    }

    public class SelectClause : SqlObject
    {
        private readonly Selection[] _selections;

        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, StarSelection starSelection)
            : this(selectionModifier, new [] { starSelection })
        {

        }

        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, IEnumerable<Selection> selections)
        {
            SelectionModifier = selectionModifier;
            _selections = selections?.ToArray() ?? Array.Empty<Selection>();
        }

        protected override void DumpContent(TextWriter output, int indentLevel) 
        {
            DumpProperty(output, nameof(SelectionModifier), SelectionModifier, indentLevel);
            DumpProperty(output, nameof(Selections), Selections, indentLevel);
        }

        public SyntaxProperty<SelectionModifier> SelectionModifier { get; }

        public IEnumerable<Selection> Selections => _selections;

        public bool IsStarSelection => _selections.Length == 1 && _selections[0].Type == SelectionType.Star;

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(Selections);
        }
    }
}
