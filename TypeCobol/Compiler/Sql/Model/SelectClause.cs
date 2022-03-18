using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Scanner;


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
        DotStar
    }

    public abstract class Selection
    {
        public abstract SelectionType Type { get; }
    }

    public class StarSelection : Selection
    {
        public override SelectionType Type => SelectionType.Star;
    }

    public class ExpressionSelection : Selection
    {
        public override SelectionType Type => SelectionType.Expression;
    }

    public class UnpackedRowSelection : Selection
    {
        public override SelectionType Type => SelectionType.UnpackedRow;
    }

    public class DotStarSelection : Selection
    {
        public SymbolReference Name { get; }
        public override SelectionType Type => SelectionType.DotStar;

        public  DotStarSelection(SymbolReference name)
        {
            this.Name = name;
        }
    }

    public class SelectClause : SqlObject
    {
        private readonly Selection[] _selections;
        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, StarSelection starSelection)
            : this(selectionModifier, new [] { starSelection })
        {

        }
        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, DotStarSelection dotStarSelection)
            : this(selectionModifier, new[] { dotStarSelection })
        {

        }

        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, IEnumerable<Selection> selections)
        {
            SelectionModifier = selectionModifier;
            _selections = selections?.ToArray() ?? Array.Empty<Selection>();
        }
        public SyntaxProperty<SelectionModifier> SelectionModifier { get; }

        public IEnumerable<Selection> Selections => _selections;

        public bool IsStarSelection => _selections.Length == 1 && _selections[0].Type == SelectionType.Star;

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}
