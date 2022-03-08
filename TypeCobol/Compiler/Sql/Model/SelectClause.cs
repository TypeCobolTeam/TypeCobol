using System;
using System.Collections.Generic;
using System.Linq;

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
        public override SelectionType Type => SelectionType.DotStar;
    }

    public class SelectClause : SqlObject
    {
        private readonly Selection[] _selections;

        public SelectClause(SelectionModifier? selectionModifier, StarSelection starSelection)
            : this(selectionModifier, new [] { starSelection })
        {

        }

        public SelectClause(SelectionModifier? selectionModifier, IEnumerable<Selection> selections)
        {
            SelectionModifier = selectionModifier;
            _selections = selections?.ToArray() ?? Array.Empty<Selection>();
        }

        public SelectionModifier? SelectionModifier { get; }

        public IEnumerable<Selection> Selections => _selections;

        public bool IsStarSelection => _selections.Length == 1 && _selections[0].Type == SelectionType.Star;
    }
}
