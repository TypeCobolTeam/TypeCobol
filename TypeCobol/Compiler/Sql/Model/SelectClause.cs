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

        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, StarSelection starSelection)
            : this(selectionModifier, new [] { starSelection })
        {

        }

        public SelectClause(SyntaxProperty<SelectionModifier> selectionModifier, IEnumerable<Selection> selections)
        {
            SelectionModifier = selectionModifier;
            _selections = selections?.ToArray() ?? Array.Empty<Selection>();
        }

        private static void DumpSelection(TextWriter output, Selection selection)
        {   
            ///TODO
            switch (selection.Type)
            {
                case SelectionType.DotStar:
                    var dotStarSelection = (DotStarSelection) selection;
                    break;
                case SelectionType.Expression:
                    var expression = (ExpressionSelection) selection;
                    break;
                case SelectionType.Star:
                    var starSelection = (StarSelection)selection;
                    break;

            }
        }
        public override void Dump(TextWriter output, int indentLevel)
        {
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            if (this.Selections != null)
            {
                foreach (var selection in this.Selections)
                {
                    DumpSelection(output,selection);
                }
            }
            if (this.SelectionModifier != null)
            {
                output.WriteLine(this.SelectionModifier);
            }
            
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
