using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;
using TypeCobol.Compiler.Sql.Nodes;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    public class SqlCodeElementBuilder
    {
        public CodeElement CreateCommitStatement(CodeElementsParser.CommitStatementContext context)
        {
            return new CommitStatement();
        }
        public CodeElement CreateSelectStatement(CodeElementsParser.SelectStatementContext context)
        {
            FullSelect fullSelect = null;
            if (context.fullselect() != null)
            {
                 fullSelect = CreateFullSelect(context.fullselect());
            }
            return new SelectStatement(fullSelect);
        }

        private FullSelect CreateFullSelect(CodeElementsParser.FullselectContext context)
        {
            SubSelect subSelect = null;
            if (context.subselect() != null)
            {
                 subSelect = CreateSubSelect(context.subselect());
            }
            return new FullSelect(subSelect);
        }

        private SubSelect CreateSubSelect(CodeElementsParser.SubselectContext context)
        {
            FromClause fromClause = null;
            SelectClause selectClause = null;
            if (context.sql_selectClause() != null)
            {
                 selectClause = CreateSelectClause(context.sql_selectClause());
            }
            return new SubSelect(selectClause, fromClause);

        }

        private SelectClause CreateSelectClause(CodeElementsParser.Sql_selectClauseContext context)
        {
            SyntaxProperty<SelectionModifier> selectionModifier = null;
            if (context.SQL_ALL() != null)
            {
                selectionModifier = new SyntaxProperty<SelectionModifier>(SelectionModifier.All, ParseTreeUtils.GetFirstToken(context.SQL_ALL()));
            }
            else if (context.SQL_DISTINCT() != null)
            {
                selectionModifier = new SyntaxProperty<SelectionModifier>(SelectionModifier.Distinct, ParseTreeUtils.GetFirstToken(context.SQL_DISTINCT()));
            }
            if (context.star() != null)
            {
                return new SelectClause(selectionModifier, new StarSelection());
            }

            return null;

        }
    }
}
