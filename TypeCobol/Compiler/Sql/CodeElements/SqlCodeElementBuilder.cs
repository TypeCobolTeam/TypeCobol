using System.Collections.Generic;
using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;

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
                selectionModifier = new SyntaxProperty<SelectionModifier>(SelectionModifier.All,
                    ParseTreeUtils.GetFirstToken(context.SQL_ALL()));
            }
            else if (context.SQL_DISTINCT() != null)
            {
                selectionModifier = new SyntaxProperty<SelectionModifier>(SelectionModifier.Distinct,
                    ParseTreeUtils.GetFirstToken(context.SQL_DISTINCT()));
            }
            if (context.star() != null)
            {
                return new SelectClause(selectionModifier, new StarSelection());
            }

            if (context.selections() != null)
            {
                var dotStarSelections = new List<DotStarSelection>();
                int l = context.selections().ChildCount;
                int i = 0;
                foreach (var selection in context.selections().selection())
                {
                    dotStarSelections.Add(CreateDotStarSelection(selection));
                }
                return new SelectClause(selectionModifier, dotStarSelections);
            }

            return null;
        }

        public DotStarSelection CreateDotStarSelection(CodeElementsParser.SelectionContext context)
        {
            if (context.dotStarSelection() != null)
            {
                if ((context.dotStarSelection().tableOrViewOrCorrelationName().Name != null))
                {
                    Token name = context.dotStarSelection().tableOrViewOrCorrelationName().Name as Token;
                    SymbolReference tail = new SymbolReference(new AlphanumericValue(name), SymbolType.AlphabetName);
                    if (context.dotStarSelection().tableOrViewOrCorrelationName().SchemaName != null)
                    {
                        Token schemaName =
                            context.dotStarSelection().tableOrViewOrCorrelationName().SchemaName as Token;
                        SymbolReference SchemaName =
                            new SymbolReference(new AlphanumericValue(schemaName), SymbolType.AlphabetName);
                        if (context.dotStarSelection().tableOrViewOrCorrelationName().DBMS != null)
                        {
                            Token dbms =
                                context.dotStarSelection().tableOrViewOrCorrelationName().DBMS as Token;
                            SymbolReference DBMS =
                                new SymbolReference(new AlphanumericValue(dbms), SymbolType.AlphabetName);
                            QualifiedSymbolReference head = new QualifiedSymbolReference(DBMS, SchemaName);
                            QualifiedSymbolReference fullName = new QualifiedSymbolReference(head, tail);
                            return new DotStarSelection(fullName);
                        }
                        else
                        {
                            QualifiedSymbolReference fullName = new QualifiedSymbolReference(SchemaName, tail);
                            return new DotStarSelection(fullName);
                        }
                    }
                    else
                    {
                        return new DotStarSelection(tail);
                    }
                }
            }

            return null;
        }

    }
}
