using System.Collections.Generic;
using System.Runtime.InteropServices.WindowsRuntime;
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
                var tableOrViewOrCorrelationName = context.dotStarSelection().tableOrViewOrCorrelationName();
                if ((tableOrViewOrCorrelationName.Name != null))
                {
                    Token name = tableOrViewOrCorrelationName.Name as Token;
                    Token schemaName = tableOrViewOrCorrelationName.SchemaName as Token;
                    Token dbms = tableOrViewOrCorrelationName.DBMS as Token;
                    SymbolReference fullName = CreateSymbolReference(name, schemaName, dbms);
                    return new DotStarSelection(fullName);
                }
                
            }

            return null;

        }

        private SymbolReference CreateSymbolReference(Token nameToken, Token qualifierToken, Token topLevelQualifierToken)
        {
            if (nameToken != null)
            {
                SymbolReference name = new SymbolReference(new AlphanumericValue(nameToken),
                    SymbolType.SqlIdentifier);
                if (qualifierToken != null)
                {
                    SymbolReference qualifier =
                        new SymbolReference(new AlphanumericValue(qualifierToken),
                            SymbolType.SqlIdentifier);
                    if (topLevelQualifierToken != null)
                    {
                        SymbolReference topLevelQualifier =
                            new SymbolReference(new AlphanumericValue(topLevelQualifierToken),
                                SymbolType.SqlIdentifier);
                        QualifiedSymbolReference head = new QualifiedSymbolReference(topLevelQualifier, qualifier);
                        return new QualifiedSymbolReference(head, name);
                    }
                    else
                    {
                        return new QualifiedSymbolReference(qualifier, name);
                    }
                }
                else
                {
                    return name;
                }
            }

            return null;
        }
    }
}
