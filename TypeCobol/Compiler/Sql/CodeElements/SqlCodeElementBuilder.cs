using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
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
                fromClause = CreateFromClause(context.from_clause());
            }
            return new SubSelect(selectClause, fromClause);
              
        }

        private FromClause CreateFromClause(CodeElementsParser.From_clauseContext context)
        {
            var tableReferences = new List<SymbolReference>();
            if (context.table_references() != null)
            {
                foreach (var tableContext in context.table_references().table_reference())
                {
                    SymbolReference fullName = CreateTableOrViewOrCorrelationName(tableContext
                        .single_table_or_view_reference().tableOrViewOrCorrelationName());
                    if ((tableContext.single_table_or_view_reference().correlation_clause()!= null))
                    {
                        Token correlationNameToken = tableContext.single_table_or_view_reference().correlation_clause()
                            .correlation_name as Token;
                        SymbolReference correlationName = new SymbolReference(
                            new AlphanumericValue(correlationNameToken), SymbolType.SqlIdentifier);

                        if (tableContext.single_table_or_view_reference().correlation_clause().new_column_names() != null)
                        {
                            SymbolReference main = null;
                            foreach (var columnName in tableContext.single_table_or_view_reference()
                                         .correlation_clause().new_column_names().new_column_name())
                            {
                                main = CreateColumnName(columnName, main);
                            }

                            QualifiedSymbolReference correlationClause =
                                new QualifiedSymbolReference(correlationName, main);
                            QualifiedSymbolReference singleTableOrViewReference = new QualifiedSymbolReference(fullName, correlationClause);
                            tableReferences.Add(singleTableOrViewReference);
                        }
                        else
                        {
                            QualifiedSymbolReference singleTableOrViewReference = new QualifiedSymbolReference(fullName, correlationName);
                            tableReferences.Add(singleTableOrViewReference);
                        }
                    }
                    else
                    {
                        tableReferences.Add(fullName);
                    }
                }

                return (new FromClause(tableReferences));
            }

            return null;
        }
        private SymbolReference CreateColumnName(CodeElementsParser.New_column_nameContext columnName , SymbolReference main)
        {
            var qualifierToken = columnName.start as Token;
            SymbolReference qualifier = new SymbolReference(
                new AlphanumericValue(qualifierToken),
                SymbolType.SqlIdentifier);
            main = main != null ? new QualifiedSymbolReference(main, qualifier) : qualifier;
            return (main);
        }

        private SymbolReference CreateTableOrViewOrCorrelationName(CodeElementsParser.TableOrViewOrCorrelationNameContext tableOrViewOrCorrelationName)
        {
            Token name = tableOrViewOrCorrelationName.Name as Token;
            Token schemaName = tableOrViewOrCorrelationName.SchemaName as Token;
            Token dbms = tableOrViewOrCorrelationName.DBMS as Token;
            SymbolReference fullName = CreateSymbolReference(name, schemaName, dbms);
            return fullName;
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
                var dotStarSelections = context.selections().selection().Select(CreateDotStarSelection).Where(selection => selection != null).ToList();

                return new SelectClause(selectionModifier, dotStarSelections);
            }
            return null;
        }

        private DotStarSelection CreateDotStarSelection(CodeElementsParser.SelectionContext context)
        {
            if (context.dotStarSelection() != null)
            {
                var tableOrViewOrCorrelationName = context.dotStarSelection().tableOrViewOrCorrelationName();
                SymbolReference fullName = CreateTableOrViewOrCorrelationName(tableOrViewOrCorrelationName);
                return new DotStarSelection(fullName);
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
