using System.Collections.Generic;
using System.Linq;
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
                fromClause = CreateFromClause(context.sql_selectClause());
            }
            return new SubSelect(selectClause, fromClause);
              
        }

        private FromClause CreateFromClause(CodeElementsParser.Sql_selectClauseContext context)
        {
            var tableReferences = new List<SymbolReference>();
            if (context.from_clause() != null)
            {
                foreach (var tableContext in context.from_clause().table_references().table_reference())
                {
                    #region tableOrViewOrCorrelationName

                    var tableOrViewOrCorrelationName =
                        tableContext.single_table_or_view_reference().tableOrViewOrCorrelationName();
                    Token name = tableOrViewOrCorrelationName.Name as Token;
                    Token schemaName = tableOrViewOrCorrelationName.SchemaName as Token;
                    Token dbms = tableOrViewOrCorrelationName.DBMS as Token;
                    SymbolReference fullName = CreateSymbolReference(name, schemaName, dbms);

                    #endregion

                    #region Correlation_Clause

                    if ((tableContext.single_table_or_view_reference().correlation_clause() != null))
                    {
                        #region CorrelationName

                        var correlation_Clause = tableContext.single_table_or_view_reference().correlation_clause();
                        Token correlationNameToken = correlation_Clause.correlation_name as Token;
                        SymbolReference correlationName =
                            new SymbolReference(new AlphanumericValue(correlationNameToken), SymbolType.SqlIdentifier);

                        #endregion

                        #region ColumnNames


                        if (correlation_Clause.new_column_names() != null)
                        {
                            SymbolReference main = null;
                            foreach (var columnName in correlation_Clause.new_column_names().new_column_name())
                            {
                                var qualifierToken = columnName.start as Token;
                                SymbolReference qualifier = new SymbolReference(
                                    new AlphanumericValue(qualifierToken),
                                    SymbolType.SqlIdentifier);
                                main = main != null ? new QualifiedSymbolReference(main, qualifier) : qualifier;
                            }

                            QualifiedSymbolReference correlationClause =
                                new QualifiedSymbolReference(correlationName, main);
                            QualifiedSymbolReference singleTableOrViewReference =
                                new QualifiedSymbolReference(fullName, correlationClause);
                            tableReferences.Add(singleTableOrViewReference);
                        }
                        else
                        {
                            QualifiedSymbolReference singleTableOrViewReference =
                                new QualifiedSymbolReference(fullName, correlationName);
                            tableReferences.Add(singleTableOrViewReference);
                        }
                    }
                    else
                    {
                        tableReferences.Add(fullName);
                        
                    }

                    #endregion

                    #endregion
                }
                return (new FromClause(tableReferences));
                


            }
            return null;
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
                Token name = tableOrViewOrCorrelationName.Name as Token;
                Token schemaName = tableOrViewOrCorrelationName.SchemaName as Token;
                Token dbms = tableOrViewOrCorrelationName.DBMS as Token;
                SymbolReference fullName = CreateSymbolReference(name, schemaName, dbms);
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
