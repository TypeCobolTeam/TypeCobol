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
        public CommitStatement CreateCommitStatement(CodeElementsParser.CommitStatementContext context)
        {
            return new CommitStatement();
        }
        public CodeElement CreateTruncateStatement(CodeElementsParser.TruncateStatementContext context)
        {
            var tableName = CreateTableOrAliasName(context.tableName);
            var storageManagementClause =
                CreateStorageManagementClause(context.storageManagementClause());
            var deleteTriggersHandlingClause = CreateDeleteTriggersHandlingClause(context.deleteTriggersHandlingClause());
            var isImmediate = context.SQL_IMMEDIATE() != null ? new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.SQL_IMMEDIATE())) : null;
            return new TruncateStatement(tableName, storageManagementClause, deleteTriggersHandlingClause,isImmediate);
        }

        private SyntaxProperty<TruncateStatement.StorageManagementOption> CreateStorageManagementClause(CodeElementsParser.StorageManagementClauseContext context)
        {
            SyntaxProperty<TruncateStatement.StorageManagementOption> storageManagement = null;
            if (context.reuse() != null)
            {
                storageManagement = new SyntaxProperty<TruncateStatement.StorageManagementOption>(TruncateStatement.StorageManagementOption.ReuseStorage, ParseTreeUtils.GetFirstToken(context.reuse()));
            }
           
            else if (context.SQL_DROP()!=null)
            {
                storageManagement = new SyntaxProperty<TruncateStatement.StorageManagementOption>(TruncateStatement.StorageManagementOption.DropStorage, ParseTreeUtils.GetFirstToken(context.SQL_DROP()));
            }
            return storageManagement;
        }

        private SyntaxProperty<TruncateStatement.DeleteTriggersHandlingOption> CreateDeleteTriggersHandlingClause(CodeElementsParser.DeleteTriggersHandlingClauseContext context)
        {
            SyntaxProperty<TruncateStatement.DeleteTriggersHandlingOption> deleteTriggersHandling = null;
            if (context.SQL_RESTRICT() != null)
            {
                deleteTriggersHandling =
                    new SyntaxProperty<TruncateStatement.DeleteTriggersHandlingOption>(
                        TruncateStatement.DeleteTriggersHandlingOption.RestrictWhenDeleteTriggers,
                        ParseTreeUtils.GetFirstToken(context.SQL_RESTRICT()));
            }
            else if (context.ignore() != null)
            {
                deleteTriggersHandling =
                    new SyntaxProperty<TruncateStatement.DeleteTriggersHandlingOption>(
                        TruncateStatement.DeleteTriggersHandlingOption.IgnoreDeleteTriggers,
                        ParseTreeUtils.GetFirstToken(context.ignore()));
            }

            return deleteTriggersHandling;
        }

        public RollbackStatement CreateRollbackStatement(CodeElementsParser.RollbackStatementContext context)
        {
            SavePointClause savePointClause = null;
            if (context.savePointClause() != null)
            {
                savePointClause = CreateSavePointClause(context.savePointClause());
            }
            return new RollbackStatement(savePointClause);
        }

        private SavePointClause CreateSavePointClause(CodeElementsParser.SavePointClauseContext context)
        {
            SymbolReference savePointName = null;
            if (context.savePoint_name != null)
            {
                savePointName = new SymbolReference(new AlphanumericValue((Token)context.savePoint_name), SymbolType.SqlIdentifier);
            }
            return new SavePointClause(savePointName);
        }

        public SelectStatement CreateSelectStatement(CodeElementsParser.SelectStatementContext context)
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
            if (context.from_clause() != null)
            {
                fromClause = CreateFromClause(context.from_clause());
            }
            return new SubSelect(selectClause, fromClause);
        }

        private FromClause CreateFromClause(CodeElementsParser.From_clauseContext context)
        {
            var tableReferences = new List<SingleTableReference>();
            if (context.table_references() != null)
            {
                foreach (var tableContext in context.table_references().table_reference())
                {
                    SingleTableReference tableReference = CreateSingleTableOrViewReference(tableContext);
                    tableReferences.Add(tableReference);
                }

                return new FromClause(tableReferences);
            }

            return null;
        }

        private SingleTableReference CreateSingleTableOrViewReference(CodeElementsParser.Table_referenceContext context)
        {
            var tableRef = CreateTableOrAliasName(context
                .single_table_or_view_reference().tableOrViewOrCorrelationName());
            CorrelationClause correlationClause = null;
            if ((context.single_table_or_view_reference().correlation_clause() != null))
            {
                 correlationClause =
                    CreateCorrelationClause(context.single_table_or_view_reference().correlation_clause());
            }
            SingleTableReference tableReference = new SingleTableReference(tableRef, correlationClause);
            return tableReference;
        }

        private CorrelationClause CreateCorrelationClause(CodeElementsParser.Correlation_clauseContext context)
        {
            Token correlationNameToken = context.correlation_name as Token;
            SymbolReference correlationName = new SymbolReference(
                new AlphanumericValue(correlationNameToken), SymbolType.SqlIdentifier);

            if (context.new_column_names() != null)
            {
                List<SymbolReference> newColumnNamesList = new List<SymbolReference>();
                foreach (var columnName in context.new_column_names().new_column_name())
                {
                    SymbolReference newColumnName = new SymbolReference(
                        new AlphanumericValue(columnName.start as Token),
                        SymbolType.SqlIdentifier);
                    newColumnNamesList.Add(newColumnName);
                }
                CorrelationClause correlationClause = new CorrelationClause(correlationName, newColumnNamesList);
                return correlationClause;
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
                var tableOrViewAllColumnsSelections = context.selections().selection().Select(selection =>
                    new TableOrViewAllColumnsSelection(CreateTableOrAliasName(selection.tableOrViewAllColumnsSelection()
                        .tableOrViewOrCorrelationName()))).ToList();
                return new SelectClause(selectionModifier, tableOrViewAllColumnsSelections);
            }

            return null;
        }

        private SymbolReference CreateSymbolReference(Token nameToken, Token qualifierToken,
            Token topLevelQualifierToken)
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
                        QualifiedSymbolReference tail = new QualifiedSymbolReference( qualifier, topLevelQualifier);
                        SymbolReference fullName= new QualifiedSymbolReference( name, tail);
                        return fullName;
                    }
                    else
                    {
                        SymbolReference fullName = new QualifiedSymbolReference(name,qualifier);
                        return fullName;
                    }
                }
                else
                {
                    return name;
                }
            }

            return null;
        }

        private DatetimeConstant CreateDatetimeConstant(CodeElementsParser.Datetime_constantContext context)
        {
            var literal = ParseTreeUtils.GetTokenFromTerminalNode(context.AlphanumericLiteral());
            if (context.date() != null)
            {
                return new DatetimeConstant(literal, DatetimeConstantKind.Date, (Token)context.date().KeywordDATE);
            }

            if (context.time() != null)
            {
                return new DatetimeConstant(literal, DatetimeConstantKind.Time, (Token)context.time().KeywordTIME);
            }

            if (context.timestamp() != null)
            {
                return new DatetimeConstant(literal, DatetimeConstantKind.Timestamp, (Token)context.timestamp().KeywordTIMESTAMP);
            }

            return null;
        }

        public WhenEverStatement CreateWhenEverStatement(CodeElementsParser.WhenEverStatementContext context)
        {
            if (context != null)
            {
                SyntaxProperty<ExceptionConditionType> exceptionCondition = null;
                SyntaxProperty<BranchingType> branchingType = null;
                SymbolReference targetSectionOrParagraph = null;
                if (context.sqlError() != null)
                {
                    exceptionCondition = new SyntaxProperty<ExceptionConditionType>(ExceptionConditionType.SqlError,
                        ParseTreeUtils.GetFirstToken(context.sqlError()));
                }
                else if (context.sqlWarning() != null)
                {
                    exceptionCondition = new SyntaxProperty<ExceptionConditionType>(ExceptionConditionType.SqlWarning,
                        ParseTreeUtils.GetFirstToken(context.sqlWarning()));
                }
                else if (context.sqlNotFound() != null)
                {
                    exceptionCondition = new SyntaxProperty<ExceptionConditionType>(ExceptionConditionType.NotFound,
                        ParseTreeUtils.GetFirstToken(context.sqlNotFound()));
                }

                if (context.SQL_CONTINUE() != null)
                {
                    branchingType = new SyntaxProperty<BranchingType>(BranchingType.Continue,
                        ParseTreeUtils.GetFirstToken(context.SQL_CONTINUE()));
                }
                else if (context.sqlGotoHostLabel() != null)
                {
                    branchingType = new SyntaxProperty<BranchingType>(BranchingType.Goto,
                        ParseTreeUtils.GetFirstToken(context.sqlGotoHostLabel()));
                    if (context.sqlGotoHostLabel().hostLabel != null)
                    {
                        targetSectionOrParagraph = new SymbolReference(
                            new AlphanumericValue((Token)context.sqlGotoHostLabel().hostLabel),
                            SymbolType.SqlIdentifier);
                    }
                }

                return new WhenEverStatement(exceptionCondition, branchingType, targetSectionOrParagraph);
            }

            return null;
        }
        public LockTableStatement CreateLockTableStatement(CodeElementsParser.LockTableStatementContext context)
        {
            var tableName = CreateTableOrAliasName(context.tableOrViewOrCorrelationName());
            var partitionId = context.IntegerLiteral() != null
                ? new SqlConstant(ParseTreeUtils.GetFirstToken(context.IntegerLiteral()))
                : null;
            SyntaxProperty<LockMode> mode = null;
            if (context.share() != null)
            {
                mode =
                    new SyntaxProperty<LockMode>(LockMode.Shared, ParseTreeUtils.GetFirstToken(context.share()));
            }
            else if (context.exclusive() != null)
            {
                mode =
                    new SyntaxProperty<LockMode>(LockMode.Exclusive, ParseTreeUtils.GetFirstToken(context.exclusive()));
            }

            return new LockTableStatement(tableName, partitionId, mode);
        }

        public DropTableStatement CreateDropTableStatement(CodeElementsParser.DropTableStatementContext context)
        {
            if (context.tableOrAliasName() == null) return null;
            var tableOrAliasName = CreateTableOrAliasName(context.tableOrAliasName().tableOrViewOrCorrelationName());
            return new DropTableStatement(tableOrAliasName);
        }

        private TableViewCorrelationName CreateTableOrAliasName(CodeElementsParser.TableOrViewOrCorrelationNameContext context)
        {
            if (context.Name == null) return null;
            var nameToken = (Token) context.Name;
            Token topLevelQualifierToken = null;
            if (context.SchemaName != null)
            {
                var qualifierToken = (Token) context.SchemaName;
                if (context.DBMS != null)
                {
                    topLevelQualifierToken = (Token)context.DBMS;
                }
                return new TableViewCorrelationName(CreateSymbolReference(nameToken, qualifierToken,
                    topLevelQualifierToken));
            }

            var aliasName = new AmbiguousSymbolReference(new AlphanumericValue(nameToken),
                new SymbolType[] {SymbolType.SqlIdentifier, SymbolType.SqlIdentifier});
            return new TableViewCorrelationName(aliasName);
        }
    }
}