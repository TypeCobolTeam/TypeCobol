using System.Collections.Generic;
using System.Linq;
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
        public CommitStatement CreateCommitStatement(CodeElementsParser.CommitStatementContext context)
        {
            return new CommitStatement();
        }
        public CodeElement CreateTruncateStatement(CodeElementsParser.TruncateStatementContext context)
        {
            var tableName = CreateTableOrViewOrCorrelationName(context.tableName);
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
            TableViewCorrelationName tableRef = CreateTableOrViewOrCorrelationName(context
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
                List<SqlColumnName> newColumnNamesList = (from columnName in context.new_column_names().column_name() where columnName.Diagnostics == null select CreateSqlColumnName(columnName)).ToList();
                CorrelationClause correlationClause = new CorrelationClause(correlationName, newColumnNamesList);
                return correlationClause;
            }
            return null;
        }

        private TableViewCorrelationName CreateTableOrViewOrCorrelationName(CodeElementsParser.TableOrViewOrCorrelationNameContext tableOrViewOrCorrelationName)
        {
            Token name = tableOrViewOrCorrelationName.Name as Token;
            Token schemaName = tableOrViewOrCorrelationName.SchemaName as Token;
            Token dbms = tableOrViewOrCorrelationName.DBMS as Token;
            SymbolReference fullName = CreateSymbolReference(name, schemaName, dbms);
            return new TableViewCorrelationName(fullName);
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
                var tableOrViewAllColumnsSelections = context.selections().selection().Select(CreateSelection)
                    .Where(selection => selection != null).ToList();

                return new SelectClause(selectionModifier, tableOrViewAllColumnsSelections);
            }

            return null;
        }

        private TableOrViewAllColumnsSelection CreateSelection(CodeElementsParser.SelectionContext context)
        {
            if (context.tableOrViewAllColumnsSelection() != null)
            {
                var tableOrViewOrCorrelationName =
                    context.tableOrViewAllColumnsSelection().tableOrViewOrCorrelationName();
                Token name = tableOrViewOrCorrelationName.Name as Token;
                Token schemaName = tableOrViewOrCorrelationName.SchemaName as Token;
                Token dbms = tableOrViewOrCorrelationName.DBMS as Token;
                SymbolReference fullName = CreateSymbolReference(name, schemaName, dbms);
                return new TableOrViewAllColumnsSelection(new TableViewCorrelationName(fullName));

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
        private SqlColumnName CreateSqlColumnName(CodeElementsParser.Column_nameContext context)
        {
                var literal = ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord());
                var literalReferenceSymbol = new SymbolReference(new AlphanumericValue(literal), SymbolType.ColumnName);
                return new SqlColumnName(literalReferenceSymbol);
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
        private HostVariable CreateSqlHostVariable(CodeElementsParser.HostVariableContext context)
        {
            if (context.mainVariable != null)
            {
                return new HostVariable(CreateHostVariableSymbolReference(context.mainVariable), CreateHostVariableSymbolReference(context.indicatorVariable));
            }

            return null;

            SymbolReference CreateHostVariableSymbolReference(IToken userDefinedWord)
            {
                if (userDefinedWord == null) return null;
                var token = (Token) userDefinedWord;
                return new SymbolReference(new AlphanumericValue(token), SymbolType.SqlIdentifier);
            }
        }
        public LockTableStatement CreateLockTableStatement(CodeElementsParser.LockTableStatementContext context)
        {
            var tableName = CreateTableOrViewOrCorrelationName(context.tableOrViewOrCorrelationName());
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

        public SetAssignmentStatement CreateSetAssignmentStatement(CodeElementsParser.SetAssignmentStatementContext context)
        {
            IList<Assignment> assignments = context.assignmentClause().Select(CreateAssignmentClause).ToList();
            return new SetAssignmentStatement(assignments);
        }

        private Assignment CreateAssignmentClause(CodeElementsParser.AssignmentClauseContext context)
        {
            if (context.simpleAssignmentClause() != null)
            {
                return CreateSimpleAssignmentClause(context.simpleAssignmentClause());
            }
            else if (context.multipleAssignmentClause() != null)
            { 
                return CreateMultipleAssignmentClause(context.multipleAssignmentClause());
            }
            return null;
        }

        private Assignment CreateMultipleAssignmentClause(CodeElementsParser.MultipleAssignmentClauseContext context)
        {
            IList<SourceValue> values = null;
            IList<TargetVariable> targets = context.targetVariable().Select(CreateTargetVariable).ToList();

            if (context.sourceValueClause().sourceValueClauses().repeatedSourceValue() != null)
            {
                values = CreateRepeatedSourceValue(context.sourceValueClause().sourceValueClauses()
                    .repeatedSourceValue());
            }
            else if (context.sourceValueClause().sourceValueClauses().sourceValue() != null)
            {
                values = new List<SourceValue>
                {
                    CreateSourceValue(context.sourceValueClause().sourceValueClauses().sourceValue())
                };
            }

            return new Assignment(targets, values);
        }

        private Assignment CreateSimpleAssignmentClause(CodeElementsParser.SimpleAssignmentClauseContext context)
        {
            IList<TargetVariable> targets = new List<TargetVariable>();
            IList<SourceValue> values = new List<SourceValue>();
            if (context.targetVariable() != null)
            {
                targets.Add(new TargetVariable(CreateSqlVariable(context.targetVariable().sqlVariable())));
            }
            if (context.sourceValue() != null)
            {
                values.Add(CreateSourceValue(context.sourceValue()));
            }
            return new Assignment(targets, values);
        }

        private List<SourceValue> CreateRepeatedSourceValue(CodeElementsParser.RepeatedSourceValueContext context)
        {
            var sourceValues = context.sourceValue().Select(CreateSourceValue).ToList();
            return sourceValues;
        }

        private TargetVariable CreateTargetVariable(CodeElementsParser.TargetVariableContext context)
        {
            return context.sqlVariable() != null ? new TargetVariable(CreateSqlVariable(context.sqlVariable())) : null;
        }

        private SourceValue CreateSourceValue(CodeElementsParser.SourceValueContext context)
        {
            if (context.sqlExpression() == null) return null;
            var isDefault = context.SQL_DEFAULT() != null
                ? new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.SQL_DEFAULT()))
                : null;
            var expression = CreateSqlExpression(context.sqlExpression());
            return new SourceValue(expression, isDefault);
        }

        private SqlVariable CreateSqlVariable(CodeElementsParser.SqlVariableContext context)
        {
            if (context.hostVariable() != null)
            {
                return CreateSqlHostVariable(context.hostVariable());
            }
            //TODO Add other conditions when adding new  Sql Variable Types 
            return null;
        }


        private SqlExpression CreateSqlExpression(CodeElementsParser.SqlExpressionContext context)
        {
            if (context.column_name() != null)
            {
               return CreateSqlColumnName(context.column_name());
            }
            else if (context.sqlConstant() != null)
            {
                return new SqlConstant(ParseTreeUtils.GetFirstToken(context.sqlConstant()));
            }
            else if (context.sqlVariable() != null)
            {
                return CreateSqlVariable(context.sqlVariable());
            }
            else if (context.SQL_NULL() != null)
            {
                return new SqlConstant(ParseTreeUtils.GetFirstToken(context.SQL_NULL()));
            }
            return null;
        }
    }
}