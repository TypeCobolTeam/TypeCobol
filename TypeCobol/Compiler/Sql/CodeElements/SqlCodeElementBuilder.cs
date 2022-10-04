using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
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
            return new TruncateStatement(tableName, storageManagementClause, deleteTriggersHandlingClause, isImmediate);
        }

        private SyntaxProperty<TruncateStatement.StorageManagementOption> CreateStorageManagementClause(CodeElementsParser.StorageManagementClauseContext context)
        {
            SyntaxProperty<TruncateStatement.StorageManagementOption> storageManagement = null;
            if (context.reuse() != null)
            {
                storageManagement = new SyntaxProperty<TruncateStatement.StorageManagementOption>(TruncateStatement.StorageManagementOption.ReuseStorage, ParseTreeUtils.GetFirstToken(context.reuse()));
            }

            else if (context.SQL_DROP() != null)
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

        private SymbolReference CreateSymbolReference(Token nameToken, Token qualifierToken = null,
            Token topLevelQualifierToken = null)
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
                        QualifiedSymbolReference tail = new QualifiedSymbolReference(qualifier, topLevelQualifier);
                        SymbolReference fullName = new QualifiedSymbolReference(name, tail);
                        return fullName;
                    }
                    else
                    {
                        SymbolReference fullName = new QualifiedSymbolReference(name, qualifier);
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

        private SqlConstant CreateSqlConstant(CodeElementsParser.NumericConstantContext context)
        {
            var terminalNode = context.IntegerLiteral() ?? context.DecimalLiteral();
            return CreateSqlConstant(terminalNode);
        }

        private SqlConstant CreateSqlConstant(ITerminalNode terminal)
        {
            return new SqlConstant(ParseTreeUtils.GetTokenFromTerminalNode(terminal));
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

        public SavepointStatement CreateSavepointStatement(CodeElementsParser.SavepointStatementContext context)
        {
            if (context == null) return null;
            SymbolReference savepointName = null;
            if (context.savepoint_name != null)
            {
                savepointName = new SymbolReference(new AlphanumericValue((Token) context.savepoint_name),
                    SymbolType.SqlIdentifier);
            }

            var retainLocks = context.sqlLocks() != null
                ? new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.sqlLocks()))
                : null;
            var isUnique = context.SQL_UNIQUE() != null
                ? new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.SQL_UNIQUE()))
                : null;
            return new SavepointStatement(savepointName, retainLocks, isUnique);
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
            var partitionId = context.IntegerLiteral() != null ? CreateSqlConstant(context.IntegerLiteral()) : null;
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

        public ReleaseSavepointStatement CreateReleaseSavepointStatement(
            CodeElementsParser.ReleaseSavepointStatementContext context)
        {
            var savepointName = context.Diagnostics == null ? new SymbolReference(new AlphanumericValue((Token)context.savepoint_name), SymbolType.SqlIdentifier) : null;
            return new ReleaseSavepointStatement(savepointName);
        }
        public ConnectStatement CreateConnectStatement(CodeElementsParser.ConnectStatementContext context)
        {
            ConnectionTarget connectionTarget = null;
            ConnectionAuthorization connectionAuthorization = null;
            SyntaxProperty<bool> reset = null;
            if (context.connectionTarget() != null)
            {
                connectionTarget = CreateConnectionTarget(context.connectionTarget());
                if (context.connectionTarget().authorizationClause() != null)
                {
                    connectionAuthorization = CreateConnectionAuthorization(context.connectionTarget().authorizationClause());
                }

            }
            else if (context.authorizationClause() != null)
            {
                connectionAuthorization = CreateConnectionAuthorization(context.authorizationClause());
            }

            else if (context.sqlReset() != null)
            {
                reset = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.sqlReset()));
            }

            return new ConnectStatement(connectionAuthorization, reset, connectionTarget);
        }

        private ConnectionTarget CreateConnectionTarget(CodeElementsParser.ConnectionTargetContext context)
        {
            SyntaxValue<string> locationNameLiteral = null;
            HostVariable locationNameVariable = null;
            if (context.locationName != null)
            {
                locationNameLiteral = new AlphanumericValue(ParseTreeUtils.GetFirstToken(context.UserDefinedWord()));
            }

            else if (context.hostVariable() != null)
            {
                locationNameVariable = CreateSqlHostVariable(context.hostVariable());
            }

            return new ConnectionTarget(locationNameLiteral, locationNameVariable);
        }

        private ConnectionAuthorization CreateConnectionAuthorization(CodeElementsParser.AuthorizationClauseContext context)
        {
            HostVariable userName = null;
            HostVariable password = null;
            if (context.userName != null)
            {
                userName = CreateSqlHostVariable(context.userName);
            }

            if (context.password != null)
            {
                password = CreateSqlHostVariable(context.password);
            }

            return new ConnectionAuthorization(userName, password);
        }

        public DropTableStatement CreateDropTableStatement(CodeElementsParser.DropTableStatementContext context)
        {
            var tableOrAliasName = CreateTableOrAliasName(context.tableOrAliasName());
            return new DropTableStatement(tableOrAliasName);
        }

        private SymbolReference CreateTableOrAliasName(CodeElementsParser.TableOrAliasNameContext context)
        {
            if (context.tableOrViewOrCorrelationName().Name == null) return null;
            var nameToken = (Token)context.tableOrViewOrCorrelationName().Name;
            if (context.tableOrViewOrCorrelationName().SchemaName != null)
            {
                var qualifierToken = (Token)context.tableOrViewOrCorrelationName().SchemaName;
                var topLevelQualifierToken = (Token)context.tableOrViewOrCorrelationName().DBMS;
                return CreateSymbolReference(nameToken, qualifierToken,
                    topLevelQualifierToken);
            }

            var aliasName = new AmbiguousSymbolReference(new AlphanumericValue(nameToken),
                new [] { SymbolType.SqlIdentifier, SymbolType.SqlIdentifier });
            return aliasName;
        }

        public SetAssignmentStatement CreateSetAssignmentStatement(CodeElementsParser.SetAssignmentStatementContext context)
        {
            IList<Assignment> assignments = context.assignmentClause().Select(CreateAssignmentClause)
                .Where(a => a != null).ToList();
            return new SetAssignmentStatement(assignments);
        }

        private Assignment CreateAssignmentClause(CodeElementsParser.AssignmentClauseContext context)
        {
            if (context.simpleAssignmentClause() != null)
            {
                return CreateSimpleAssignmentClause(context.simpleAssignmentClause());
            }
            
            if (context.multipleAssignmentClause() != null)
            { 
                return CreateMultipleAssignmentClause(context.multipleAssignmentClause());
            }
            
            return null;
        }

        private Assignment CreateSimpleAssignmentClause(CodeElementsParser.SimpleAssignmentClauseContext context)
        {
            IList<TargetVariable> targets = new List<TargetVariable>();
            IList<SourceValue> values = new List<SourceValue>();
            if (context.sqlSetTargetVariable() != null)
            {
                var variable = CreateTargetVariable(context.sqlSetTargetVariable());
                if (variable != null)
                {
                    targets.Add(variable);
                }
            }
            if (context.sourceValue() != null)
            {
                values.Add(CreateSourceValue(context.sourceValue()));
            }
            return new Assignment(targets, values);
        }

        private Assignment CreateMultipleAssignmentClause(CodeElementsParser.MultipleAssignmentClauseContext context)
        {
            IList<SourceValue> values;
            IList<TargetVariable> targets = context.sqlSetTargetVariable().Select(CreateTargetVariable).Where(v => v != null).ToList();

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
            else
            {
                values = new List<SourceValue>();
            }

            return new Assignment(targets, values);
        }

        private List<SourceValue> CreateRepeatedSourceValue(CodeElementsParser.RepeatedSourceValueContext context)
        {
            var sourceValues = context.sourceValue().Select(CreateSourceValue).ToList();
            return sourceValues;
        }

        private TargetVariable CreateTargetVariable(CodeElementsParser.SqlSetTargetVariableContext context)
        {
            return context.sqlVariable() != null ? new TargetVariable(CreateSqlVariable(context.sqlVariable())) : null;
        }

        private SourceValue CreateSourceValue(CodeElementsParser.SourceValueContext context)
        {
            SyntaxProperty<bool> isDefault = null;
            SqlExpression expression = null;
            if (context.sqlExpression() != null)
            {
                expression = CreateSqlExpression(context.sqlExpression());
            }
            else if (context.SQL_DEFAULT() != null)
            {
                isDefault = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.SQL_DEFAULT()));
            }

            return new SourceValue(expression, isDefault);
        }

        private SqlVariable CreateSqlVariable(CodeElementsParser.SqlVariableContext context)
        {
            if (context.hostVariable() != null)
            {
                return CreateSqlHostVariable(context.hostVariable());
            }
            //TODO Add other conditions when adding new Sql Variable Types 
            return null;
        }

        private SqlExpression CreateSqlExpression(CodeElementsParser.SqlExpressionContext context)
        {
            if (context.column_name() != null)
            {
               return CreateSqlColumnName(context.column_name());
            }

            if (context.sqlConstant() != null)
            {
                return CreateSqlConstant(context.sqlConstant());
            }

            if (context.sqlVariable() != null)
            {
                return CreateSqlVariable(context.sqlVariable());
            }
            return null;
        }

        private SqlConstant CreateSqlConstant(CodeElementsParser.SqlConstantContext context)
        {
            return context.datetime_constant() != null
                ? CreateDatetimeConstant(context.datetime_constant())
                : new SqlConstant(ParseTreeUtils.GetFirstToken(context));
        }

        /*
        private SqlConstant CreateSqlConstant(ITerminalNode node)
        {
            return node != null ? new SqlConstant(ParseTreeUtils.GetTokenFromTerminalNode(node)) : null;
        }
        */

        public GetDiagnosticsStatement CreateGetDiagnosticsStatement(CodeElementsParser.GetDiagnosticsStatementContext context)
        {
            var isCurrent = context.SQL_CURRENT() != null ? new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.SQL_CURRENT())) : null;
            var isStacked = context.stacked() != null ? new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.stacked())) : null;
            GetDiagnosticInformation requestedInformation = null;
            if (context.statementInformationClauses() != null)
            {
                requestedInformation = CreateStatementInformation(context.statementInformationClauses());
            }
            else if (context.conditionInformationClause() != null)
            {
                requestedInformation = CreateConditionInformationClause(context.conditionInformationClause());
            }
            else if (context.combinedInformationClause() != null)
            {
                requestedInformation = CreateCombinedInformationClause(context.combinedInformationClause());
            }
            return new GetDiagnosticsStatement(isCurrent, isStacked, requestedInformation);
        }

        private StatementInformation CreateStatementInformation(CodeElementsParser.StatementInformationClausesContext context)
        {
            var assignments = context.statementInformationClause().Select(CreateInformationAssignment).ToList();
            return new StatementInformation(assignments);
        }

        private InformationAssignment CreateInformationAssignment(CodeElementsParser.StatementInformationClauseContext context)
        {
            SymbolReference itemName = null;
            SqlVariable storage = null;
            if (context.variable_1 != null)
            {
                storage = CreateSqlVariable(context.variable_1);
            }

            if (context.statementInformationItemName != null)
            {
                itemName = CreateSymbolReference((Token)context.statementInformationItemName);
            }

            return new InformationAssignment(storage, itemName);
        }

        private ConditionInformation CreateConditionInformationClause(CodeElementsParser.ConditionInformationClauseContext context)
        {
            SqlVariable diagnosticIdVariable = null;
            SqlConstant diagnosticIdLiteral = null;
            if (context.variable_2 != null)
            {
                diagnosticIdVariable = CreateSqlVariable(context.variable_2);
            }
            else if (context.IntegerLiteral() != null)
            {
                diagnosticIdLiteral = CreateSqlConstant(context.IntegerLiteral());
            }
            var assignments = context.repeatedConnectionOrConditionInformation().Select(CreateInformationAssignment).ToList();
            return new ConditionInformation(diagnosticIdVariable, diagnosticIdLiteral, assignments);
        }


        private InformationAssignment CreateInformationAssignment(CodeElementsParser.RepeatedConnectionOrConditionInformationContext context)
        {
            SymbolReference itemName = null;
            SqlVariable storage = null;
            if (context.variable_3 != null)
            {
                storage = CreateSqlVariable(context.variable_3);
            }

            if (context.UserDefinedWord() != null)
            {
                itemName = CreateSymbolReference(ParseTreeUtils.GetTokenFromTerminalNode(context
                    .UserDefinedWord()));
            }

            return new InformationAssignment(storage, itemName);
        }

        private CombinedInformation CreateCombinedInformationClause(CodeElementsParser.CombinedInformationClauseContext context)
        {
            SqlVariable storage = null;
            var items = new List<CombinedInformationItem>();
            if (context.variable_4 != null)
            {
                storage = CreateSqlVariable(context.variable_4);
            }
            foreach (var combinedInformationItem in context.repeatedCombinedInformation())
            {
                var combinedInformation = CreateCombinedInformationItem(combinedInformationItem);
                if (combinedInformation != null)
                {
                    items.Add(combinedInformation);
                }
            }
            return new CombinedInformation(storage, items);
        }

        private CombinedInformationItem CreateCombinedInformationItem(
            CodeElementsParser.RepeatedCombinedInformationContext context)
        {
            if (context.SQL_STATEMENT() != null)
            {
                return new CombinedInformationItem(CombinedInformationItemType.Statement, null, null);
            }
            if (context.SQL_CONDITION() != null)
            {
                return NewCombinedInformationItem(CombinedInformationItemType.Condition);
            }
            if (context.SQL_CONNECTION() != null)
            {
                return NewCombinedInformationItem(CombinedInformationItemType.Connection);
            }
            return null;

            CombinedInformationItem NewCombinedInformationItem(CombinedInformationItemType combinedInformationItemType)
            {
                SqlVariable diagnosticIdVariable = null;
                SqlConstant diagnosticIdLiteral = null;
                if (context.variable_5 != null)
                {
                    diagnosticIdVariable = CreateSqlVariable(context.variable_5);
                }
                else if (context.IntegerLiteral() != null)
                {
                    diagnosticIdLiteral = CreateSqlConstant(context.IntegerLiteral());
                }
                return new CombinedInformationItem(combinedInformationItemType, diagnosticIdVariable, diagnosticIdLiteral);
            }
        }



        private enum AlterSequenceClauseType
        {
            MinValue,
            MaxValue,
            Cycle,
            Order,
            Cache,
            Restart,
            Increment
        }

        public AlterSequenceStatement CreateAlterSequenceStatement(
            CodeElementsParser.AlterSequenceStatementContext context)
        {
            var duplicates = new List<string>();
            var clauseSet = new HashSet<AlterSequenceClauseType>();

            TableViewCorrelationName sequenceName = null;
            SyntaxProperty<bool> restart = null;
            SqlConstant restartValue = null;
            SqlConstant incrementValue = null;
            SyntaxProperty<bool> hasMinValue = null;
            SqlConstant minValue = null;
            SyntaxProperty<bool> hasMaxValue = null;
            SqlConstant maxValue = null;
            SyntaxProperty<bool> cycle = null;
            SyntaxProperty<bool> hasCache = null;
            SqlConstant cacheSize = null;
            SyntaxProperty<bool> ordered = null;

            if (context.sequence_name != null)
            {
                sequenceName = CreateTableOrViewOrCorrelationName(context.sequence_name);
            }

            foreach (var alterSequenceClauseContext in context.alterSequenceClause())
            {
                if (alterSequenceClauseContext.restartClause() != null)
                {
                    SetOption(alterSequenceClauseContext.restartClause(), AlterSequenceClauseType.Restart,
                        c => restart = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(c)));
                    if (alterSequenceClauseContext.restartClause().numericConstant() != null)
                    {
                        restartValue = CreateSqlConstant(alterSequenceClauseContext.restartClause().numericConstant());
                    }
                }

                if (alterSequenceClauseContext.incrementClause() != null &&
                    alterSequenceClauseContext.incrementClause().numericConstant() != null)
                {
                    SetOption(alterSequenceClauseContext.incrementClause(), AlterSequenceClauseType.Increment,
                        c => incrementValue = CreateSqlConstant(c.numericConstant()));
                }

                if (alterSequenceClauseContext.minValueClause() != null &&
                    alterSequenceClauseContext.minValueClause().numericConstant() != null)
                {
                    SetOption(alterSequenceClauseContext.minValueClause(), AlterSequenceClauseType.MinValue,
                        c =>
                        {
                            hasMinValue = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(c));
                            minValue = CreateSqlConstant(c.numericConstant());
                        });
                }

                if (alterSequenceClauseContext.maxValueClause() != null &&
                    alterSequenceClauseContext.maxValueClause().numericConstant() != null)
                {
                    SetOption(alterSequenceClauseContext.maxValueClause(), AlterSequenceClauseType.MaxValue,
                        c =>
                        {
                            hasMaxValue = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(c));
                            maxValue = CreateSqlConstant(c.numericConstant());
                        });
                }

                if (alterSequenceClauseContext.cycle() != null)
                {
                    SetOption(alterSequenceClauseContext.cycle(), AlterSequenceClauseType.Cycle,
                        c => cycle = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(c)));
                }

                if (alterSequenceClauseContext.cacheClause() != null &&
                    alterSequenceClauseContext.cacheClause().IntegerLiteral() != null)
                {
                    SetOption(alterSequenceClauseContext.cacheClause(), AlterSequenceClauseType.Cache,
                        c =>
                        {
                            hasCache = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(c));
                            cacheSize = CreateSqlConstant(c.IntegerLiteral());
                        });
                }

                if (alterSequenceClauseContext.SQL_ORDER() != null)
                {
                    SetOption(alterSequenceClauseContext.SQL_ORDER(), AlterSequenceClauseType.Order,
                        c => ordered = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(c)));
                }

                if (alterSequenceClauseContext.noClauses() != null)
                {
                    var noKeyword = alterSequenceClauseContext.noClauses().SQL_NO();
                    if (alterSequenceClauseContext.noClauses().minvalue() != null)
                    {
                        SetOption(noKeyword, AlterSequenceClauseType.MinValue,
                            c => hasMinValue =
                                new SyntaxProperty<bool>(false, ParseTreeUtils.GetTokenFromTerminalNode(c)));
                    }

                    if (alterSequenceClauseContext.noClauses().maxvalue() != null)
                    {
                        SetOption(noKeyword, AlterSequenceClauseType.MaxValue,
                            c => hasMaxValue =
                                new SyntaxProperty<bool>(false, ParseTreeUtils.GetTokenFromTerminalNode(c)));
                    }

                    if (alterSequenceClauseContext.noClauses().cycle() != null)
                    {
                        SetOption(noKeyword, AlterSequenceClauseType.Cycle,
                            c => cycle = new SyntaxProperty<bool>(false, ParseTreeUtils.GetTokenFromTerminalNode(c)));
                    }

                    if (alterSequenceClauseContext.noClauses().cache() != null)
                    {
                        SetOption(noKeyword, AlterSequenceClauseType.Cache,
                            c => hasCache =
                                new SyntaxProperty<bool>(false, ParseTreeUtils.GetTokenFromTerminalNode(c)));
                    }

                    if (alterSequenceClauseContext.noClauses().SQL_ORDER() != null)
                    {
                        SetOption(noKeyword, AlterSequenceClauseType.Order,
                            c => ordered = new SyntaxProperty<bool>(false, ParseTreeUtils.GetTokenFromTerminalNode(c)));
                    }
                }
            }

            var alterSequenceStatement = new AlterSequenceStatement(sequenceName, restart, restartValue, incrementValue,
                hasMinValue, minValue, hasMaxValue, maxValue, cycle, hasCache, cacheSize, ordered);
            AlterSequenceStatementChecker.OnCodeElement(alterSequenceStatement, duplicates, clauseSet.Count == 0,
                context);
            return alterSequenceStatement;

            void SetOption<TClause>(TClause clause, AlterSequenceClauseType type, Action<TClause> set)
                where TClause : IParseTree
            {
                if (clauseSet.Add(type))
                {
                    set(clause);
                }
                else
                {
                    duplicates.Add(type.ToString().ToUpper());
                }
            }
        }

        public ExecuteImmediateStatement CreateExecuteImmediateStatement(CodeElementsParser.ExecuteImmediateStatementContext context)
        {
            SqlVariable statementVariable = null;
            StringExpression statementExpression = null;

            if (context.sqlVariable() != null)
            {
                statementVariable = CreateSqlVariable(context.sqlVariable());
            }
            else if (context.stringExpression() != null)
            {
                statementExpression = CreateStringExpression(context.stringExpression());
            }

            return new ExecuteImmediateStatement(statementVariable, statementExpression);
        }

        private StringExpression CreateStringExpression(CodeElementsParser.StringExpressionContext context)
        {
            if (context.AlphanumericLiteral() != null)
            {
                return new StringExpression(CreateSqlConstant(context.AlphanumericLiteral()));
            }

            return null;
        }
    }
}
