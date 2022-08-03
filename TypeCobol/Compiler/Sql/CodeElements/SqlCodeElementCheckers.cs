using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    internal static class SavepointStatementChecker
    {
        public static void OnCodeElement(SavepointStatement savepointStatement,
            CodeElementsParser.SavepointStatementContext context)
        {
            if (savepointStatement.Name != null)
            {
                var savePointName = savepointStatement.Name.ToString();
                if (savePointName.StartsWith("SYS", StringComparison.OrdinalIgnoreCase))
                {
                    DiagnosticUtils.AddError(savepointStatement,
                        "Invalid savepoint-name, it must not begin with 'SYS'.",
                        context);
                }
            }
        }
    }

    internal static class GetDiagnosticsStatementChecker
    {
        private static readonly Regex StatementInformationItemNames = new Regex("^(DB2_GET_DIAGNOSTICS_DIAGNOSTICS|DB2_SQL_NESTING_LEVEL|DB2_LAST_ROW|DB2_NUMBER_PARAMETER_MARKERS|DB2_NUMBER_RESULT_SETS|DB2_NUMBER_ROWS|DB2_RETURN_STATUS|DB2_SQL_ATTR_CURSOR_HOLD|DB2_SQL_ATTR_CURSOR_ROWSET|DB2_SQL_ATTR_CURSOR_SCROLLABLE|DB2_SQL_ATTR_CURSOR_SENSITIVITY|DB2_SQL_ATTR_CURSOR_TYPE|MORE|NUMBER|ROW_COUNT)$", RegexOptions.IgnoreCase | RegexOptions.Compiled);
        private static readonly Regex ConditionInformationItemNames = new Regex("^(CATALOG_NAME|CONDITION_NUMBER|CURSOR_NAME|DB2_ERROR_CODE1|DB2_ERROR_CODE2|DB2_ERROR_CODE3|DB2_ERROR_CODE4|DB2_INTERNAL_ERROR_POINTER|DB2_LINE_NUMBER|DB2_MESSAGE_ID|DB2_MODULE_DETECTING_ERROR|DB2_ORDINAL_TOKEN_[0-9]+|DB2_REASON_CODE|DB2_RETURNED_SQLCODE|DB2_ROW_NUMBER|DB2_SQLERRD_SET|DB2_SQLERRD1|DB2_SQLERRD2|DB2_SQLERRD3|DB2_SQLERRD4|DB2_SQLERRD5|DB2_SQLERRD6|DB2_TOKEN_COUNT|MESSAGE_TEXT|RETURNED_SQLSTATE|SERVER_NAME)$", RegexOptions.IgnoreCase | RegexOptions.Compiled);
        private static readonly Regex ConnectionInformationItemNames = new Regex("^(DB2_AUTHENTICATION_TYPE|DB2_AUTHORIZATION_ID|DB2_CONNECTION_STATE|DB2_CONNECTION_STATUS|DB2_ENCRYPTION_TYPE|DB2_SERVER_CLASS_NAME|DB2_PRODUCT_ID)$", RegexOptions.IgnoreCase | RegexOptions.Compiled);

        public static void OnCodeElement(GetDiagnosticsStatement getDiagnosticsStatement,
            CodeElementsParser.GetDiagnosticsStatementContext context)
        {
            if (getDiagnosticsStatement.RequestedInformation == null) return;

            switch (getDiagnosticsStatement.RequestedInformation.Type)
            {
                case InformationType.Statement:
                    var statementInformation = (StatementInformation)getDiagnosticsStatement.RequestedInformation;
                    CheckStatementInformationItemNames(statementInformation.Assignments);
                    break;
                case InformationType.Condition:
                    var conditionInformation = (ConditionInformation)getDiagnosticsStatement.RequestedInformation;
                    CheckDiagnosticVariableHasNoIndicator(conditionInformation.DiagnosticIdVariable);
                    CheckDiagnosticIdIsStrictlyPositive(conditionInformation.DiagnosticIdLiteral);
                    CheckConditionOrConnectionInformationItemNames(conditionInformation.Assignments);
                    break;
                case InformationType.Combined:
                    var combinedInformation = (CombinedInformation)getDiagnosticsStatement.RequestedInformation;
                    CheckCombinedInformationItems(combinedInformation.Items);
                    break;
            }

            void CheckStatementInformationItemNames(List<InformationAssignment> assignments)
            {
                foreach (var informationAssignment in assignments)
                {
                    if (informationAssignment.ItemName == null) continue;

                    var name = informationAssignment.ItemName.Name;
                    if (!StatementInformationItemNames.IsMatch(name))
                    {
                        DiagnosticUtils.AddError(getDiagnosticsStatement, $"'{name}' is not a valid name for statement information item.", context);
                    }
                }
            }

            void CheckDiagnosticIdIsStrictlyPositive(SqlConstant sqlConstant)
            {
                if (sqlConstant == null) return;

                if (sqlConstant.Literal.LiteralValue != null &&
                    sqlConstant.Literal.LiteralValue.Type == LiteralTokenValueType.Integer)
                {
                    var value = (IntegerLiteralTokenValue)sqlConstant.Literal.LiteralValue;
                    if (value.Number <= 0)
                    {
                        DiagnosticUtils.AddError(getDiagnosticsStatement, "Diagnostic identifier must be strictly positive.", context);
                    }
                }
            }

            void CheckDiagnosticVariableHasNoIndicator(SqlVariable sqlVariable)
            {
                if (sqlVariable == null) return;

                if (sqlVariable.Type == VariableType.HostVariable &&
                    ((HostVariable)sqlVariable).IndicatorReference != null)
                {
                    DiagnosticUtils.AddError(getDiagnosticsStatement, "No indicator variable are allowed for diagnostic identifiers.", context);
                }
            }

            void CheckConditionOrConnectionInformationItemNames(List<InformationAssignment> assignments)
            {
                foreach (var informationAssignment in assignments)
                {
                    if (informationAssignment.ItemName == null) continue;

                    var name = informationAssignment.ItemName.Name;
                    if (!ConditionInformationItemNames.IsMatch(name) && !ConnectionInformationItemNames.IsMatch(name))
                    {
                        DiagnosticUtils.AddError(getDiagnosticsStatement, $"'{name}' is not a valid name for condition/connection information item.", context);
                    }
                }
            }

            void CheckCombinedInformationItems(List<CombinedInformationItem> items)
            {
                var types = new HashSet<CombinedInformationItemType>();
                foreach (var item in items)
                {
                    bool hasDiagnosticIdentifier = item.DiagnosticIdVariable != null || item.DiagnosticIdLiteral != null; //Always false for STATEMENT
                    if (!types.Add(item.Type) && !hasDiagnosticIdentifier)
                    {
                        //STATEMENT can only be specified once.
                        //CONDITION and CONNECTION can only be specified once if diagnostic identifier is not also specified.
                        DiagnosticUtils.AddError(getDiagnosticsStatement, $"Duplicate {item.Type.ToString().ToUpper()} clause.", context);
                    }
                    CheckDiagnosticVariableHasNoIndicator(item.DiagnosticIdVariable);
                    CheckDiagnosticIdIsStrictlyPositive(item.DiagnosticIdLiteral);
                }
            }
        }
    }
}
