using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

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
                        "Invalid savepoint-name, it must not begin with 'SYS'.", context);
                }
            }
        }
    }

    internal static class AlterSequenceStatementChecker
    {
        public static void OnCodeElement(AlterSequenceStatement alterSequenceStatement,
            List<string> duplicatedClauses, bool emptyClauseSet,
            CodeElementsParser.AlterSequenceStatementContext context)
        {
            foreach (var clauseType in duplicatedClauses)
            {
                DiagnosticUtils.AddError(alterSequenceStatement,
                    clauseType + " cannot be specified more than once",
                    context);
            }

            if (emptyClauseSet)
            {
                DiagnosticUtils.AddError(alterSequenceStatement,
                    "At least one option among RESTART, INCREMENT, MINVALUE, MAXVALUE, CYCLE, CACHE or ORDER must be specified",
                    context);
            }

            if (alterSequenceStatement.MinValue != null)
            {
                var tokenValue = alterSequenceStatement.MinValue.Literal.LiteralValue;
                if (tokenValue.Type != LiteralTokenValueType.Decimal) return;
                var decimalLiteral = (DecimalLiteralTokenValue) tokenValue;
                bool isInteger = Math.Abs(decimalLiteral.Number % 1) < double.Epsilon;
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        " The minValue decimal should be without non-zero digits in the right of the decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.MaxValue != null)
            {
                var tokenValue = alterSequenceStatement.MaxValue.Literal.LiteralValue;
                if (tokenValue.Type != LiteralTokenValueType.Decimal) return;
                var decimalLiteral = (DecimalLiteralTokenValue) tokenValue;
                bool isInteger = Math.Abs(decimalLiteral.Number % 1) < double.Epsilon;
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "The maxValue decimal should be without non-zero digits in the right of the decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.RestartValue != null)
            {
                var tokenValue = alterSequenceStatement.RestartValue.Literal.LiteralValue;
                if (tokenValue.Type != LiteralTokenValueType.Decimal) return;
                var decimalLiteral = (DecimalLiteralTokenValue) tokenValue;
                bool isInteger = Math.Abs(decimalLiteral.Number % 1) < double.Epsilon;
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "The restartValue decimal should be without non-zero digits in the right of the decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.IncrementValue != null)
            {
                var tokenValue = alterSequenceStatement.IncrementValue.Literal.LiteralValue;
                if (tokenValue.Type != LiteralTokenValueType.Decimal) return;
                var decimalLiteral = (DecimalLiteralTokenValue) tokenValue;
                bool isInteger = Math.Abs(decimalLiteral.Number % 1) < double.Epsilon;
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "The IncrementValue decimal should be without non-zero digits in the right of the decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.CacheSize != null)
            {
                var tokenValue = alterSequenceStatement.CacheSize.Literal.LiteralValue;
                if (tokenValue.Type != LiteralTokenValueType.Integer)
                {

                    DiagnosticUtils.AddError(alterSequenceStatement, "The cache size should be an integer", context);
                }
            }
        }

    }
}
