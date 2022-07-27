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

            bool isInteger = false;
            if (emptyClauseSet)
            {
                DiagnosticUtils.AddError(alterSequenceStatement,
                    "At least one option among RESTART, INCREMENT, MINVALUE, MAXVALUE, CYCLE, CACHE or ORDER must be specified",
                    context);
            }

            if (alterSequenceStatement.MinValue != null)
            {
                CheckIsInteger(alterSequenceStatement.MinValue.Literal);
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        " In minValue only digits '0' are allowed after decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.MaxValue != null)
            {
                var decimalLiteral = (DecimalLiteralTokenValue)alterSequenceStatement.MaxValue.Literal.LiteralValue;
                CheckIsInteger(alterSequenceStatement.MaxValue.Literal);
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "In maxValue only digits '0' are allowed after decimal point",
                        context);
                }

                if (alterSequenceStatement.MinValue != null)
                {
                    var minDecimalLiteral = (DecimalLiteralTokenValue)alterSequenceStatement.MinValue.Literal.LiteralValue;
                    if (minDecimalLiteral.Number > decimalLiteral.Number)
                    {
                        DiagnosticUtils.AddError(alterSequenceStatement,
                            "The maxValue must be greater than or equal to the minimum value.",
                            context);
                    }
                }
            }

            if (alterSequenceStatement.RestartValue != null)
            {
                CheckIsInteger(alterSequenceStatement.RestartValue.Literal);
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "In RESTART value only digits '0' are allowed after decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.IncrementValue != null)
            {
                CheckIsInteger(alterSequenceStatement.IncrementValue.Literal);
                if (!isInteger)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "The INCREMENT value only digits '0' are allowed after decimal point",
                        context);
                }
            }

            if (alterSequenceStatement.CacheSize != null)
            {
                var tokenValue = alterSequenceStatement.CacheSize.Literal.LiteralValue;
                var integerLiteral = (IntegerLiteralTokenValue) tokenValue;
                if (integerLiteral.Number < 2)
                {
                    DiagnosticUtils.AddError(alterSequenceStatement,
                        "Minimum value for cache size is 2",
                        context);
                }

            }
            void CheckIsInteger(Token optionValue)
            {
                var tokenValue = optionValue.LiteralValue;
                if (tokenValue.Type != LiteralTokenValueType.Decimal) return;
                var decimalLiteral = (DecimalLiteralTokenValue)tokenValue;
                isInteger = Math.Abs(decimalLiteral.Number % 1) < double.Epsilon;
            }
        }

    }
}
