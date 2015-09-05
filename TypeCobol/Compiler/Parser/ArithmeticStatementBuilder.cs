using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    class ArithmeticStatementBuilder
    {
        private char op;
        public ArithmeticOperationStatement statement { get; private set; }
        private ArithmeticExpressionBuilder builder;

        public ArithmeticStatementBuilder(char op)
        {
            this.op = op;
            this.statement = ArithmeticOperationStatement.Create(op);
            this.builder = new ArithmeticExpressionBuilder();
        }

        private void InitializeFormat1RightOperand(Expression left,
            IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> rightContext)
        {
            // note: "ADD a b TO c d." gives c = a+b+c and d = a+b+d
            // note: "SUBTRACT a b FROM c d." gives c = c-a+b and d = d-a+b
            // so add/subtract the "left" operand to all the elements of the "right" operand added together
            foreach (var operand in rightContext)
            {
                Expression right = CreateIdentifierRounded(operand);
                Expression operation = ArithmeticOperation.Create(left, op, right);
                statement.Affectations.Add(right, operation);
            }
        }

        public void InitializeFormat1Statement(
            IReadOnlyList<CobolCodeElementsParser.IdentifierOrNumericLiteralContext> leftContext,
            IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> rightContext)
        {
            // create the "left" operand of this addition
            Expression left = builder.CreateAddition(leftContext);
            if (left != null && rightContext != null)
            {
                InitializeFormat1RightOperand(left, rightContext);
            }
        }

        public void InitializeFormat1Statement(
            CobolCodeElementsParser.IdentifierOrNumericLiteralContext leftContext,
            IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> rightContext)
        {
            // create the "left" operand of this addition
            Expression left = builder.CreateNumberOrIdentifier(leftContext);
            if (left != null && rightContext != null)
            {
                InitializeFormat1RightOperand(left, rightContext);
            }
        }

        public void InitializeFormat2Statement(
            IReadOnlyList<CobolCodeElementsParser.IdentifierOrNumericLiteralContext> leftContext,
            CobolCodeElementsParser.IdentifierOrNumericLiteralTmpContext rightContext,
            IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> resultContext)
        {
            Expression operation = builder.CreateAddition(leftContext);
            if (operation != null && rightContext != null)
            {
                Expression right = builder.CreateNumberOrIdentifier(rightContext.identifierOrNumericLiteral());
                operation = ArithmeticOperation.Create(operation, op, right);
            }

            if (operation != null && resultContext != null)
            {
                InitializeFormat2Result(resultContext, operation);
            }
        }

        public void InitializeFormat2Statement(
            CobolCodeElementsParser.IdentifierOrNumericLiteralContext leftContext,
            CobolCodeElementsParser.IdentifierOrNumericLiteralTmpContext rightContext,
            IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> resultContext)
        {
            Expression operation = builder.CreateNumberOrIdentifier(leftContext);
            if (operation != null && rightContext != null)
            {
                Expression right = builder.CreateNumberOrIdentifier(rightContext.identifierOrNumericLiteral());
                operation = ArithmeticOperation.Create(operation, op, right);
            }

            if (operation != null && resultContext != null)
            {
                InitializeFormat2Result(resultContext, operation);
            }
        }

        private void InitializeFormat2Result(IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> resultContext, Expression operation)
        {
            foreach (var operand in resultContext)
            {
                Expression result = CreateIdentifierRounded(operand);
                statement.Affectations.Add(result, operation);
            }
        }

        public void InitializeFormat3Statement(
            CobolCodeElementsParser.IdentifierContext leftContext,
            CobolCodeElementsParser.IdentifierRoundedContext rightContext)
        {
            Expression left = null;
            if (leftContext != null)
            {
                left = SyntaxElementBuilder.CreateIdentifier(leftContext);
            }
            if (left != null && rightContext != null)
            {
                Expression right = CreateIdentifierRounded(rightContext);
                Expression operation = ArithmeticOperation.Create(left, op, right);
                statement.Affectations.Add(right, operation);
            }
        }

        internal static Expression CreateIdentifierRounded(CobolCodeElementsParser.IdentifierRoundedContext operand)
        {
            Expression identifier = SyntaxElementBuilder.CreateIdentifier(operand.identifier());
            if (operand.ROUNDED() != null) identifier = new Rounded(identifier);
            return identifier;
        }

        internal static IList<Expression> CreateIdentifiersRounded(IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> operands)
        {
            var expressions = new List<Expression>();
            foreach (var operand in operands) expressions.Add(CreateIdentifierRounded(operand));
            return expressions;
        }
    }

    class ComputeStatementBuilder
    {
        internal ComputeStatement CreateComputeStatement(CobolCodeElementsParser.ComputeStatementContext context)
        {
            if (context == null) return null;
            var statement = new ComputeStatement();
            var right = new ArithmeticExpressionBuilder().CreateArithmeticExpression(context.arithmeticExpression());
            if (context.identifierRounded() != null)
            {
                foreach (var identifier in context.identifierRounded())
                {
                    var left = ArithmeticStatementBuilder.CreateIdentifierRounded(identifier);
                    statement.Affectations.Add(left, right);
                }
            }
            return statement;
        }
    }

    class DivideStatementBuilder
    {
        internal DivideStatement CreateStatement(CobolCodeElementsParser.DivideStatementContext context)
        {
            if (context != null)
            {
                if (context.divideSimple() != null) return CreateStatement(context.divideSimple());
                if (context.divideGiving() != null) return CreateStatement(context.divideGiving());
            }
            return new DivideStatement();
        }

        private DivideStatement CreateStatement(CobolCodeElementsParser.DivideSimpleContext context)
        {
            var statement = new DivideStatement();

            if (context.dDivisor() == null)
                DiagnosticUtils.AddError(statement, "Missing: <identifier> or <literal> divisor", context);
            if (context.identifierRounded() == null)
                DiagnosticUtils.AddError(statement, "Missing: <identifier> dividend", context);

            var builder = new ArithmeticExpressionBuilder();
            var divisor = context.dDivisor();
            Expression denominator = divisor == null ? null : builder.CreateNumberOrIdentifier(divisor.identifierOrNumericLiteral());

            var quotients = ArithmeticStatementBuilder.CreateIdentifiersRounded(context.identifierRounded());
            foreach (var q in quotients)
                statement.Affectations.Add(q, ArithmeticOperation.Create(q, '÷', denominator));

            return statement;
        }

        private DivideStatement CreateStatement(CobolCodeElementsParser.DivideGivingContext context)
        {
            var statement = new DivideStatement();

            if (context.dDividend() == null)
                DiagnosticUtils.AddError(statement, "Missing: <identifier> or <literal> dividend", context);
            if (context.dDivisor() == null)
                DiagnosticUtils.AddError(statement, "Missing: <identifier> or <literal> divisor", context);
            var builder = new ArithmeticExpressionBuilder();
            var dividend = context.dDividend();
            var divisor = context.dDivisor();
            Expression numerator = dividend == null ? null : builder.CreateNumberOrIdentifier(dividend.identifierOrNumericLiteral());
            Expression denominator = divisor == null ? null : builder.CreateNumberOrIdentifier(divisor.identifierOrNumericLiteral());
            var quotient = ArithmeticOperation.Create(numerator, '÷', denominator);
            var quotients = ArithmeticStatementBuilder.CreateIdentifiersRounded(context.identifierRounded());
            foreach (var q in quotients) statement.Affectations.Add(q, quotient);

            if (context.REMAINDER() != null && context.identifier() == null)
                DiagnosticUtils.AddError(statement, "Missing: <identifier> after REMAINDER", context);
            if (context.REMAINDER() == null && context.identifier() != null)
                DiagnosticUtils.AddError(statement, "Missing: REMAINDER", context);
            if (context.identifier() != null)
            {
                if (quotients.Count > 1 && context.REMAINDER() != null)
                    DiagnosticUtils.AddError(statement, "Error: only one <identifier> allowed before REMAINDER", context);
                var identifier = SyntaxElementBuilder.CreateIdentifier(context.identifier());
                var remainder = ArithmeticOperation.Create(numerator, '/', denominator);
                statement.Affectations.Add(identifier, remainder);
            }
            return statement;
        }

    }


}