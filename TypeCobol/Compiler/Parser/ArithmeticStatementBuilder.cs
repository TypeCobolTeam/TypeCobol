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
                var token = ParseTreeUtils.GetFirstToken(operand);
                Expression right = CreateIdentifierRounded(operand);
                Expression operation = ArithmeticOperation.Create(left, op, right);
                statement.affectations.Add(new SymbolReference<DataName>(new DataName(token)), operation);
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
                var token = ParseTreeUtils.GetFirstToken(operand);
                statement.affectations.Add(new SymbolReference<DataName>(new DataName(token)), operation);
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
                var token = ParseTreeUtils.GetFirstToken(rightContext);
                Expression right = CreateIdentifierRounded(rightContext);
                Expression operation = ArithmeticOperation.Create(left, op, right);
                statement.affectations.Add(new SymbolReference<DataName>(new DataName(token)), operation);
            }
        }

        internal static Expression CreateIdentifierRounded(CobolCodeElementsParser.IdentifierRoundedContext operand)
        {
            Expression identifier = SyntaxElementBuilder.CreateIdentifier(operand.identifier());
            if (operand.ROUNDED() != null) identifier = new Rounded(identifier);
            return identifier;
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
}