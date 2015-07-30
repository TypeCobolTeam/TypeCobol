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
                Identifier right = new Identifier(operand);
                Expression operation = ArithmeticOperation.Create(left, op, right);
                statement.affectations.Add(new SymbolReference<DataName>(new DataName(right.token)), operation);
            }
        }

        public void InitializeFormat1Statement(
            IReadOnlyList<CobolCodeElementsParser.IdentifierOrNumericLiteralContext> leftContext,
            IReadOnlyList<CobolCodeElementsParser.IdentifierRoundedContext> rightContext)
        {
            // create the "left" operand of this addition
            Expression left = builder.createAddition(leftContext);
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
            Expression left = builder.createNumberOrIdentifier(leftContext);
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
            Expression operation = builder.createAddition(leftContext);
            if (operation != null && rightContext != null)
            {
                Expression right = builder.createNumberOrIdentifier(rightContext.identifierOrNumericLiteral());
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
            Expression operation = builder.createNumberOrIdentifier(leftContext);
            if (operation != null && rightContext != null)
            {
                Expression right = builder.createNumberOrIdentifier(rightContext.identifierOrNumericLiteral());
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
                Identifier right = new Identifier(operand);
                statement.affectations.Add(new SymbolReference<DataName>(new DataName(right.token)), operation);
            }
        }

        public void InitializeFormat3Statement(
            CobolCodeElementsParser.IdentifierContext leftContext,
            CobolCodeElementsParser.IdentifierRoundedContext rightContext)
        {
            Expression left = null;
            if (leftContext != null)
            {
                left = new Identifier(leftContext);
            }
            if (left != null && rightContext != null)
            {
                Identifier right = new Identifier(rightContext);
                Expression operation = ArithmeticOperation.Create(left, op, right);
                statement.affectations.Add(new SymbolReference<DataName>(new DataName(right.token)), operation);
            }
        }
    }
}