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

        public ArithmeticStatementBuilder(char op)
        {
            this.op = op;
            this.statement = ArithmeticOperationStatement.Create(op);
        }

        private SyntaxNumber CreateNumberLiteral(CobolCodeElementsParser.NumericLiteralContext context)
        {
            if (context.IntegerLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
            }
            if (context.DecimalLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.DecimalLiteral()));
            }
            if (context.FloatingPointLiteral() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.FloatingPointLiteral()));
            }
            if (context.ZERO() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZERO()));
            }
            if (context.ZEROS() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROS()));
            }
            if (context.ZEROES() != null)
            {
                return new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.ZEROES()));
            }
            throw new System.Exception("This is not a number!");
        }

        private Expression createOperand(CobolCodeElementsParser.IdentifierOrNumericLiteralContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null)
            {
                return new Identifier(context.identifier());
            }
            if (context.numericLiteral() != null)
            {
                return new Number(CreateNumberLiteral(context.numericLiteral()));
            }
            return null;
        }

        private Expression createLeftOperand(IReadOnlyList<CobolCodeElementsParser.IdentifierOrNumericLiteralContext> operands)
        {
            if (operands == null) return null;

            Expression left = null;
            foreach (var operand in operands)
            {
                Expression tail = createOperand(operand);
                if (tail == null) continue;
                if (left == null)
                {
                    // first element of the list that is the "left" operand
                    left = tail;
                }
                else
                {
                    // add this element to the others, to get the sum that is the "left" operand
                    left = new Addition(left, tail);
                }
            }
            return left;
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
            Expression left = createLeftOperand(leftContext);
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
            Expression left = createOperand(leftContext);
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
            Expression operation = createLeftOperand(leftContext);
            if (operation != null && rightContext != null)
            {
                Expression right = createOperand(rightContext.identifierOrNumericLiteral());
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
            Expression operation = createOperand(leftContext);
            if (operation != null && rightContext != null)
            {
                Expression right = createOperand(rightContext.identifierOrNumericLiteral());
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