using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    internal class ArithmeticExpressionBuilder
    {
        internal Expression CreateNumberOrIdentifier(CobolCodeElementsParser.IdentifierOrNumericLiteralContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null)
            {
                return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            }
            if (context.numericLiteral() != null)
            {
                return new Number(SyntaxElementBuilder.CreateSyntaxNumber(context.numericLiteral()));
            }
            return null;
        }

        internal Expression CreateNumberOrIdentifier(CobolCodeElementsParser.IdentifierOrIntegerContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null)
            {
                return SyntaxElementBuilder.CreateIdentifier(context.identifier());
            }
            if (context.IntegerLiteral() != null)
            {
                var number = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
                return new Number(number);
            }
            return null;
        }

        internal Expression CreateAddition(IReadOnlyList<CobolCodeElementsParser.IdentifierOrNumericLiteralContext> operands)
        {
            if (operands == null) return null;

            Expression head = null;
            foreach (var operand in operands)
            {
                Expression tail = CreateNumberOrIdentifier(operand);
                if (tail == null) continue;
                if (head == null)
                {
                    // first element of the list that is the "left" operand
                    head = tail;
                }
                else
                {
                    // add this element to the others, to get the sum that is the "left" operand
                    head = new Addition(head, tail);
                }
            }
            return head;
        }



        internal ArithmeticExpression CreateArithmeticExpression(CobolCodeElementsParser.ArithmeticExpressionContext context)
        {
            char op = '?';
            if (context.PlusOperator() != null)  op = '+';
            if (context.MinusOperator() != null) op = '-';
            ArithmeticExpression current = null;
            ArithmeticExpression result = null;
            if (context.multiplicationAndDivision() != null)
            {
                foreach (var operation in context.multiplicationAndDivision())
                {
                    current = CreateArithmeticExpression(operation);
                    result = CreateResult(result, op, current);
                }
            }
            return result;
        }

        private ArithmeticExpression CreateArithmeticExpression(CobolCodeElementsParser.MultiplicationAndDivisionContext context)
        {
            char op = '?';
            if (context.MultiplyOperator() != null) op = '×';
            if (context.DivideOperator() != null)   op = '÷';
            ArithmeticExpression current = null;
            ArithmeticExpression result = null;
            if (context.exponentiation() != null)
            {
                foreach (var operation in context.exponentiation())
                {
                    current = CreateArithmeticExpression(operation);
                    result = CreateResult(result, op, current);
                }
            }
            return result;
        }

        private ArithmeticExpression CreateArithmeticExpression(CobolCodeElementsParser.ExponentiationContext context)
        {
            char op = '?';
            if (context.PowerOperator() != null) op = '^';
            ArithmeticExpression current = null;
            ArithmeticExpression result = null;
            if (context.unaryOperator() != null)
            {
                foreach (var operation in context.unaryOperator())
                {
                    current = CreateArithmeticExpression(operation);
                    result = CreateResult(result, op, current);
                }
            }
            return result;
        }

        private ArithmeticExpression CreateResult(ArithmeticExpression left, char op, ArithmeticExpression right)
        {
            if (left == null) return right;
            return new ArithmeticOperation(left, op, right);
        }

        private ArithmeticExpression CreateArithmeticExpression(CobolCodeElementsParser.UnaryOperatorContext context)
        {
            ArithmeticExpression result = CreateArithmeticExpression(context.expressionBase());
            if (context.MinusOperator() != null) result = new ArithmeticOperation(new Zero(), '-', result);
            return result;
        }

        private ArithmeticExpression CreateArithmeticExpression(CobolCodeElementsParser.ExpressionBaseContext context)
        {
            if (context.identifier() != null) return new ArithmeticIdentifier(SyntaxElementBuilder.CreateIdentifier(context.identifier()));
            if (context.numericLiteral() != null) return new Number(SyntaxElementBuilder.CreateSyntaxNumber(context.numericLiteral()));
            return CreateArithmeticExpression(context.arithmeticExpression());
        }
    }
}