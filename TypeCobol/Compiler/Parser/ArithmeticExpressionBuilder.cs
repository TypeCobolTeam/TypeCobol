using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    internal class ArithmeticExpressionBuilder
    {
        internal Expression CreateNumberOrIdentifier(CodeElementsParser.IdentifierOrNumericLiteralContext context)
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

        internal Expression CreateNumberOrIdentifier(CodeElementsParser.IdentifierOrIntegerContext context)
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

        internal Expression CreateAddition(IReadOnlyList<CodeElementsParser.IdentifierOrNumericLiteralContext> operands)
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

        

        internal TypeCobol.Compiler.CodeElements.Expressions.ArithmeticExpression CreateArithmeticExpression(CodeElementsParser.ArithmeticExpressionContext context) {
            if (context.identifier() != null) return new ArithmeticIdentifier(SyntaxElementBuilder.CreateIdentifier(context.identifier()));
            if (context.numericLiteral() != null) return new Number(SyntaxElementBuilder.CreateSyntaxNumber(context.numericLiteral()));

            TypeCobol.Compiler.CodeElements.Expressions.ArithmeticExpression result = null;
            char op = CreateOperator(context);
            var members = context.arithmeticExpression();
            if (members.Length > 1) {
                var left  = CreateArithmeticExpression(members[0]);
                var right = CreateArithmeticExpression(members[1]);
                result = new ArithmeticOperation(left, op, right);
            } else
            if (members.Length > 0) {
                result = CreateArithmeticExpression(members[0]);
                if (context.LeftParenthesisSeparator() == null && context.RightParenthesisSeparator() == null) {
                    if (op == '-') result = new ArithmeticOperation(new Zero(), '-', result);
                }
            }
            return result;
        }

        private char CreateOperator(CodeElementsParser.ArithmeticExpressionContext context) {
            char op = '?';
            if (context.PlusOperator()     != null) op = '+';
            if (context.MinusOperator()    != null) op = '-';
            if (context.MultiplyOperator() != null) op = '×';
            if (context.DivideOperator()   != null) op = '÷';
            if (context.PowerOperator()    != null) op = '^';
            return op;
        }
    }
}