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
            return null; //TODO
        }
    }
}