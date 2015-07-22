using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    
    public abstract class Expression
    {
        public abstract string TextValue();
    }
    public abstract class ArithmeticExpression : Expression { }

    public class ArithmeticOperation : ArithmeticExpression
    {
        public Expression left { get; set; }
        public Expression right { get; set; }
        public char op { get; set; }

        public ArithmeticOperation(Expression left, char op, Expression right)
        {
            this.left = left;
            this.right = right;
            this.op = op;
        }

        public static ArithmeticOperation Create(Expression left, char op, Expression right)
        {
            switch (op)
            {
                case '+': return new Addition(left, right);
                case '-': return new Subtraction(left, right);
                case '×': return new Multiplication(left, right);
                default: throw new System.ArgumentException("Illegal operator \""+op+"\"");
            }
        }

        public override string TextValue()
        {
            return new StringBuilder(left.TextValue()).Append(" ").Append(right.TextValue()).ToString();
        }

        public override string ToString() { //RPN
            return new StringBuilder(left.ToString()).Append(" ").Append(right.ToString()).Append(" ").Append(op).ToString();
        }
    }

    public class Addition : ArithmeticOperation
    {
        public Addition(Expression left, Expression right)
            : base(left, '+', right) { }
    }

    public class Subtraction : ArithmeticOperation
    {
        public Subtraction(Expression left, Expression right)
            : base(left, '-', right) { }
    }

    public class Multiplication : ArithmeticOperation
    {
        public Multiplication(Expression left, Expression right)
            : base(left, '×', right) { }
    }

    public class Number : ArithmeticExpression
    {
        public SyntaxNumber number { get; set; }
        public Number(SyntaxNumber number)
        {
            this.number = number;
        }

        public override string TextValue()
        {
            if (number == null)
            {
                return base.ToString();
            }
            else
            {
                return number.ToString();
            }
        }

        public override string ToString()
        {
            return TextValue();
        }


    }


    //TODO this class is only use for display literals for now.
    //osmedile: I'm not sure if literal must inherits from Expression (so Literal and identifier can be seen as the same object) 
    //if this class can inherits from Expression, rename it to "Literal"
    public class LiteralForDisplay : Expression
    {
        public Token token { get; set; }
        public LiteralForDisplay(Token token)
        {
            this.token = token;
        }
        public override string ToString() { return  TextValue();}

        public override string TextValue()
        {
            return token.Text; 
        }
    }
}
