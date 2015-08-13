using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
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

    public class Rounded : ArithmeticOperation
    {
        public Rounded(Expression expression)
            : base(expression, '°', null) { }

        public override string ToString()
        { //RPN
            return new StringBuilder(left.ToString()).Append(" ").Append(op).ToString();
        }
    }

    public class Number : ArithmeticExpression
    {
        public SyntaxNumber value { get; set; }
        public Number(SyntaxNumber value)
        {
            this.value = value;
        }

        public override string ToString()
        {
            if (value == null)
            {
                return base.ToString();
            }
            else
            {
                return value.ToString();
            }
        }
    }
}
