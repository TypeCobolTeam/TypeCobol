using System.Text;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    public abstract class LogicalExpression : Expression { }

    public class LogicOperation : LogicalExpression
    {
        public Expression left { get; set; }
        public Expression right { get; set; }
        public char op { get; set; }

        public LogicOperation(Expression left, char op, Expression right)
        {
            this.left = left;
            this.right = right;
            this.op = op;
        }

        public static LogicOperation Create(Expression left, char op, Expression right)
        {
            switch (op)
            {
                case '|': return new OR(left, right);
                case '&': return new AND(left, right);
                case '!': return new NOT(left, right);
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

    public class OR : LogicOperation
    {
        public OR(Expression left, Expression right)
            : base(left, '+', right) { }
    }

    public class AND : LogicOperation
    {
        public AND(Expression left, Expression right)
            : base(left, '-', right) { }
    }

    public class NOT : LogicOperation
    {
        public NOT(Expression left, Expression right)
            : base(left, '×', right) { }
    }

    public class Boolean : ArithmeticExpression
    {
        public SyntaxBoolean value { get; set; }
        public Boolean(SyntaxBoolean value)
        {
            this.value = value;
        }

        public override string TextValue()
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

        public override string ToString()
        {
            return TextValue();
        }
    }
}
