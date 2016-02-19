using System.Collections.Generic;
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

        public override string ToString() { //RPN
            return new StringBuilder(left!=null?left.ToString():"<?>").Append(" ").Append(right!=null?right.ToString():"<?>").Append(" ").Append(op).ToString();
        }

        public static LogicOperation Create(Expression left, char op, Expression right)
        {
            switch (op)
            {
                case '|': return new OR(left, right);
                case '&': return new AND(left, right);
                case '!': return new NOT(left);
                default: throw new System.ArgumentException("Illegal operator \"" + op + "\"");
            }
        }
    }

    public class OR : LogicOperation
    {
        public OR(Expression left, Expression right)
            : base(left, '|', right) { }
    }

    public class AND : LogicOperation
    {
        public AND(Expression left, Expression right)
            : base(left, '&', right) { }
    }

    public class NOT : LogicOperation
    {
        public NOT(Expression expression)
            : base(expression, '!', null) { }

        public override string ToString() {
            return new StringBuilder("NOT( ").Append(left.ToString()).Append(" )").ToString();
        }
    }

    public class Relation : LogicOperation
    {
        public Relation(Expression left, char op, Expression right)
            : base(left, op, right) { }
    }

    public class Boolean : LogicalExpression
    {
        public SyntaxBoolean value { get; set; }
        public Boolean(SyntaxBoolean value)
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

    public class ClassCondition : LogicalExpression
    {
        public Expression expression { get; set; }
        public Symbol type { get; set; }

        public ClassCondition(Expression expression, Symbol type)
        {
            this.expression = expression;
            this.type = type;
        }

        public override string ToString()
        {
            return new StringBuilder(type.ToString()).Append(" ").Append(type).Append(" ?").ToString();
        }
    }

    public class SignCondition : LogicalExpression
    {
        public Expression operand { get; private set; }
        public enum Type { ZERO, POSITIVE, NEGATIVE, UNKNOWN }
        public Type type { get; private set; }
        public bool not { get; private set; }

        public SignCondition(Expression operand, bool not, Type type) {
            this.operand = operand;
            this.not = not;
            this.type = type;
        }

        public override string ToString()
        {
            var str = new StringBuilder(operand.ToString()).Append(" IS ");
            if (not) str.Append("NOT ");
            str.Append(type).Append(" ?");
            return str.ToString();
        }
    }

    public class Null : LogicalExpression
    {
        public override string ToString() { return "NIL"; }
    }

    public class Self : LogicalExpression
    {
        public override string ToString() { return "SELF"; }
    }
}
