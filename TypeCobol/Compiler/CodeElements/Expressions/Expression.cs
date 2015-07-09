using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    public class Expression { }
    public class ArithmeticExpression : Expression { }

    public class Addition : ArithmeticExpression
    {
        public Expression left { get; set; }
        public Expression right { get; set; }

        public Addition(Expression left, Expression right)
        {
            this.left = left;
            this.right = right;
        }
        public override string ToString() { //RPN
            return left.ToString()+" "+right.ToString()+" +";
        }
    }
    public class Identifier : Expression
    {
        public Token token { get; set; }
        public bool rounded { get; set; }
        public Identifier(Token token, bool rounded = false)
        {
            this.token = token;
            this.rounded = rounded;
        }
        public override string ToString() { return token.Text; }
    }
    public class Number : ArithmeticExpression
    {
        public SyntaxNumber number { get; set; }
        public Number(SyntaxNumber number)
        {
            this.number = number;
        }
        public override string ToString() { return number.ToString(); }
    }
}
