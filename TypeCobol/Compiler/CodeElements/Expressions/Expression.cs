using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions
{
    public class Expression { }

    public class Addition : Expression
    {
        public Expression left { get; set; }
        public Expression right { get; set; }

        public Addition(Expression left, Expression right)
        {
            this.left = left;
            this.right = right;
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
    }
    public class Literal : Expression
    {
        public Token token { get; set; }
        public Literal(Token token)
        {
            this.token = token;
        }
    }
}
