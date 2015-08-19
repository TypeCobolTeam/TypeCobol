using TypeCobol.Compiler.CodeElements.Expressions;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    public class ArithmeticOperationStatement : CodeElement
    {
        public ArithmeticOperationStatement(CodeElementType type)
            : base(type) {
            this.Affectations = new Dictionary<Expression, Expression>();
        }
        public Dictionary<Expression, Expression> Affectations { get; set; }

        public static ArithmeticOperationStatement Create(char op)
        {
            switch (op)
            {
                case '+': return new AddStatement();
                case '-': return new SubtractStatement();
                case '×': return new MultiplyStatement();
                default: throw new System.ArgumentException("Illegal operator \"" + op + "\"");
            }
        }
    }

    public class AddStatement : ArithmeticOperationStatement
    {
        public AddStatement() : base(CodeElementType.AddStatement) { }
    }

    public class SubtractStatement : ArithmeticOperationStatement
    {
        public SubtractStatement() : base(CodeElementType.SubtractStatement) { }
    }

    public class MultiplyStatement : ArithmeticOperationStatement
    {
        public MultiplyStatement() : base(CodeElementType.MultiplyStatement) { }
    }

    public class ComputeStatement : ArithmeticOperationStatement
    {
        public ComputeStatement() : base(CodeElementType.ComputeStatement) { }
    }

    public class DivideStatement : ArithmeticOperationStatement
    {
        public DivideStatement() : base(CodeElementType.DivideStatement) { }
    }
}
