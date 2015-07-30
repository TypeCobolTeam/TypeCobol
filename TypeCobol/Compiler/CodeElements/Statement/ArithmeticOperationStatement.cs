using TypeCobol.Compiler.CodeElements.Expressions;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    public class ArithmeticOperationStatement : CodeElement
    {
        public ArithmeticOperationStatement(CodeElementType type)
            : base(type) {
            affectations = new Dictionary<SymbolReference<DataName>, Expression>();
        }
        public Dictionary<SymbolReference<DataName>, Expression> affectations { get; set; }

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
}
