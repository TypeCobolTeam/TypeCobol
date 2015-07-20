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
    }

    public class AddStatement : ArithmeticOperationStatement
    {
        public AddStatement() : base(CodeElementType.AddStatement) { }
    }

    public class SubtractStatement : ArithmeticOperationStatement
    {
        public SubtractStatement() : base(CodeElementType.SubtractStatement) { }
    }
}
