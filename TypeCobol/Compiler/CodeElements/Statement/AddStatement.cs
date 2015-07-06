using TypeCobol.Compiler.CodeElements.Expressions;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    public class AddStatement : CodeElement
    {
        public AddStatement() : base(CodeElementType.AddStatement)
        {
            affectations = new Dictionary<Token, int>();
            operations = new List<Expression>();
        }
        public Dictionary<Token, int> affectations { get; set; }
        public List<Expression> operations { get; set; }
    }
}
