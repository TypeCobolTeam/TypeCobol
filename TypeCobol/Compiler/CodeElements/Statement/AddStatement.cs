using TypeCobol.Compiler.CodeElements.Expressions;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public class AddStatement : CodeElement
    {
        public AddStatement() : base(CodeElementType.AddStatement)
        {
            operations = new List<Expression>();
        }
        public List<Expression> operations { get; set; }
    }
}
