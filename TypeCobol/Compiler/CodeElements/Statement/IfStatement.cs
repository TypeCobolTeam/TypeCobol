using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class IfStatement : CodeElement
    {
        public LogicalExpression condition = null;

        public IfStatement() : base(CodeElementType.IfStatement) { }
    }
}
