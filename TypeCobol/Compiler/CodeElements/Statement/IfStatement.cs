using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class IfStatement : CodeElement
    {
        public ConditionalExpression condition = null;

        public IfStatement() : base(CodeElementType.IfStatement) { }
    }
}
