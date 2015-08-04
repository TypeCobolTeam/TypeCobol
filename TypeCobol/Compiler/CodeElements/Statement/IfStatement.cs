using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public class IfStatement : CodeElement, FlowControl
    {
        public List<CodeElement> IF   { get; private set; }
        public List<CodeElement> ELSE { get; private set; }
        public bool isIF { get; set; }
        private bool scope = true;

        public IfStatement() : base(CodeElementType.IfStatement)
        {
            isIF = true;
            IF = new List<CodeElement>();
            ELSE = new List<CodeElement>();
        }

        public bool AddNestedElement(CodeElement e)
        {
            if (!scope) return false;
            if (isIF) IF.Add(e);
            else ELSE.Add(e);
            return true;
        }

        public void CloseScope() { scope = false; }
    }
}
