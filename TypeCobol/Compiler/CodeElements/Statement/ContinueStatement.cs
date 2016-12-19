using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p319: CONTINUE statement
    /// The CONTINUE statement is a no operation statement. CONTINUE indicates that no executable instruction is present.
    /// </summary>
    public class ContinueStatement : StatementElement
    {
        public ContinueStatement() : base(CodeElementType.ContinueStatement, StatementType.ContinueStatement)
        { }
    }
}
