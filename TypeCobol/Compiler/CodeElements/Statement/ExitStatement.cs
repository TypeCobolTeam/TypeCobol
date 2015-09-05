namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The EXIT statement provides a common end point for a series of procedures.
    ///
    /// The EXIT statement enables you to assign a procedure-name to a given point
    /// in a program.
    /// The EXIT statement is treated as a CONTINUE statement. Any statements
    /// following the EXIT statement are executed.
    /// </summary>
    public class ExitStatement : CodeElement
    {
        public ExitStatement() : base(CodeElementType.ExitStatement) { }
    }
}