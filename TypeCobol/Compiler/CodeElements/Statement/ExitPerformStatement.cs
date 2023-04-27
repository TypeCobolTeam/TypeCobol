
namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// EXIT PERFORM statement
    /// The EXIT PERFORM statement specifies the end of an invoked PERFORM.
    /// 
    /// </summary>
    public class ExitPerformStatement : StatementElement
    {
        public ExitPerformStatement() : base(CodeElementType.ExitPerformStatement, StatementType.ExitPerformStatement)
        { }

        /// CYCLE
        /// When an EXIT PERFORM statement without the CYCLE phrase is executed, control is passed to an implicit
        /// CONTINUE statement. This implicit CONTINUE statement immediately follows the END-PERFORM phrase
        ///  that matches the most closely preceding and unterminated inline PERFORM statement.
        /// When an EXIT PERFORM statement with the CYCLE phrase is executed, control is passed to an implicit
        /// CONTINUE statement.This implicit CONTINUE statement immediately precedes the END-PERFORM
        /// phrase that matches the most closely preceding and unterminated inline PERFORM statement.
        public SyntaxProperty<bool> Cycle { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, Cycle);
        }
    }
}