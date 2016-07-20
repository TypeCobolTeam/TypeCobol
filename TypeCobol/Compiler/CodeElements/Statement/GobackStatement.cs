using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p338:
    /// The GOBACK statement functions like the EXIT PROGRAM statement when it is
    /// coded as part of a called program (or the EXIT METHOD statement when
    /// GOBACK is coded as part of an invoked method) and like the STOP RUN
    /// statement when coded in a main program.
    ///
    /// The GOBACK statement specifies the logical end of a called program or invoked
    /// method.
    ///
    /// A GOBACK statement should appear as the only statement or as the last of a
    /// series of imperative statements in a sentence because any statements following the
    /// GOBACK are not executed. GOBACK must not be used in a declarative procedure
    /// in which the GLOBAL phrase is specified.
    ///
    /// If control reaches a GOBACK statement while a CALL statement is active, control
    /// returns to the point in the calling program or method immediately following the
    /// CALL statement, as in the EXIT PROGRAM statement.
    ///
    /// If control reaches a GOBACK statement while an INVOKE statement is active,
    /// control returns to the point in the invoking program or method immediately
    /// following the INVOKE statement, as in the EXIT METHOD statement.
    ///
    /// In addition, the execution of a GOBACK statement in a called program that
    /// possesses the INITIAL attribute is equivalent to executing a CANCEL statement
    /// referencing that program.
    /// </summary>
    public class GobackStatement : StatementElement
    {
        public GobackStatement() : base(CodeElementType.GobackStatement, StatementType.GobackStatement)
        { }
    }
}
