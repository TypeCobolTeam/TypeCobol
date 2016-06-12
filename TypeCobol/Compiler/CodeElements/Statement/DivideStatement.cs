using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p325:
    /// The DIVIDE statement divides one numeric data item into or by others and sets
    /// the values of data items equal to the quotient and remainder.
    /// </summary>
    public abstract class DivideStatement : StatementElement
    {
        public DivideStatement(StatementType statementType) : base(CodeElementType.DivideStatement, statementType)
        { }
    }

    /// <summary>
    /// In format 1, the value of identifier-1 or literal-1 is divided into the value of identifier-2, and the quotient is then stored in identifier-2. 
    /// For each successive occurrence of identifier-2, the division takes place in the left-to-right order in which identifier-2 is specified.
    /// </summary>
    public class DivideSimpleStatement : DivideStatement
    {
        public DivideSimpleStatement() : base(StatementType.DivideSimpleStatement)
        { }
    }

    /// <summary>
    /// Format 2: DIVIDE statement with INTO and GIVING phrases
    /// In format 2, the value of identifier-1 or literal-1 is divided into the value of identifier-2 or literal-2.
    /// The value of the quotient is stored in each data item referenced by identifier-3.
    /// 
    /// Format 3: DIVIDE statement with BY and GIVING phrase
    /// In format 3, the value of identifier-1 or literal-1 is divided by the value of identifier-2 or literal-2. 
    /// The value of the quotient is stored in each data item referenced by identifier-3.
    /// </summary>
    public class DivideGivingStatement : DivideStatement
    {
        public DivideGivingStatement() : base(StatementType.DivideGivingStatement)
        { }
    }

    /// <summary>
    /// Format 4: DIVIDE statement with INTO and REMAINDER phrases
    /// In format 4, the value of identifier-1 or literal-1 is divided into identifier-2 or literal-2. 
    /// The value of the quotient is stored in identifier-3, and the value of the remainder is stored in identifier-4.
    /// 
    /// Format 5: DIVIDE statement with BY and REMAINDER phrase
    /// In format 5, the value of identifier-1 or literal-1 is divided by identifier-2 or literal-2.
    /// The value of the quotient is stored in identifier-3, and the value of the remainder is stored in identifier-4.
    /// </summary>
    public class DivideRemainderStatement : DivideStatement
    {
        public DivideRemainderStatement() : base(StatementType.DivideRemainderStatement)
        { }
    }
}
