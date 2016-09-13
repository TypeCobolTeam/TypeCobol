using TypeCobol.Compiler.CodeElements.Expressions;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p284:
    /// The arithmetic statements are used for computations. Individual operations are
    /// specified by the ADD, SUBTRACT, MULTIPLY, and DIVIDE statements. These
    /// individual operations can be combined symbolically in a formula that uses the
    /// COMPUTE statement.
    /// </summary>
    public class ArithmeticOperationStatement : CodeElement
    {
        /// <summary>
        /// p285:
        /// When an arithmetic statement has multiple results, execution conceptually
        /// proceeds as follows:
        /// 1. The statement performs all arithmetic operations to find the result to be placed
        /// in the receiving items, and stores that result in a temporary location.
        /// 2. A sequence of statements transfers or combines the value of this temporary
        /// result with each single receiving field. The statements are considered to be
        /// written in the same left-to-right order in which the multiple results are listed.
        /// </summary>
        public Dictionary<Expression, Expression> Affectations { get; set; }

        public ArithmeticOperationStatement(CodeElementType type)
            : base(type) {
            this.Affectations = new Dictionary<Expression, Expression>();
        }

        public static ArithmeticOperationStatement Create(char op)
        {
            switch (op)
            {
                case '+': return new AddStatement();
                case '-': return new SubtractStatement();
                case '×': return new MultiplyStatement();
                case '÷': return new DivideStatement();
                default: throw new System.ArgumentException("Illegal operator \"" + op + "\"");
            }
        }
    }

    /// <summary>
    /// p298:
    /// The ADD statement sums two or more numeric operands and stores the result.
    /// </summary>
    public class AddStatement : ArithmeticOperationStatement
    {
        public AddStatement() : base(CodeElementType.AddStatement) { }
    }

    /// <summary>
    /// p438:
    /// The SUBTRACT statement subtracts one numeric item, or the sum of two or more
    /// numeric items, from one or more numeric items, and stores the result.
    /// </summary>
    public class SubtractStatement : ArithmeticOperationStatement
    {
        public SubtractStatement() : base(CodeElementType.SubtractStatement) { }
    }

    /// <summary>
    /// p376:
    /// The MULTIPLY statement multiplies numeric items and sets the values of data
    /// items equal to the results.
    /// </summary>
    public class MultiplyStatement : ArithmeticOperationStatement
    {
        public MultiplyStatement() : base(CodeElementType.MultiplyStatement) { }
    }

    /// <summary>
    /// p325:
    /// The DIVIDE statement divides one numeric data item into or by others and sets
    /// the values of data items equal to the quotient and remainder.
    /// </summary>
    public class DivideStatement : ArithmeticOperationStatement
    {
        public DivideStatement() : base(CodeElementType.DivideStatement) { }
    }

    /// <summary>
    /// p317:
    /// The COMPUTE statement assigns the value of an arithmetic expression to one or
    /// more data items.
    ///
    /// With the COMPUTE statement, arithmetic operations can be combined without the
    /// restrictions on receiving data items imposed by the rules for the ADD, SUBTRACT,
    /// MULTIPLY, and DIVIDE statements.
    ///
    /// When arithmetic operations are combined, the COMPUTE statement can be more
    /// efficient than the separate arithmetic statements written in a series.
    /// </summary>
    public class ComputeStatement : ArithmeticOperationStatement
    {
        public ComputeStatement() : base(CodeElementType.ComputeStatement) { }
    }
}
