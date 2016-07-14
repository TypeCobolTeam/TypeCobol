using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Conditional expression case for the SEARCH statement.
    /// </summary>
    public class WhenSearchCondition : StatementElement
    {
        public WhenSearchCondition() : base(CodeElementType.WhenSearchCondition, StatementType.WhenSearchCondition)
        { }

        /// <summary>
        /// WHEN phrase (serial search) 
        /// condition-1 
        /// Can be any condition described under “Conditional expressions” on page 256.
        /// 
        /// WHEN phrase (binary search)
        /// If a relation condition is specified in the WHEN phrase, the evaluation of the relation 
        /// is based on the USAGE of the data item referenced by data-name-1. The search argument is 
        /// moved to a temporary data item with the same USAGE as data-name-1, and this temporary 
        /// data item is used for the compare operations associated with the SEARCH.
        ///  
        /// IMPORTANT : p413/414
        /// The more restrictive syntax for binary search can not be distinguished 
        /// from the full syntax allowed for serial search at this parsing stage.
        /// We need to check the following restriction at the second parsing stage :
        ///
        /// whenBinarySearchCondition :
        ///     WHEN binarySearchCondition (AND searchCondition)*
        ///
        /// binarySearchCondition:
        ///     (variable2 IS? ((EQUAL TO?) | EqualOperator) variableOrExpression2) | 
        ///     conditionNameConditionOrSwitchStatusCondition;
        /// </summary>
        public ConditionalExpression ConditionalExpression { get; set; }
    }
}
