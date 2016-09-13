using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    ///     Format 4: SET for condition-names (to true)
    /// </summary>
    internal class SetStatementForCondition : SetStatement
    {
        /// <summary>
        ///     identifier (condition-name)
        /// </summary>
        public List<Identifier> ConditionIdentifiers { get; set; }


        public override string ToString()
        {
            if (ConditionIdentifiers == null)
            {
                return base.ToString();
            }
            var sb = new StringBuilder(base.ToString());
            foreach (Identifier conditionIdentifier in ConditionIdentifiers)
            {
                sb.Append(' ');
                sb.Append(conditionIdentifier);
            }
            sb.AppendLine(" TO TRUE");
            return sb.ToString();
        }
    }
}