using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements.Statement
{
    /// <summary>
    ///     Format 2: SET for adjusting indexes
    /// </summary>
    public class SetStatementForIndex : SetStatement
    {
        /// <summary>
        ///     index-name
        /// </summary>
        public List<Index> ReceivingIndexs { get; set; }

        public SyntaxBoolean UpBy { get; set; }
        public SyntaxBoolean DownBy { get; set; }

        /// <summary>
        ///     identifier(numeric integer item) or positive integer
        /// </summary>
        public Expression SendingField { get; set; }

        public override string ToString()
        {
            if (ReceivingIndexs == null && UpBy == null && SendingField == null)
            {
                return base.ToString();
            }
            var sb = new StringBuilder("Set ");
            if (ReceivingIndexs != null)
            {
                foreach (var receivingIndex in ReceivingIndexs)
                {
                    sb.Append(' ');
                    sb.Append(receivingIndex);
                }
            }
            if (UpBy != null && UpBy.Value)
            {

                sb.Append(" UP BY ");
            } else if (DownBy != null && DownBy.Value)
            {
                sb.Append(" DOWN BY ");
            }
            else
            {
                sb.Append(" ");
            }
            if (SendingField != null)
            {
                sb.Append(SendingField);
            }
            sb.AppendLine(" ");
            return sb.ToString();
        }
    }
}