using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
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

		public bool UpBy   { get; set; }
		public bool DownBy { get; set; }

        /// <summary>
        ///     identifier(numeric integer item) or positive integer
        /// </summary>
        public Expression SendingField { get; set; }

        public override string ToString()
        {
            if (ReceivingIndexs == null && !UpBy && SendingField == null)
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

			if (UpBy) sb.Append(" UP BY ");
			else if (DownBy) sb.Append(" DOWN BY ");
			else sb.Append(" ");
			if (SendingField != null) sb.Append(SendingField);

            sb.AppendLine(" ");
            return sb.ToString();
        }
    }
}