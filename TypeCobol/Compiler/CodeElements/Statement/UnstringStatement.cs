using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class UnstringStatement : StatementElement
    {
        public UnstringStatement() : base(CodeElementType.UnstringStatement, StatementType.UnstringStatement) { }

/*      public IList<Expression> Expressions {
            get {
                var result = new List<Expression>();
                foreach (var unstringReceiver in this.UnstringReceivers) {
                    result.Add(unstringReceiver.IntoIdentifier);
                }
                return result;
            }
        }
*/
        /// <summary>
        /// identifier-1
        /// </summary>
        public Variable UnstringIdentifier { get; set; }

		public class Delimiter {
			public SyntaxProperty<bool> All { get; set; }
			public Variable Variable { get; set; }

			public override string ToString() {
				return (All != null && All.Value? "ALL ") + (Variable != null? Variable.ToString() : "?");
			}
		}
		/// <summary>identifier-2, identifier-3, literal-1, literal-2</summary>
		public IList<Delimiter> Delimiters { get; set; } 

		public class Receiver {
			/// <summary>identifier-4</summary>
			public ReceivingStorageArea StorageArea { get; set; }
			/// <summary>identifier-5</summary>
			public ReceivingStorageArea Delimiter { get; set; }
			/// <summary>identifier-6</summary>
			public ReceivingStorageArea Count { get; set; }

			public override string ToString() {
				var str = new StringBuilder();
				if (StorageArea != null) str.Append(StorageArea);
				else str.Append("?");
				if (Delimiter != null) str.Append(" DELIMITER IN ").Append(Delimiter);
				if (Count != null) str.Append(" COUNT IN ").Append(Count);
				return str.ToString();
			}
		}
		/// <summary>identifier-4, identifier-5, identifier-6</summary>
        public IList<Receiver> Receivers { get; set; }

		/// <summary>identifier-7</summary>
		public ReceivingStorageArea WithPointer { get; set; }

		/// <summary>identifier-8</summary>
		public ReceivingStorageArea Tallying { get; set; }

        /// <summary>
        /// Executed when the pointer value (explicit or implicit):
        ///  - Is less than 1
        ///  - Exceeds a value equal to the length of the receiving field
        /// When either of the above conditions occurs, an overflow condition exists,
        /// and no more data is transferred. Then the STRING operation is terminated,
        /// the NOT ON OVERFLOW phrase, if specified, is ignored, and control is
        /// transferred to the end of the STRING statement or, if the ON OVERFLOW
        /// phrase is specified, to imperative-statement-1.
        /// </summary>
        public List<OnOverflowCondition> OnOverflowStatement { get; set; }

        /// <summary>
        /// If at the time of execution of a STRING statement, conditions that would
        /// cause an overflow condition are not encountered, then after completion of
        /// the transfer of data, the ON OVERFLOW phrase, if specified, is ignored.
        /// Control is then transferred to the end of the STRING statement, or if the
        /// NOT ON OVERFLOW phrase is specified, to imperative-statement-2.
        /// </summary>
        public List<NotOnOverflowCondition> NotOnOverflowStatement { get; set; }

       

		public override string ToString() {
			if (UnstringIdentifier == null && Delimiters == null && Receivers == null && WithPointer == null && Tallying == null && OnOverflowStatement == null && NotOnOverflowStatement == null)
				return base.ToString();
			var str = new StringBuilder("");
			if (UnstringIdentifier != null) str.AppendLine("UNSTRING " + UnstringIdentifier);
			if (Delimiters.Count > 0) {
				str.Append(" DELIMITED BY ");
				foreach (var delimiter in Delimiters) str.Append(delimiter).Append(" OR ");
				str.Length -= 4;
				str.AppendLine();
			}
			if (Receivers.Count > 0) {
				str.Append(" INTO ");
				foreach (var receiver in Receivers) str.Append(receiver);
				str.AppendLine();
			}
			if (WithPointer != null) str.Append(" WITH POINTER ").AppendLine(WithPointer);
			if (Tallying != null) str.Append(" TALLYING IN ").AppendLine(Tallying);
		}
    }
}
