using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class StringStatement : StatementElement
    {
        public StringStatement() : base(CodeElementType.StringStatement, StatementType.StringStatement) { }


// TODO        public IList<Expression> Expressions { get {return new List<Expression> {this.IntoIdentifier};} }

        /// <summary>
        /// 
        /// </summary>
        public List<StringStatementWhat> StringStatementWhat { get; set; }


        /// <summary>
        /// identifier-3
        /// 
        /// INTO phrase: Identifies the receiving field (identifier-3).
        /// 
        /// Constraints:
        /// - identifier-3 must reference data items described explicitly
        ///   or implicitly as usage DISPLAY, DISPLAY-1, or NATIONAL.
        ///
        /// - identifier-3 must not reference a data item of category numeric-edited,
        /// alphanumeric-edited, or national-edited; an external floating-point data item of
        /// usage DISPLAY, or an external floating-point data item of usage NATIONAL.
        ///
        /// - identifier-3 must not described with the JUSTIFIED clause.
        /// </summary>
        public ReceivingStorageArea IntoIdentifier { get; set; }
        

        /// <summary>
        /// identifier-4
        /// 
        /// POINTER phrase
        /// Points to a character position in the receiving field. The pointer field
        /// indicates a relative alphanumeric character position, DBCS character
        /// position, or national character position when the receiving field is of usage
        /// DISPLAY, DISPLAY-1, or NATIONAL, respectively.
        /// identifier-4
        /// Represents the pointer field. identifier-4 must be large enough to
        /// contain a value equal to the length of the receiving field plus 1.
        /// You must initialize identifier-4 to a nonzero value before execution
        /// of the STRING statement begins.
        /// 
        /// 
        /// identifier-4 must not be described with the symbol P in its PICTURE
        /// character-string.
        /// </summary>
        public ReceivingStorageArea PointerIdentifier { get; set; }

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


        

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (StringStatementWhat == null && IntoIdentifier == null && PointerIdentifier == null)
            {
                return base.ToString();
            }
            else
            {
                var sb = new StringBuilder(base.ToString());
                if (StringStatementWhat != null)
                {
                    sb.Append("- variables to concat =");
                    foreach (var statementWhat in StringStatementWhat)
                    {
                        sb.Append(" ").Append(statementWhat);
                    }
                }

                if (IntoIdentifier != null)
                {
                    sb.AppendLine(" into = " + IntoIdentifier);
                }

                if (PointerIdentifier != null)
                {
                    sb.AppendLine(" pointer = " + PointerIdentifier);
                }
                return sb.ToString();
            }
        }

    }

	public class StringStatementWhat
	{
		/// <summary>
		/// identifier-1 or literal-1
		/// </summary>
		public List<Variable> IdentifierToConcat { get; set; }

		private Variable _delimiterIdentifier;
		/// <summary>
		/// identifier-2 or literal-2
		/// </summary>
		public Variable DelimiterIdentifier {
			get { return _delimiterIdentifier; }
			set {
				if (DelimitedBySize) throw new Exception("TODO");
				_delimiterIdentifier = value;
			}
		}

		private bool _size;
		public bool DelimitedBySize {
			get { return _size; }
			set {
				if (DelimiterIdentifier != null) throw new Exception("TODO");
				_size = value;
			}
		}



        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (IdentifierToConcat == null && DelimiterIdentifier == null && !DelimitedBySize)
            {
                return base.ToString();
            }
            else
            {
                var sb = new StringBuilder("");
                if (IdentifierToConcat != null)
                {
                    foreach (var idOrLiteral in IdentifierToConcat)
                    {
                        sb.Append(' ');
                        sb.Append(idOrLiteral);
                    }
                    sb.Append("   ");
                    //sb.AppendLine();
                }

                if (DelimiterIdentifier != null)
                {
                    sb.AppendLine(" delimited by " + DelimiterIdentifier);
                }

				if (DelimitedBySize) sb.AppendLine(" delimited by Size");

                return sb.ToString();
            }
        }

    }
}
