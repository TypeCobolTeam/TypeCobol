using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    public class UnstringStatement : CodeElement
    {
        /// <summary>
        /// identifier-1
        /// </summary>
        public Identifier UnstringIdentifier { get; set; }

        /// <summary>
        /// Identifier-2 or  literal-1
        /// </summary>
        public Expression DelimitedBy { get; set; }

        /// <summary>
        /// identifier-3 or literal-2
        /// </summary>
        public List<Expression> OtherDelimiters { get; set; } 

        /// <summary>
        /// identifier-4 or identifier-5 or identifier-6
        /// </summary>
        public List<UnstringReceiver> UnstringReceivers { get; set; }

        /// <summary>
        /// identifier-7
        /// </summary>
        public Identifier WithPointer { get; set; }

        /// <summary>
        /// identifier-8
        /// </summary>
        public Identifier Tallying { get; set; }

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
        public List<CodeElement> OnOverflowStatement { get; set; }

        /// <summary>
        /// If at the time of execution of a STRING statement, conditions that would
        /// cause an overflow condition are not encountered, then after completion of
        /// the transfer of data, the ON OVERFLOW phrase, if specified, is ignored.
        /// Control is then transferred to the end of the STRING statement, or if the
        /// NOT ON OVERFLOW phrase is specified, to imperative-statement-2.
        /// </summary>
        public List<CodeElement> NotOnOverflowStatement { get; set; }

        public UnstringStatement() : base(CodeElementType.UnstringStatement)
        { }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            if (UnstringIdentifier == null && DelimitedBy == null && OtherDelimiters == null && UnstringReceivers == null && WithPointer == null && Tallying == null && OnOverflowStatement == null && NotOnOverflowStatement == null)
            {
                return base.ToString();
            }
            else
            {
                var sb = new StringBuilder("");
                if (UnstringIdentifier != null)
                {
                    sb.AppendLine("unstring " + UnstringIdentifier);
                }
                if (DelimitedBy != null || OtherDelimiters != null)
                {
                    sb.Append(" delimited by ");

                    if (DelimitedBy != null)
                    {
                        sb.Append(DelimitedBy);
                    }
                    if (OtherDelimiters != null)
                    {
                        foreach (var otherDelimiter in OtherDelimiters)
                        {
                            sb.Append(" or ");
                            sb.Append(otherDelimiter);
                        }
                    }
                    sb.AppendLine("");
                }

                if (UnstringReceivers != null)
                {
                    sb.Append("INTO ");
                    foreach (var unstringReceiver in UnstringReceivers)
                    {
                        if (unstringReceiver.IntoIdentifier != null)
                        {
                            sb.Append(' ');
                            sb.Append(unstringReceiver.IntoIdentifier);
                        }                       
                        if (unstringReceiver.DelimiterIdentifier != null)
                        {
                            sb.Append(" DELIMITER IN ");
                            sb.Append(unstringReceiver.DelimiterIdentifier);
                        }
                        if (unstringReceiver.CountIdentifier != null)
                        {
                            sb.Append(" COUNT IN ");
                            sb.Append(unstringReceiver.CountIdentifier);
                        }
                    }
                    sb.AppendLine("");
                }

                if (WithPointer != null)
                {
                    sb.Append(" WITH POINTER ");
                    sb.AppendLine(WithPointer.ToString());
                }
                if (Tallying != null)
                {
                    sb.Append(" TALLYING IN ");
                    sb.AppendLine(Tallying.ToString());
                }
                return sb.ToString();
            }
        }
    }

    public class UnstringReceiver
    {
        /// <summary>
        /// identifier-4
        /// </summary>
        public Identifier IntoIdentifier { get; set; }

        /// <summary>
        /// identifier-5
        /// </summary>
        public Identifier DelimiterIdentifier { get; set; }

        /// <summary>
        /// identifier-6
        /// </summary>
        public Identifier CountIdentifier { get; set; }
    }
}
