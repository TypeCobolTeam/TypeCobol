using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The INITIALIZE statement sets selected categories of data fields to predetermined
    /// values. The INITIALIZE statement is functionally equivalent to one or more MOVE
    /// statements.
    /// </summary>
    public class InitializeStatement : CodeElement
    {
        /// <summary>
        /// Receiving areas.
        /// identifier-1 must reference one of the following items:
        /// * An alphanumeric group item
        /// * A national group item
        /// * An elementary data item of one of the following categories:
        /// – Alphabetic
        /// – Alphanumeric
        /// – Alphanumeric-edited
        /// – DBCS
        /// – External floating-point
        /// – Internal floating-point
        /// – National
        /// – National-edited
        /// – Numeric
        /// – Numeric-edited
        /// * A special register that is valid as a receiving operand in a MOVE
        /// statement with identifer-2 or literal-1 as the sending operand.
        /// When identifier-1 references a national group item, identifier-1 is processed
        /// as a group item.
        ///
        /// pp344:
        /// A subscripted item can be specified for identifier-1. A complete table can be
        /// initialized only by specifying identifier-1 as a group that contains the complete
        /// table.
        ///
        /// Usage note: The data description entry for identifier-1 can contain the DEPENDING
        /// phrase of the OCCURS clause. However, you cannot use the INITIALIZE statement
        /// to initialize a variably-located item or a variable-length item.
        ///
        /// The data description entry for identifier-1 must not contain a RENAMES clause.
        ///
        /// Special registers can be specified for identifier-1 and identifier-2 only if they are
        /// valid receiving fields or sending fields, respectively, for the implied MOVE
        /// statements.
        /// </summary>
        public IList<Identifier> Receiving;

        /// <summary>
        /// Sending areas.
        ///
        /// p344:
        /// When the REPLACING phrase is not specified:
        /// * SPACE is the implied sending item for receiving items of category alphabetic,
        /// alphanumeric, alphanumeric-edited, DBCS, national, or national-edited.
        /// * ZERO is the implied sending item for receiving items of category numeric or
        /// numeric-edited.
        /// </summary>
        public IList<Replacing> Sending = new List<Replacing>();

        public InitializeStatement() : base(CodeElementType.InitializeStatement)
        { }

        /// <summary>
        /// REPLACING phrase
        /// When the REPLACING phrase is specified:
        /// * identifier-2 must reference an item of a category that is valid as a sending
        /// operand in a MOVE statement to an item of the corresponding category
        /// specified in the REPLACING phrase.
        /// * literal-1 must be of a category that is valid as a sending operand in a MOVE
        /// statement to an item of the corresponding category specified in the REPLACING
        /// phrase.
        /// * A floating-point literal, a data item of category internal floating-point, or a data
        /// item of category external floating point is treated as if it were in the NUMERIC
        /// category.
        /// * The same category cannot be repeated in a REPLACING phrase.
        ///
        /// With the exception of EGCS, the keyword after the word REPLACING corresponds
        /// to a category of data shown in “Classes and categories of data” on page 162.
        ///
        /// EGCS in the REPLACING phrase is synonymous with DBCS.
        /// </summary>
        public class Replacing
        {
            public enum Mode
            {
                ALPHABETIC,
                ALPHANUMERIC,
                ALPHANUMERIC_EDITED,
                NATIONAL,
                NATIONAL_EDITED,
                NUMERIC,
                NUMERIC_EDITED,
                DBCS,
                EGCS,
                UNKNOWN,
            }
            /// <summary>
            /// pp343-344:
            /// Sending areas
            /// When identifier-2 references a national group item, identifier-2 is processed
            /// as an elementary data item of category national.
            /// identifier-2 must reference an elementary data item (or a national group
            /// item treated as elementary) that is valid as a sending operand in a MOVE
            /// statement with identifier-1 as the receiving operand.
            /// literal-1 must be a literal that is valid as a sending operand in a MOVE
            /// statement with identifier-1 as the receiving operand.
            ///
            /// pp344:
            /// Special registers can be specified for identifier-1 and identifier-2 only if they are
            /// valid receiving fields or sending fields, respectively, for the implied MOVE
            /// statements.
            /// </summary>
            public Expression Receiving;
            public Mode ReceivingMode;

            public Replacing(Expression receiving, Mode mode)
            {
                this.Receiving = receiving;
                this.ReceivingMode = mode;
            }
        }
    }
}
