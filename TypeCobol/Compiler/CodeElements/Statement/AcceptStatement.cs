using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p294:
    /// The ACCEPT statement transfers data or system date-related information into the
    /// data area referenced by the specified identifier. There is no editing or error
    /// checking of the incoming data.
    /// </summary>
    public abstract class AcceptStatement : StatementElement
    {
        protected AcceptStatement(StatementType statementType) : base(CodeElementType.AcceptStatement, statementType) { }

        /// <summary>
        /// p294:
        /// identifier-1
        /// The receiving area. Can be:
        /// * An alphanumeric group item
        /// * A national group item
        /// * An elementary data item of usage DISPLAY, DISPLAY-1, or NATIONAL
        /// A national group item is processed an elementary data item of category national.
        ///
        /// p296:
        /// identifier-2
        /// The receiving area. Can be:
        /// * An alphanumeric group item
        /// * A national group item
        /// * An elementary data item of one of the following categories:
        /// – alphanumeric
        /// – alphanumeric-edited
        /// – numeric-edited (with usage DISPLAY or NATIONAL)
        /// – national
        /// – national-edited
        /// – numeric
        /// – internal floating-point
        /// – external floating-point (with usage DISPLAY or NATIONAL)
        /// A national group item is processed an an elementary data item of category national.
        /// </summary>
        public ReceivingStorageArea ReceivingStorageArea { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
	        return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, ReceivingStorageArea);
	    }
    }

    /// <summary>
    /// p294:
    /// Format 1: Data transfer
    /// Format 1 transfers data from an input source into the data item referenced by
    /// identifier-1 (the receiving area). When the FROM phrase is omitted, the system
    /// input device is assumed.
    /// Format 1 is useful for exceptional situations in a program when operator
    /// intervention (to supply a given message, code, or exception indicator) is required.
    /// The operator must of course be supplied with the appropriate messages with
    /// which to reply.
    /// </summary>
    public class AcceptFromInputDeviceStatement : AcceptStatement
    {
        public AcceptFromInputDeviceStatement() : base(StatementType.AcceptFromInputDeviceStatement)
        { }

        /// <summary>
        /// p294:
        /// mnemonic-name-1
        /// Specifies the input device. mnemonic-name-1 must be associated in the
        /// SPECIAL-NAMES paragraph with an environment-name.
        /// See “SPECIAL-NAMES paragraph” on page 112.
        ///
        /// p295:
        /// environment-name
        /// Identifies the source of input data. An environment-name from the names
        /// given in Table 5 on page 114 can be specified.
        ///
        /// If the device is the same as that used for READ statements for a LINE
        /// SEQUENTIAL file, results are unpredictable.
        /// </summary>
        public ExternalNameOrSymbolReference InputDevice { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, InputDevice);
        }
    }

    /// <summary>
    /// pp295-296
    /// Format 2: System date-related information transfer
    /// System information contained in the specified conceptual data items DATE, DATE
    /// YYYYMMDD, DAY, DAY YYYYDDD, DAY-OF-WEEK, or TIME, can be transferred
    /// into the data item referenced by identifier-2. The transfer must follow the rules for
    /// the MOVE statement without the CORRESPONDING phrase.
    /// For more information, see “MOVE statement” on page 369.
    ///
    /// Format 2 accesses the current date in two formats: the day of the week or the time
    /// of day as carried by the system (which can be useful in identifying when a
    /// particular run of an object program was executed). You can also use format 2 to
    /// supply the date in headings and footings.
    /// The current date and time can also be accessed with the intrinsic function
    /// CURRENT-DATE, which also supports four-digit year values and provides
    /// additional information (see “CURRENT-DATE” on page 490). 
    /// </summary>
    public class AcceptFromSystemDateStatement : AcceptStatement
    { 
        public AcceptFromSystemDateStatement() : base(StatementType.AcceptFromInputDeviceStatement)
        { }

        /// <summary>
        /// The conceptual data items DATE, DATE YYYYMMDD, DAY, DAY YYYYDDD,
        /// DAY-OF-WEEK, and TIME implicitly have USAGE DISPLAY. Because these are
        /// conceptual data items, they cannot be described in the COBOL program.
        ///
        /// The content of the conceptual data items is moved to the receiving area using the
        /// rules of the MOVE statement. If the receiving area is of usage NATIONAL, the
        /// data is converted to national character representation.
        /// </summary>
        public SyntaxProperty<SystemDateFormat> SystemDateFormat { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, SystemDateFormat);
        }
    }

    public enum SystemDateFormat
    {
        UNKNOWN,
        /// <summary>
        /// p297:
        /// DATE
        /// Has the implicit PICTURE 9(6).
        /// The sequence of data elements (from left to right) is:
        /// Two digits for the year
        /// Two digits for the month
        /// Two digits for the day
        /// Thus 27 April 2003 is expressed as 030427.
        /// </summary>
        DATE_YYMMDD,
        /// <summary>
        /// p297:
        /// DATE YYYYMMDD
        /// Has the implicit PICTURE 9(8).
        /// The sequence of data elements (from left to right) is:
        /// Four digits for the year
        /// Two digits for the month
        /// Two digits for the day
        /// Thus 27 April 2003 is expressed as 20030427.
        /// </summary>
        DATE_YYYYMMDD,
        /// <summary>
        /// p297:
        /// DAY
        /// Has the implicit PICTURE 9(5).
        /// The sequence of data elements (from left to right) is:
        /// Two digits for the year
        /// Three digits for the day
        /// Thus 27 April 2003 is expressed as 03117.
        /// </summary>
        DAY_YYDDD,
        /// <summary>
        /// p297:
        /// DAY YYYYDDD
        /// Has the implicit PICTURE 9(7).
        /// The sequence of data elements (from left to right) is:
        /// Four digits for the year
        /// Three digits for the day
        /// Thus 27 April 2003 is expressed as 2003117.
        /// </summary>
        DAY_YYYYDDD,
        /// <summary>
        /// p297:
        /// DAY-OF-WEEK
        /// Has the implicit PICTURE 9(1).
        /// The single data element represents the day of the week according to the
        /// following values:
        /// 1 represents Monday
        /// 2 represents Tuesday
        /// 3 represents Wednesday
        /// 4 represents Thursday
        /// 5 represents Friday
        /// 6 represents Saturday
        /// 7 represents Sunday
        /// </summary>
        DAY_OF_WEEK,
        /// <summary>
        /// p297:
        /// TIME
        /// Has the implicit PICTURE 9(8).
        /// The sequence of data elements (from left to right) is:
        /// Two digits for hour of day
        /// Two digits for minute of hour
        /// Two digits for second of minute
        /// Two digits for hundredths of second
        /// Thus 2:41 PM is expressed as 14410000.
        /// </summary>
        TIME,
    }
}
