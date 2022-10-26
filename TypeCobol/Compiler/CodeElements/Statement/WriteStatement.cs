namespace TypeCobol.Compiler.CodeElements
{
	/// <summary>
	/// p449:
	/// The WRITE statement releases a logical record to an output or input/output file.
	/// When the WRITE statement is executed:
	/// * The associated sequential file must be open in OUTPUT or EXTEND mode.
	/// * The associated indexed or relative file must be open in OUTPUT, I-O, or EXTEND mode.
	/// </summary>
	public class WriteStatement : StatementElement
	{
		public WriteStatement() : base(CodeElementType.WriteStatement, StatementType.WriteStatement) { }

		/// <summary>
		/// p450:
		/// Must be defined in a DATA DIVISION FD entry. record-name-1 can be
		/// qualified. It must not be associated with a sort or merge file.
		/// For relative files, the number of character positions in the record being
		/// written can be different from the number of character positions in the
		///record being replaced.
		/// </summary>
		public SymbolReference RecordName { get; set; }

		/// <summary>
		/// p450:
		/// FROM phrase
		/// The result of the execution of the WRITE statement with the FROM
		/// identifier-1 phrase is equivalent to the execution of the following statements
		/// in the order specified:
		/// MOVE identifier-1 TO record-name-1.
		/// WRITE record-name-1.
		/// The MOVE is performed according to the rules for a MOVE statement
		/// without the CORRESPONDING phrase.
		///
		/// identifier-1 can reference any of the following items:
		/// * A data item defined in the WORKING-STORAGE SECTION, the
		/// LOCAL-STORAGE SECTION, or the LINKAGE SECTION
		/// * A record description for another previously opened file
		/// * An alphanumeric function
		/// * A national function
		/// identifier-1 must be a valid sending item for a MOVE statement with
		/// record-name-1 as the receiving item.
		/// identifier-1 and record-name-1 must not refer to the same storage area.
		/// After the WRITE statement is executed, the information is still available in
		/// identifier-1. (See “INTO and FROM phrases” on page 291 under "Common
		/// processing facilities".)
		/// identifier-2 Must be an integer data item.
		/// </summary>
		public Variable FromSendingField { get; set; }

		/// <summary>
		/// p451:
		/// When BEFORE ADVANCING is specified, the line is printed before the page is advanced.
		/// </summary>
		public SyntaxProperty<bool> WriteBeforeAdvancing { get; set; }

		/// <summary>
		/// p451:
		/// When AFTER ADVANCING is specified, the line is advanced before the page is printed.
		/// </summary>
		public SyntaxProperty<bool> WriteAfterAdvancing { get; set; }

		/// <summary>
		/// p451:
		/// When the ADVANCING phrase is specified, the following rules apply:
		/// (...)
		/// 3. When identifier-2 is specified, the page is advanced the number of lines equal to
		/// the current value in identifier-2. identifier-2 must name an elementary integer
		/// data item.
		/// 4. When integer is specified, the page is advanced the number of lines equal to
		/// the value of integer.
		/// 5. Integer or the value in identifier-2 can be zero.
		///
		/// p452:
		/// LINAGE-COUNTER rules
		/// If the LINAGE clause is specified for this file, the associated LINAGE-COUNTER
		/// special register is modified during the execution of the WRITE statement,
		/// according to the following rules:
		/// 1. If ADVANCING PAGE is specified, LINAGE-COUNTER is reset to 1.
		/// 2. If ADVANCING identifier-2 or integer is specified, LINAGE-COUNTER is
		/// increased by the value in identifier-2 or integer.
		/// 3. If the ADVANCING phrase is omitted, LINAGE-COUNTER is increased by 1.
		/// 4. When the device is repositioned to the first available line of a new page,
		/// LINAGE-COUNTER is reset to 1.
		///
		/// (...)
        /// 7. When mnemonic-name is specified, a skip to channels 1 through 12, or space
        /// suppression, takes place. mnemonic-name must be equated with
        /// environment-name-1 in the SPECIAL-NAMES paragraph.
        /// The mnemonic-name phrase can also be specified for stacker selection with a
        /// card punch file. When using stacker selection, WRITE AFTER ADVANCING
        /// must be used.
		/// </summary>
		public IntegerVariable ByNumberOfLinesOrByMnemonicForEnvironmentName { get; set; }

        /// <summary>
		/// p451:
		/// When the ADVANCING phrase is specified, the following rules apply:
		/// (...)
		/// 6. When PAGE is specified, the record is printed on the logical page BEFORE or
		/// AFTER (depending on the phrase used) the device is positioned to the next
		/// logical page. If PAGE has no meaning for the device used, then BEFORE or
		/// AFTER (depending on the phrase specified) ADVANCING 1 LINE is provided.
		/// If the FD entry contains a LINAGE clause, the repositioning is to the first
		/// printable line of the next page, as specified in that clause. If the LINAGE clause
		/// is omitted, the repositioning is to line 1 of the next succeeding page.
		/// </summary>
		public SyntaxProperty<bool> ByLogicalPage { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, RecordName, FromSendingField,
                   WriteBeforeAdvancing, WriteAfterAdvancing, ByNumberOfLinesOrByMnemonicForEnvironmentName, ByLogicalPage);
        }
    }
}
