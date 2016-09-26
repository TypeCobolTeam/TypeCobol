namespace TypeCobol.Compiler.CodeElements {
/// <summary>
/// p438:
/// The SUBTRACT statement subtracts one numeric item, or the sum of two or more
/// numeric items, from one or more numeric items, and stores the result.
/// </summary>
public abstract class SubtractStatement: StatementElement {
	public SubtractStatement(StatementType statementType): base(CodeElementType.SubtractStatement, statementType) { }
}

/// <summary>
/// p438: SUBTRACT statement
/// The SUBTRACT statement subtracts one numeric item, or the sum of two or more
/// numeric items, from one or more numeric items, and stores the result.
/// </summary>
public class SubtractSimpleStatement: SubtractStatement {
	public SubtractSimpleStatement(): base(StatementType.SubtractSimpleStatement) { }

	public NumericVariable[] VariablesTogether { get; set; }
	public RoundedResult[] SendingAndReceivingStorageAreas { get; set; }
}

/// <summary>
/// p439: Format 2: SUBTRACT statement with GIVING phrase
/// All identifiers or literals preceding the keyword FROM are added together and
/// their sum is subtracted from identifier-2 or literal-2. The result of the subtraction is
/// stored as the new value of each data item referenced by identifier-3.
/// </summary>
public class SubtractGivingStatement: SubtractStatement {
	public SubtractGivingStatement(): base(StatementType.SubtractGivingStatement) { }

	public NumericVariable[] VariablesTogether { get; set; }
	public NumericVariable Operand { get; set; }
	public RoundedResult[] ReceivingStorageAreas { get; set; }
}

/// <summary>
/// p439: Format 3: SUBTRACT statement with CORRESPONDING phrase
/// Elementary data items within identifier-1 are subtracted from, and the results are
/// stored in, the corresponding elementary data items within identifier-2.
/// </summary>
public class SubtractCorrespondingStatement: SubtractStatement {
	public SubtractCorrespondingStatement(): base(StatementType.SubtractCorrespondingStatement) { }

	public StorageArea GroupItem { get; set; }
	public StorageArea SendingAndReceivingGroupItem { get; set; }
	public SyntaxProperty<bool> IsRounded { get; set; }
}

}
