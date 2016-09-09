namespace TypeCobol.Compiler.CodeElements {

	using JetBrains.Annotations;
	using System.Collections.Generic;
	using System.Text;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Scanner;

/// <summary>
/// Base class for  7 differents forms of set statement:
///
/// Format 1: SET for basic table handling
/// Format 2: SET for adjusting indexes
/// Format 3: SET for external switches
/// Format 4: SET for condition-names (to true)
/// Format 5: SET for USAGE IS POINTER data items
/// Format 6: SET for procedure-pointer and function-pointer data items
/// Format 7: SET for USAGE OBJECT REFERENCE data items
///
/// Format 1, 5, 6, 7 can be ambiguous
/// </summary>
public abstract class SetStatement: StatementElement, Sending,VariableWriter {
	public SetStatement(StatementType statementType): base(CodeElementType.SetStatement, statementType) { }

	public bool IsUnsafe { get { return true; } }

	protected IDictionary<QualifiedName, object> _variablesWritten;
	protected IList<QualifiedName> _sendingItems = null;
	protected List<QualifiedName> _variables = null;


	public virtual IList<QualifiedName> Variables { get { return new List<QualifiedName>(); } }
	public virtual IList<QualifiedName> SendingItems { get { return new List<QualifiedName>(); } }
	public virtual IDictionary<QualifiedName,object> VariablesWritten { get { return new Dictionary<QualifiedName,object>(); } }
}

/// <summary>
/// Format 1: SET for basic table handling
/// set index-name or identifier(numeric integer item) TO index-name or identifier(numeric integer item) or positive integer
///
/// Format 5: SET for USAGE IS POINTER data items
/// set (address of)? identifier(pointer) TO (address of)? identifier | NULL
///
/// Format 6: SET for procedure-pointer and function-pointer data items
/// set procedureOrFunctionPointer to procedureOrFunctionPointer | (ENTRY identifier|literal) | NULL | pointer
///
/// Format 7: SET for USAGE OBJECT REFERENCE data items
/// set objectReference to objectReference | NULL | SELF
/// </summary>
public class SetStatementForAssignment: SetStatement {
	public SetStatementForAssignment(): base(StatementType.SetStatementForAssignment) { }

	/// <summary>index-name, identifier(numeric integer item), pointer, procedure-pointer, function-pointer, object reference id</summary>
	public ReceivingStorageArea[] ReceivingStorageAreas { get; set; }

	/// <summary>
	/// index-name, identifier, positive integer, address of, null, nulls, entry identifier|literal, object reference id, pointer,
	/// procedure-pointer, function-pointer,
	/// </summary>
	public SetSendingVariable SendingVariable { get; set; }

	public override string ToString() {
		if (ReceivingStorageAreas == null && SendingVariable == null) return base.ToString();
		var str = new StringBuilder("Set ");
		if (ReceivingStorageAreas != null) {
			foreach (var receivingField in ReceivingStorageAreas) {
				str.Append(' ');
				str.Append(receivingField);
			}
		}
		str.Append(" TO ");
		if (SendingVariable != null) str.AppendLine(SendingVariable.ToString());
		return str.ToString();
	}



	public override IList<QualifiedName> Variables {
		[NotNull]
		get {
			if (_variables != null) return _variables;

			_variables = new List<QualifiedName>();
			var sending = SendingItem as QualifiedName;
			if (sending != null) _variables.Add(sending);
			_variables.AddRange(VariablesWritten.Keys);
			return _variables;
		}
	}

	public override IDictionary<QualifiedName,object> VariablesWritten {
		[NotNull]
		get {
			if (_variablesWritten != null) return _variablesWritten;

			_variablesWritten = new Dictionary<QualifiedName,object>();
			if (ReceivingStorageAreas == null) return _variablesWritten;

			foreach(var item in ReceivingStorageAreas) {
				var name = ((Named)item.StorageArea).QualifiedName;
				if (_variablesWritten.ContainsKey(name))
					if (item.StorageArea is Subscripted) continue; // same variable with (presumably) different subscript
					else throw new System.ArgumentException(name+" already written, but not subscripted?");
				else _variablesWritten.Add(name, SendingItem);
			}
			return _variablesWritten;
		}
	}

	private object SendingItem {
		[CanBeNull]
		get {
			if (SendingVariable == null) return null;
			return SendingVariable.Value;
		}
	}

	public override IList<QualifiedName> SendingItems {
		[NotNull]
		get {
			if (_sendingItems != null) return _sendingItems;

			_sendingItems = new List<QualifiedName>();
			if (SendingVariable != null) {
				var name = SendingVariable.Value as QualifiedName;
				if (name != null) _sendingItems.Add(name);
			}
			return _sendingItems;
		}
	}
}

/// <summary>Sending field for SET statement for assignation</summary>
public class SetSendingVariable {
	/// <summary>integerVariableOrIndex1 - identifier can also be an index name	(Format 1 + 5)</summary>
	public IntegerVariable IntegerVariableOrIndex { get; set; }
	/// <summary>nullPointerValue - pointer data item (Format 5 + 6 + 7)</summary>
	public NullPointerValue NullPointerValue { get; set; }
	/// <summary>procedure pointer, function pointer or a pointer data item (Format 6 (+NULL|NULLS))</summary>
	public SymbolReferenceVariable ProgramNameOrProgramEntryVariable { get; set; }

	/// <summary>object reference id (Format 7 (+NULL))</summary>
	public Token SelfObjectIdentifier { get; set; }

	public override string ToString() {
		if(IntegerVariableOrIndex != null) return IntegerVariableOrIndex.ToString();
		if(NullPointerValue != null) return NullPointerValue.ToString();
		if(ProgramNameOrProgramEntryVariable != null) return ProgramNameOrProgramEntryVariable.ToString();
		if(SelfObjectIdentifier != null) return SelfObjectIdentifier.ToString();
		return string.Empty;
	}

	public object Value {
		get {
			if(IntegerVariableOrIndex != null)
				if (IntegerVariableOrIndex.Value != null) return IntegerVariableOrIndex.Value.Value;
				else return ((Named)IntegerVariableOrIndex.StorageArea).Name;
			if(NullPointerValue != null) return NullPointerValue.ToString();
			if(ProgramNameOrProgramEntryVariable != null) {
				var program = ProgramNameOrProgramEntryVariable.StorageArea as Named;
				if (program != null) return program.QualifiedName;
			}
			if(SelfObjectIdentifier != null) return SelfObjectIdentifier.ToString();
			return null;
		}
	}
}

/// <summary>
/// Format 2: SET for adjusting indexes
/// set index-name UP|DOWN BY identifier(numeric integer item) or positive integer
/// </summary>
public class SetStatementForIndexes: SetStatement {
	public SetStatementForIndexes() : base(StatementType.SetStatementForIndexes) { }

	/// <summary>index-name</summary>
	public ReceivingStorageArea[] ReceivingIndexes { get; set; }

	/// <summary>UP | DOWN</summary>
	public SyntaxProperty<IndexIncrementDirection> IncrementDirection { get; set; }

	/// <summary>identifier(numeric integer item) or positive integer</summary>
	public IntegerVariable SendingVariable{ get; set; }

	public override string ToString() {
		var str = new StringBuilder("Set ");
		if (ReceivingIndexes != null) {
			foreach (var receivingIndex in ReceivingIndexes) {
				str.Append(' ');
				str.Append(receivingIndex);
			}
		}
		if (IncrementDirection != null) {
			if (IncrementDirection.Value == IndexIncrementDirection.Up) str.Append(" UP BY ");
			else if (IncrementDirection.Value == IndexIncrementDirection.Down) str.Append(" DOWN BY ");
		} else str.Append(" ");
		if (SendingVariable != null) str.Append(SendingVariable);
		str.AppendLine(" ");
		return str.ToString();
	}
}

public enum IndexIncrementDirection {
	Up,
	Down
}

/// <summary> Format 3: SET for external switches: set externalSwitches to ON|OFF</summary>
internal class SetStatementForSwitches: SetStatement {
	public SetStatementForSwitches(): base(StatementType.SetStatementForSwitches) { }

	/// <summary>mnemonicForUPSISwitchNameReference+ TO (ON | OFF)</summary>
	public IList<SetUPSISwitchInstruction> SetUPSISwitchInstructions { get; set; }

	public override string ToString() {
		var str = new StringBuilder("SET ");
		foreach (var setSwitchPositionInstruction in SetUPSISwitchInstructions) {
			if (setSwitchPositionInstruction.MnemonicForUPSISwitchName != null) {
				str.Append(' ');
				str.Append(setSwitchPositionInstruction.MnemonicForUPSISwitchName);
			}
			if (setSwitchPositionInstruction.SwitchPosition != null) {
				if (setSwitchPositionInstruction.SwitchPosition.Value == UPSISwitchPosition.On) str.AppendLine(" TO ON");
				else if (setSwitchPositionInstruction.SwitchPosition.Value == UPSISwitchPosition.Off) str.AppendLine(" TO OFF");
			}
			else str.AppendLine("");
		}
		return str.ToString();
	}
}

public class SetUPSISwitchInstruction {
	public SymbolReference MnemonicForUPSISwitchName { get; set; }
	public SyntaxProperty<UPSISwitchPosition> SwitchPosition { get; set; }
}

public enum UPSISwitchPosition {
	On,
	Off
}

/// <summary>
/// Format 4: SET for condition-names (to true)
/// set condition-names to true
///          List<Identifier> ConditionIdentifiers //identifier
/// </summary>
internal class SetStatementForConditions: SetStatement {
	public SetStatementForConditions(): base(StatementType.SetStatementForConditions) { }

	/// <summary>identifier (condition-name)</summary>
	public DataOrConditionStorageArea[] Conditions { get; set; }

	/// <summary>Always TRUE in COBOL, can be either TRUE or FALSE in TypeCobol.</summary>
	public BooleanValue SendingValue { get; set; }

	public override string ToString() {
		var str = new StringBuilder(base.ToString());
		foreach (var condition in Conditions) {
			str.Append(' ');
			str.Append(condition);
		}
		str.Append(" TO ").Append(SendingValue.Value.ToString()).AppendLine();
		return str.ToString();
	}

	public override IList<QualifiedName> Variables { get { return new List<QualifiedName>(this.VariablesWritten.Keys); } }

	public override IList<QualifiedName> SendingItems {
		get { 
			var items = new List<QualifiedName>();
			items.Add(new URI(SendingValue.Value.ToString()));
			return items;
		}
	}
	private bool? SendingItem {
		get {
			if (SendingValue == null) return null;
			return SendingValue.Value;
		}
	}

	public override IDictionary<QualifiedName,object> VariablesWritten {
		get {
			var items = new Dictionary<QualifiedName,object>();
			foreach(var item in Conditions)
				items.Add(item.QualifiedName, SendingItem);
			return items;
		}
	}
}

}
