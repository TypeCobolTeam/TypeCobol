﻿namespace TypeCobol.Compiler.CodeElements {

	using JetBrains.Annotations;
	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements.Expressions;

/// <summary>p369: The MOVE statement transfers data from one area of storage to one or more other areas.</summary>
public abstract class MoveStatement : StatementElement, VariableWriter,FunctionCaller {
	public MoveStatement(StatementType statementType) : base(CodeElementType.MoveStatement, statementType) { }
// [TYPECOBOL]
	public SyntaxProperty<bool> Unsafe { get; set; }
	public bool IsUnsafe { get { return Unsafe != null && Unsafe.Value; } }
// [/TYPECOBOL]

	protected IDictionary<QualifiedName,object> variables;
	protected List<Functions.FunctionCall> _functions = null;

	public abstract IDictionary<QualifiedName,object> Variables { get; }
	public virtual  IDictionary<QualifiedName,object> VariablesWritten {
		[NotNull]
		get {
			var written = new Dictionary<QualifiedName,object>();
			foreach(var v in Variables) if (v.Value != null) written.Add(v.Key, v.Value);
			return written;
		}
	}
	public abstract IList<Functions.FunctionCall> FunctionCalls { get; }
}

/// <summary>
///  p369: Format 1: MOVE statement
///
/// When format 1 (no CORRESPONDING) is specified:
/// * All identifiers can reference alphanumeric group items, national group items, or
/// elementary items.
/// * When one of identifier-1 or identifier-2 references a national group item and the
/// other operand references an alphanumeric group item, the national group is
/// processed as a group item; in all other cases, the national group item is
/// processed as an elementary data item of category national.
/// * The data in the sending area is moved into the data item referenced by each
/// identifier-2 in the order in which the identifier-2 data items are specified in the
/// MOVE statement. See “Elementary moves” on page 370 and “Group moves” on page 374 below.
/// </summary>
public class MoveSimpleStatement : MoveStatement {
	public MoveSimpleStatement(Variable sendingVariable, ReceivingStorageArea[] receivingStorageAreas, BooleanValue sendingBoolean)
			: base(StatementType.MoveSimpleStatement) {
		SendingVariable = sendingVariable;
		SendingBoolean = sendingBoolean;
		ReceivingStorageAreas = receivingStorageAreas;
	}

	/// <summary>The sending area.</summary>
	public Variable SendingVariable { get; private set; }
// [TYPECOBOL]
	public BooleanValue SendingBoolean { get; private set; }
// [/TYPECOBOL]
	/// <summary>The receiving areas. Must not reference an intrinsic function.</summary>
	public ReceivingStorageArea[] ReceivingStorageAreas { get; private set; }



	public override IDictionary<QualifiedName,object> Variables {
		[NotNull]
		get {
			if (variables != null) return variables;
			variables = new Dictionary<QualifiedName,object>();

			var sending = SendingItem as QualifiedName;
			if (sending != null) variables.Add(sending, null);

			foreach(var item in ReceivingStorageAreas) {
				var name = ((Named)item.StorageArea).QualifiedName;
				if (variables.ContainsKey(name))
					if (item.StorageArea is Subscripted) continue; // same variable with (presumably) different subscript
					else throw new System.ArgumentException(name+" already written, but not subscripted?");
				else variables.Add(name, SendingItem);
			}

			return variables;
		}
	}

	public IDictionary<QualifiedName,ICollection<List<SubscriptExpression>>> Subscripts {
		get {
			var subscripts = new Dictionary<QualifiedName,ICollection<List<SubscriptExpression>>>();
			if (SendingVariable != null) {
				var kv = GetSubscriptedVariable(SendingVariable.StorageArea);
				if (!kv.Equals(default(KeyValuePair<QualifiedName,List<SubscriptExpression>>))) {
					AddKeyValue<QualifiedName,SubscriptExpression>(subscripts, kv);
				}
			}
			foreach(var v in ReceivingStorageAreas) {
				var kv = GetSubscriptedVariable(v.StorageArea);
				if (!kv.Equals(default(KeyValuePair<QualifiedName,List<SubscriptExpression>>))) {
					AddKeyValue<QualifiedName,SubscriptExpression>(subscripts, kv);
				}
			}
			return subscripts;
		}
	}
	private KeyValuePair<QualifiedName,List<SubscriptExpression>> GetSubscriptedVariable(StorageArea variable) {
		var subscripted = variable as Subscripted;
		if (subscripted == null || subscripted.Subscripts.Count < 1) return default(KeyValuePair<QualifiedName,List<SubscriptExpression>>);
		var name = ((Named)variable).QualifiedName;
		return new KeyValuePair<QualifiedName,List<SubscriptExpression>>(name, subscripted.Subscripts);
	}
	private void AddKeyValue<K,V>(Dictionary<K,ICollection<List<V>>> map, KeyValuePair<K,List<V>> kv) {
		ICollection<List<V>> values = new List<List<V>>();
		try { values = map[kv.Key]; }
		catch(KeyNotFoundException) { }// values is already initialized as an empty list
		values.Add(kv.Value);
		map[kv.Key] = values;
	}

	private object SendingItem {
		[CanBeNull]
		get {
			if (SendingVariable != null) {
				if (SendingVariable.IsLiteral) {
					if (SendingVariable.NumericValue != null) return SendingVariable.NumericValue.Value;
					if (SendingVariable.AlphanumericValue != null) return SendingVariable.AlphanumericValue.Value;
					throw new System.NotSupportedException();
				}
				return SendingVariable.QualifiedName;
			}
			else if (SendingBoolean != null) return SendingBoolean.Value;
			else return null;
		}
	}

	public override IList<Functions.FunctionCall> FunctionCalls {
		[NotNull]
		get {
			if (_functions != null) return _functions;

			_functions = new List<Functions.FunctionCall>();
			IntrinsicFunctionCallResult sending = null;
			if (SendingVariable != null) sending = SendingVariable.StorageArea as IntrinsicFunctionCallResult;
			if (sending != null) _functions.Add(new Functions.FunctionCall(sending));
			return _functions;
		}
	}
}

/// <summary>
/// p369: Format 2: MOVE statement with CORRESPONDING phrase
///
/// When format 2 (with CORRESPONDING) is specified:
/// * Both identifiers must be group items.
/// * A national group item is processed as a group item (and not as an elementary
/// data item of category national).
/// * Selected items in identifier-1 are moved to identifier-2 according to the rules for
/// the “CORRESPONDING phrase” on page 281. The results are the same as if
/// each pair of CORRESPONDING identifiers were referenced in a separate MOVE
/// statement.
/// </summary>
public class MoveCorrespondingStatement : MoveStatement {
	public MoveCorrespondingStatement() : base(StatementType.MoveCorrespondingStatement) { }

	/// <summary>
	/// identifier-1
	/// The sending group item.
	/// </summary>
	public DataOrConditionStorageArea FromGroupItem { get; set; }

	/// <summary>
	/// identifier-2
	/// The receiving group item.
	/// </summary>
	public DataOrConditionStorageArea ToGroupItem { get; set; }



	public override IDictionary<QualifiedName,object> Variables {
		[NotNull]
		get {
			if (variables != null) return variables;

			variables = new Dictionary<QualifiedName,object>();
			if (FromGroupItem != null && FromGroupItem.QualifiedName != null)
				variables.Add(FromGroupItem.QualifiedName, null);
			if (  ToGroupItem != null &&   ToGroupItem.QualifiedName != null)
				variables.Add(  ToGroupItem.QualifiedName, FromGroupItem!=null? FromGroupItem.QualifiedName:null);
			return variables;
		}
	}

	public override IList<Functions.FunctionCall> FunctionCalls {
		[NotNull]
		get {
			if (_functions != null) return _functions;

			_functions = new List<Functions.FunctionCall>();
			return _functions;
		}
	}
}



}
