namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.CodeElements.Functions;



/// <summary>For statements using items for sending data.</summary>
public interface VariableUser {
	IList<QualifiedName> Variables { get; }
}

/// <summary>For statements using items for sending data.</summary>
public interface Sending {
	IList<QualifiedName> SendingItems { get; }
}

/// <summary>For statements using items for receiving data.</summary>
public interface VariableWriter: VariableUser {
	/// <summary>Keys: WHERE it is written. Values: WHAT is written.</summary>
	IDictionary<QualifiedName,object> VariablesWritten { get; }
	/// <summary>Are unsafe writes allowed?</summary>
	bool IsUnsafe { get; }
}

/// <summary>For statements calling functions.</summary>
public interface FunctionCaller {
	IList<FunctionCall> FunctionCalls { get; }
}



}
