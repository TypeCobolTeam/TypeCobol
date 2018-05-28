using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements.Expressions;



/// <summary>For statements using items for receiving data.</summary>
public interface VariableWriter {
	/// <summary>Keys: WHERE it is written. Values: WHAT is written.</summary>
	IDictionary<StorageArea,object> VariablesWritten { get; }
	/// <summary>Are unsafe writes allowed?</summary>
	bool IsUnsafe { get; }
}

/// <summary>For statements calling functions.</summary>
public interface FunctionCaller {
	FunctionCall FunctionCall { get; }
    FunctionDeclaration FunctionDeclaration { get; set; }
    }



}
