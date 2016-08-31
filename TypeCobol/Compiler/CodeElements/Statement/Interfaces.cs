namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements.Expressions;



/// <summary>For statements using items for sending data.</summary>
public interface VariableUser {
	IList<QualifiedName> Variables { get; }
}

/// <summary>For statements using items for sending data.</summary>
public interface Sending {
	IList<QualifiedName> SendingItems { get; }
}

/// <summary>For statements using items for receiving data.</summary>
public interface Receiving {
	IList<QualifiedName> ReceivingItems { get; }
}



}
