namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;



/// <summary>For statements using items for sending data.</summary>
public interface Sending {
	IList<string> SendingItems { get; }
}

/// <summary>For statements using items for receiving data.</summary>
public interface Receiving {
	IList<string> ReceivingItems { get; }
}



}
