namespace TypeCobol.Codegen.Skeletons {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Tools;


/// <summary>
/// Condtition to be verified on a Node 
/// </summary>
public interface Condition {
    /// <summary>
    /// Verify the condition on the given node.
    /// </summary>
    /// <param name="node">The node to be verified.</param>
    /// <returns>true if the condition is verified, false otherwise</returns>
	bool Verify(Node node);
}

/// <summary>
/// Condition on an attributed Node.
/// </summary>
public class ConditionOnNode: Condition {
    /// <summary>
    /// The reference node
    /// </summary>
	public System.Type Node { get; internal set; }
    /// <summary>
    /// Attributes (Key, Value)
    /// </summary>
	public Dictionary<string,string> Attributes { get; internal set; }

    /// <summary>
    /// Verify the condition on the given node.
    /// </summary>
    /// <param name="node">The node to be verified.</param>
    /// <returns>true if the condition is verified, false otherwise</returns>
	public bool Verify(Node node) {
		if (Node != null && !Reflection.IsTypeOf(node.GetType(), Node)) return false;
		foreach(var x in Attributes) {
			var property = node[x.Key];
			if ("+".Equals(x.Value)) {
				var values = property as System.Collections.ICollection;
				return values != null && values.Count > 0;
			} else
			if ("*".Equals(x.Value)) {
				return ToString(property) != null;
			} else
			if (!x.Value.Equals(ToString(property), System.StringComparison.InvariantCultureIgnoreCase)) {
				return false;
			}
		}
		return true;
	}
	private string ToString(object o) {
		return o == null? null : o.ToString();
	}

	public override string ToString() {
		var str = new System.Text.StringBuilder();
		str.Append("node.Type=").Append(Node);
		foreach(var x in Attributes) str.Append(' ').Append(x.Key).Append(':').Append(x.Value);
		return str.ToString();
	}
}

}

