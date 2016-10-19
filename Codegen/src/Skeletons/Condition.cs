namespace TypeCobol.Codegen.Skeletons {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Tools;



public interface Condition {
	bool Verify(Node node);
}
public class ConditionOnNode: Condition {
	public System.Type Node { get; internal set; }
	public Dictionary<string,string> Attributes { get; internal set; }

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
