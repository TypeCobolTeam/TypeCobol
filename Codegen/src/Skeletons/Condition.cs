using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Tools;

namespace TypeCobol.Codegen.Skeletons {

	public interface Condition {
		bool Verify(Node node);
	}
	public class ConditionOnNode: Condition {
		public System.Type Node { get; internal set; }
		public Dictionary<string,string> Attributes { get; internal set; }

		public bool Verify(Node node) {
			var ce = node.CodeElement;
			if (ce == null) return false;
			if (Node != null && !Reflection.IsTypeOf(ce.GetType(), Node)) return false;
			foreach(var x in Attributes) {
				if ("*".Equals(x.Value)) {
					if (node[x.Key] == null) return false;
				} else {
					if (!x.Value.Equals(node[x.Key], System.StringComparison.InvariantCultureIgnoreCase)) return false;
				}
			}
			return true;
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			str.Append("node.Type=").Append(Node);
			foreach(var x in Attributes) str.Append(' ').Append(x.Key).Append(':').Append(x.Value);
			return str.ToString();
		}
	}

}
