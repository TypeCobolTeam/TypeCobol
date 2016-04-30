using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Tools;

namespace TypeCobol.Codegen.Skeletons {

	public interface Condition {
		bool Verify(Node node);
	}
	public class ConditionOnNode: Condition {
		public System.Type Node { get; internal set; }
		public string Attribute { get; internal set; }

		public bool Verify(Node node) {
			var ce = node.CodeElement;
			if (ce == null) return false;
			if (Node != null) {
				bool okay = Reflection.IsTypeOf(ce.GetType(), Node);
				if (Attribute == null) return okay;
				else return okay && node[Attribute] != null;
			} else 
			if (Attribute != null) return node[Attribute] != null;
			return false;
		}

		public override string ToString() {
			return "Node.Type="+Node+(Attribute!=null?(" clause="+Attribute):"");
		}
	}

}
