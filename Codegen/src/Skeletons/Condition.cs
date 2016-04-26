using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Tools;

namespace TypeCobol.Codegen.Skeletons {

	public interface Condition {
		bool Verify(Node node, SymbolTable table);
	}
	public class ConditionOnNode: Condition {
		public System.Type Node { get; internal set; }
		public string Clause { get; internal set; }

		public bool Verify(Node node, SymbolTable table) {
			var ce = node.CodeElement;
			if (ce == null) return false;
			if (Node != null) {
				bool okay = Reflection.IsTypeOf(ce.GetType(), Node);
				if (Clause == null) return okay;
				else return okay && validate(Clause, node, table);
			} else 
			if (Clause != null) return validate(Clause, node, table);
			return false;
		}

		private bool validate(string clause, Node node, SymbolTable table) {
			var data = node.CodeElement as DataDescriptionEntry;
			if (data == null) return false;
			if (clause.Equals("TYPEDEF") && data.IsTypeDefinitionPart) return true;
			if (clause.Equals("TYPE") && !data.IsTypeDefinition
			 && table.IsCustomType(data.DataType)) return true;
			return false;
		}

		public override string ToString() {
			return "Node.Type="+Node+(Clause!=null?(" clause="+Clause):"");
		}
	}

}
