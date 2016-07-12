using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class FunctionDeclaration: Node, Generated {

		QualifiedName ProgramName = null;

		public FunctionDeclaration(Node node) {
			var header = (FunctionDeclarationHeader)node.CodeElement;
			ProgramName = header.Name;
			FunctionDeclarationProfile profile = null;
			foreach(var child in node.Children) {
				var ce = child.CodeElement;
				System.Console.WriteLine("::"+(ce!=null?ce.GetType().Name:"?"));
				if (child.CodeElement is FunctionDeclarationProfile) {
					profile = (FunctionDeclarationProfile)child.CodeElement;
					var pdiv = new ProcedureDivision(profile);
					foreach(var sentence in child.Children)
						pdiv.Children.Add(sentence);
					Children.Add(pdiv);
				} else
				if (child.CodeElement is FunctionDeclarationEnd) {
					Children.Add(new TypeCobol.Codegen.Nodes.ProgramEnd(ProgramName));
				} else {
					Children.Add(child);
				}
			}
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>();
					_cache.Add(new TextLineSnapshot(-1, "PROGRAM-ID. "+ProgramName.Head+'.', null));
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return false; } }
	}
}
