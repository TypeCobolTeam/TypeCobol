using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class ProcedureDivision: Node, Generated {

		public IList<InputParameter> UsingParameters { get; private set; }
		public DataName ReturningParameter { get; private set; }

		public ProcedureDivision(FunctionDeclarationProfile profile): base(null) {
			UsingParameters = new List<InputParameter>();
			foreach(var parameter in profile.InputParameters)
				UsingParameters.Add(parameter);
			int outputs = profile.OutputParameters.Count;
			if (outputs > 0)
				ReturningParameter = profile.OutputParameters[0];
			if (outputs > 1)
				System.Console.WriteLine("Cannot properly convert "+outputs+" procedure outputs to standard PROCEDURE DIVISION");
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>();
					_cache.Add(new TextLineSnapshot(-1, "  PROCEDURE DIVISION", null));
					int c = 0;
					foreach(var parameter in UsingParameters) {
						string strmode = "BY REFERENCE ";
						if (parameter.ReceivingMode.Value == ReceivingMode.ByValue) strmode = "BY VALUE ";
						string strusing = c==0? "      USING ":"            ";
						_cache.Add(new TextLineSnapshot(-1, strusing+strmode+parameter.DataName, null));
						c++;
					}
					if (ReturningParameter != null)
						_cache.Add(new TextLineSnapshot(-1, "      RETURNING "+ReturningParameter, null));
					_cache.Add(new TextLineSnapshot(-1, "  .", null));
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return false; } }
	}
}
