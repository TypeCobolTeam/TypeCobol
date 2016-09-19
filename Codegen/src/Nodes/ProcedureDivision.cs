﻿using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class ProcedureDivision: Node, Generated {

		public IList<InputParameter> UsingParameters { get; private set; }
		public Symbol ReturningParameter { get; private set; }

		public ProcedureDivision(FunctionDeclarationProfile profile): base(null) {
			UsingParameters = new List<InputParameter>();
			// TCRFUN_CODEGEN_PARAMETERS_ORDER
			foreach(var parameter in profile.Profile.InputParameters)  UsingParameters.Add(new GeneratedParameter(parameter.Name));
			foreach(var parameter in profile.Profile.InoutParameters)  UsingParameters.Add(new GeneratedParameter(parameter.Name));
			foreach(var parameter in profile.Profile.OutputParameters) UsingParameters.Add(new GeneratedParameter(parameter.Name));
			// TCRFUN_CODEGEN_RETURNING_PARAMETER
			ReturningParameter = profile.Profile.ReturningParameter != null ? profile.Profile.ReturningParameter.Name : null;
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>();
					_cache.Add(new TextLineSnapshot(-1, "  PROCEDURE DIVISION", null));
					int c = 0;
					var done = new List<string>();
					foreach(var parameter in UsingParameters) {
						if (done.Contains(parameter.Symbol.Name)) continue;
						else done.Add(parameter.Symbol.Name);
						string strmode = "BY REFERENCE ";
						if (parameter.ReceivingMode.Value == ReceivingMode.ByValue) strmode = "BY VALUE ";
						string strusing = c==0? "      USING ":"            ";
						_cache.Add(new TextLineSnapshot(-1, strusing+strmode+parameter.Symbol, null));
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

	public class GeneratedParameter: InputParameter {
		public GeneratedParameter(Symbol name): base(name, null) {
			var mode = TypeCobol.Compiler.CodeElements.ReceivingMode.ByReference;
			this.ReceivingMode = new SyntaxProperty<ReceivingMode>(mode, null);
		}
	}
}
