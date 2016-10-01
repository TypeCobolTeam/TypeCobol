namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Functions;
	using TypeCobol.Compiler.Text;

internal class ProcedureDivision: Compiler.Nodes.ProcedureDivision, Generated {

	public IList<InputParameter> UsingParameters { get; private set; }
	public ReceivingStorageArea ReturningParameter { get; private set; }


	public ProcedureDivision(Compiler.Nodes.FunctionDeclaration declaration, List<Compiler.Nodes.Node> sentences): base(null) {
		UsingParameters = new List<InputParameter>();
		// TCRFUN_CODEGEN_PARAMETERS_ORDER
		foreach(var parameter in declaration.Profile.InputParameters)
			if (((DataDescriptionEntry)parameter.CodeElement).LevelNumber.Value == 1)
				UsingParameters.Add(new GeneratedParameter(((ParameterDescriptionEntry)parameter.CodeElement).DataName));
		foreach(var parameter in declaration.Profile.InoutParameters)
			if (((DataDescriptionEntry)parameter.CodeElement).LevelNumber.Value == 1)
				UsingParameters.Add(new GeneratedParameter(((ParameterDescriptionEntry)parameter.CodeElement).DataName));
		foreach(var parameter in declaration.Profile.OutputParameters)
			if (((DataDescriptionEntry)parameter.CodeElement).LevelNumber.Value == 1)
				UsingParameters.Add(new GeneratedParameter(((ParameterDescriptionEntry)parameter.CodeElement).DataName));
		// TCRFUN_CODEGEN_RETURNING_PARAMETER
		if (declaration.Profile.ReturningParameter != null)
			if (((DataDescriptionEntry)declaration.Profile.ReturningParameter.CodeElement).LevelNumber.Value == 1)
				ReturningParameter = GeneratedParameter.CreateReceivingStorageArea(((ParameterDescriptionEntry)declaration.Profile.ReturningParameter.CodeElement).DataName);

		this.children.AddRange(sentences);
	}

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "PROCEDURE DIVISION", null));
				int c = 0;
				var done = new List<string>();
				foreach(var parameter in UsingParameters) {
					var data = parameter.ReceivingStorageArea.StorageArea;
					string name = data != null? data.SymbolReference.Name : null;
					if (done.Contains(name)) continue;
					else done.Add(name);
					string strmode = "BY REFERENCE ";
					if (parameter.ReceivingMode.Value == ReceivingMode.ByValue) strmode = "BY VALUE ";
					string strusing = c==0? "      USING ":"            ";
					string strname = "?ANONYMOUS?";
					if (parameter.ReceivingStorageArea.StorageArea != null) strname = name;
					_cache.Add(new TextLineSnapshot(-1, strusing+strmode+strname, null));
					c++;
				}
				if (ReturningParameter != null) {
					string strmode = "BY REFERENCE ";
					string strusing = c==0? "      USING ":"            ";
					string strname = "?ANONYMOUS?";
					var named = ReturningParameter.StorageArea;
					if (named != null) strname = named.SymbolReference.Name;
					_cache.Add(new TextLineSnapshot(-1, strusing+strmode+strname, null));
				}
				_cache.Add(new TextLineSnapshot(-1, "    .", null));
			}
			return _cache;
		}
	}

	public bool IsLeaf { get { return false; } }
}



	public class GeneratedParameter: InputParameter {
//		public GeneratedParameter(ReceivingStorageArea storage): base(storage, null) {
//			var mode = TypeCobol.Compiler.CodeElements.ReceivingMode.ByReference;
//			this.ReceivingMode = new SyntaxProperty<ReceivingMode>(mode, null);
//		}

		public GeneratedParameter(SymbolDefinition symbol) {
			this.ReceivingStorageArea = CreateReceivingStorageArea(symbol);
			var mode = TypeCobol.Compiler.CodeElements.ReceivingMode.ByReference;
			this.ReceivingMode = new SyntaxProperty<ReceivingMode>(mode, null);
		}

		public static ReceivingStorageArea CreateReceivingStorageArea(SymbolDefinition symbol) {
			if (symbol == null) return null;
			var storage = new DataOrConditionStorageArea(new SymbolReference(symbol));
			return new ReceivingStorageArea(StorageDataType.Any, storage);
		}
	}
}
