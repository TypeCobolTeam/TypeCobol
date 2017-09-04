﻿

using JetBrains.Annotations;

namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Text;
	using TypeCobol.Compiler.CodeModel;

internal class ProcedureDivision: Compiler.Nodes.ProcedureDivision, Generated {

	public IList<CallTargetParameter> UsingParameters { get; private set; }
	public CallTargetParameter ReturningParameter { get; private set; }
	private SymbolTable table;


	public ProcedureDivision(Compiler.Nodes.FunctionDeclaration declaration, [NotNull] List<Compiler.Nodes.Node> sentences): base(null) {
		table = declaration.SymbolTable;
		UsingParameters = new List<CallTargetParameter>();
		// TCRFUN_CODEGEN_PARAMETERS_ORDER
		foreach(var parameter in declaration.Profile.InputParameters)
			if (parameter.LevelNumber.Value == 1)
				UsingParameters.Add(new GeneratedParameter(parameter.DataName));
		foreach(var parameter in declaration.Profile.InoutParameters)
			if (parameter.LevelNumber.Value == 1)
				UsingParameters.Add(new GeneratedParameter(parameter.DataName));
		foreach(var parameter in declaration.Profile.OutputParameters)
			if (parameter.LevelNumber.Value == 1)
				UsingParameters.Add(new GeneratedParameter(parameter.DataName));
		// TCRFUN_CODEGEN_RETURNING_PARAMETER
		if (declaration.Profile.ReturningParameter != null)
			if (declaration.Profile.ReturningParameter.LevelNumber.Value == 1)
				ReturningParameter = new CallTargetParameter() { StorageArea = GeneratedParameter.CreateReceivingStorageArea(declaration.Profile.ReturningParameter.DataName) };

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
					var data = parameter.StorageArea;
					string name = data != null? data.SymbolReference.Name : null;
					if (done.Contains(name)) continue;
					else done.Add(name);
					string strmode = "BY REFERENCE ";
					if (parameter.SharingMode.Value == ParameterSharingMode.ByValue) strmode = "BY VALUE ";
					string strusing = c==0? "      USING ":"            ";
					string strname = "?ANONYMOUS?";
					if (parameter.StorageArea != null) strname = CreateName(data.SymbolReference);
					_cache.Add(new TextLineSnapshot(-1, strusing+strmode+strname, null));
					c++;
				}
				if (ReturningParameter != null) {
					string strmode = "BY REFERENCE ";
					string strusing = c==0? "      USING ":"            ";
					string strname = "?ANONYMOUS?";
					var named = ReturningParameter.StorageArea;
					if (named != null) strname = CreateName(named.SymbolReference);
					_cache.Add(new TextLineSnapshot(-1, strusing+strmode+strname, null));
				}
				_cache.Add(new TextLineSnapshot(-1, "    .", null));
			}
			return _cache;
		}
	}
	private string CreateName(SymbolReference symbolReference) {
	    var name = symbolReference.Name;
        var found = table.GetVariable(symbolReference);
		if (found.Count < 1) return "?NOT_FOUND?";
		if (found.Count > 1) return name;
		var pentry = (DataDescriptionEntry)found[0].CodeElement;
		if (pentry.DataType == DataType.Boolean) return name+"-value";
		return name;
	}

	public bool IsLeaf { get { return false; } }
}

public class GeneratedParameter: CallTargetParameter {
	public GeneratedParameter(SymbolDefinition symbol) {
		this.StorageArea = CreateReceivingStorageArea(symbol);
		var mode = TypeCobol.Compiler.CodeElements.ParameterSharingMode.ByReference;
		this.SharingMode = new SyntaxProperty<ParameterSharingMode>(mode, null);
	}

	public static StorageArea CreateReceivingStorageArea(SymbolDefinition symbol) {
		if (symbol == null) return null;
		var storage = new DataOrConditionStorageArea(new SymbolReference(symbol));
		return storage;
	}
}



internal class Sentence: Compiler.Nodes.Sentence, Generated {
	public Sentence(): base() { }

	private IList<ITextLine> _nuthin = new List<ITextLine>();
	public override IEnumerable<ITextLine> Lines {
		get {
			return _nuthin;
		}
	}
	public bool IsLeaf { get { return false; } }
}
internal class SentenceEnd: Compiler.Nodes.End, Generated {
	public SentenceEnd(): base(null) { }

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, "    .", null));
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return true; } }
}

}
