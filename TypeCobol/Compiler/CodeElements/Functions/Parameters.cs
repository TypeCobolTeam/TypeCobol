namespace TypeCobol.Compiler.CodeElements.Functions {

using System;
using System.Collections.Generic;

public class Passing {
	public SyntaxProperty<Mode> PassingMode { get; set; }
	public enum Mode {
		Input,
		Output,
		Inout,
		Returning,
	}
}

public class ParameterDescription: DataDescriptionEntry {
	// TODO#245
	// create an interface shared with DataDeclarationEntry
	// that aggregates all the non-illegal stuff like justified, 
	// group usage national, blank when zero and so on
}

public class CallParameterDescription: ParameterDescription {
	public bool ByReference { private get; set; }
	public string SendingMode { get { return ByReference? "REFERENCE":"CONTENT"; } }
	public string Value { get; set; }
}

public class FunctionCall: Named {
	public FunctionCall(IntrinsicFunctionCallResult call) {
		QualifiedName = new Expressions.URI(call.IntrinsicFunctionName.Name);
		foreach(var variableOrExpression in call.Arguments)
			InputParameters.Add(new CallParameter(variableOrExpression));
	}
	/// <summary>Used for codegen.</summary>
	public FunctionCall(Expressions.QualifiedName name, string lib, string copy) {
		QualifiedName = name;
		Library = lib;
		Copy = copy;
	}

	public string Name { get { return QualifiedName.Head; } }
	public Expressions.QualifiedName QualifiedName { get; private set; }

	public IList<CallParameter> InputParameters = new List<CallParameter>();

	public string Library { get; set; }
	public string Copy    { get; set; }
}

public class CallParameter {

	private VariableOrExpression voe;
	public CallParameter(VariableOrExpression voe) { this.voe = voe; }

	public bool   IsLiteral   { get { return voe.IsLiteral; } }
	public string SendingMode { get { return IsLiteral? "CONTENT":"REFERENCE"; } }
	public string Value       { get { return voe.QualifiedName.ToString(); } }
}
public class EmptyCallParameter: CallParameter {
	public EmptyCallParameter(): base(null) { }
	public bool   IsLiteral   { get { return true; } }
	public string SendingMode { get { return "CONTENT"; } }
	public string Value       { get { return "SPACE"; } }
}



public class ParametersProfile {
	public IList<ParameterDescription> InputParameters { get; set; }
	public IList<ParameterDescription> OutputParameters { get; set; }
	public IList<ParameterDescription> InoutParameters { get; set; }
	public ParameterDescription ReturningParameter { get; set; }

	public ParametersProfile() {
		InputParameters  = new List<ParameterDescription>();
		OutputParameters = new List<ParameterDescription>();
		InoutParameters  = new List<ParameterDescription>();
		ReturningParameter = null;
	}

	public IList<ParameterDescription> Parameters {
		get {
			var parameters = new List<ParameterDescription>();
			parameters.AddRange(InputParameters);
			parameters.AddRange(InoutParameters);
			parameters.AddRange(OutputParameters);
			return parameters;
		}
	}

	public override bool Equals(object other) {
		if (other == null || GetType() != other.GetType()) return false;
		var o = other as ParametersProfile;
		if (o == null) return false;
		// instead of doing foreach(var mode in Tools.Reflection.GetValues<Passing.Mode>()) ...,
		// only iterate over input+output+inout parameters: returning parameter does not have
		// any impact in conflict between function profiles resolution
		bool okay = true;
		okay = AreEqual(InputParameters, o.InputParameters);
		if (!okay) return false;
		okay = AreEqual(InoutParameters, o.InoutParameters);
		if (!okay) return false;
		okay = AreEqual(OutputParameters,o.OutputParameters);
		return okay;
    }

	private static bool AreEqual(IList<ParameterDescription> mine, IList<ParameterDescription> hers) {
		if (mine.Count != hers.Count) return false;
		for (int c=0; c<mine.Count; c++) {
			if (!mine[c].Equals(hers[c])) return false;
			if (!mine[c].Equals(hers[c])) return false;
		}
		return true;
	}
    
	public override int GetHashCode() {
		int hash = 17;
		foreach(var p in Parameters) hash = hash*23 + p.GetHashCode();
		return hash;
	}


	public override string ToString() {
		var str = new System.Text.StringBuilder();
		str.Append('(');
		foreach(var p in InputParameters) str.Append(p).Append(", ");
		if (InputParameters.Count > 0) str.Length -= 2;
		str.Append("):(");
		foreach(var p in OutputParameters) str.Append(p).Append(", ");
		if (OutputParameters.Count > 0) str.Length -= 2;
		str.Append("):(");
		foreach(var p in InoutParameters) str.Append(p).Append(", ");
		if (InoutParameters.Count > 0) str.Length -= 2;
		str.Append("):(");
		if (ReturningParameter != null) str.Append(ReturningParameter);
		str.Append(')');
		return str.ToString();
	}

}

}
