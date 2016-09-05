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

	public string SendingMode { get { return voe.IsLiteral? "CONTENT":"REFERENCE"; } }
	public string Value { get { return voe.QualifiedName.ToString(); } }
}
public class EmptyCallParameter: CallParameter {
	public EmptyCallParameter(): base(null) { }
	public string SendingMode { get { return "CONTENT"; } }
	public string Value       { get { return "SPACE"; } }
}


public interface Profile {
	Dictionary<Passing.Mode, IList<System.Tuple<DataType,int>>> Parameters { get; }
	System.Tuple<DataType,int> Return { get; }
}



public class ParametersProfile: Profile {
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

	public Dictionary<Passing.Mode,IList<Tuple<DataType,int>>> Parameters {
		get {
			var parameters = new Dictionary<Passing.Mode,IList<Tuple<DataType,int>>>();
			parameters[Passing.Mode.Input] = new List<Tuple<DataType,int>>();
//TODO#249			foreach(var p in InputParameters) parameters[Passing.Mode.Input].Add(new Tuple<DataType,int>(p.DataType, p.MemoryArea.Length));
			parameters[Passing.Mode.Inout] = new List<Tuple<DataType,int>>();
//TODO#249			foreach(var p in InoutParameters) parameters[Passing.Mode.Inout].Add(new Tuple<DataType,int>(p.DataType, p.MemoryArea.Length));
			parameters[Passing.Mode.Output] = new List<Tuple<DataType,int>>();
//TODO#249			foreach(var p in OutputParameters) parameters[Passing.Mode.Output].Add(new Tuple<DataType,int>(p.DataType, p.MemoryArea.Length));
			return parameters;
		}
	}
	public Tuple<DataType,int> Return {
		get {
//TODO#249			if (ReturningParameter == null) return null;
//TODO#249			return new Tuple<DataType,int>(ReturningParameter.DataType, ReturningParameter.MemoryArea.Length);
				return null;
		}
	}

	public override bool Equals(object other) {
		if (other == null || GetType() != other.GetType()) return false;
		var o = other as Profile;
		if (o == null) return false;
		// instead of doing foreach(var mode in Tools.Reflection.GetValues<Passing.Mode>()) ...,
		// only iterate over input+output+inout parameters: returning parameter does not have
		// any impact in conflict between function profiles resolution
		foreach(var mode in new List<Passing.Mode> { Passing.Mode.Input, Passing.Mode.Output, Passing.Mode.Inout }) {
			var mine =   Parameters.ContainsKey(mode) ?   Parameters[mode] : new List<Tuple<DataType,int>>();
			var hers = o.Parameters.ContainsKey(mode) ? o.Parameters[mode] : new List<Tuple<DataType,int>>();
			if (mine.Count != hers.Count) return false;
			for (int c=0; c<mine.Count; c++) {
				if (!mine[c].Item1.Equals(hers[c].Item1)) return false;
				if (!mine[c].Item2.Equals(hers[c].Item2)) return false;
			}
		}
		return true;
    }
    
	public override int GetHashCode() {
		int hash = 0;
		foreach(var mode in Tools.Reflection.GetValues<Passing.Mode>()) {
			hash = hash*17 + mode.GetHashCode();
			foreach(var p in Parameters[mode]) hash = hash*23 + p.GetHashCode();
		}
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
