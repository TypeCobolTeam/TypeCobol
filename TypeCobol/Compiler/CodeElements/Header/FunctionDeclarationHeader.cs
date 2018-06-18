using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;

    /// <summary>TypeCobol function declaration</summary>
    public class FunctionDeclarationHeader: CodeElement {
	    public SymbolDefinition FunctionName { get; private set; }
	    public AccessModifier Visibility { get; private set; }
	    public FunctionType UserDefinedType { get; private set; }
	    public FunctionType ActualType {
		    get {
			    if ( Profile.IsFunction && !Profile.IsProcedure) return FunctionType.Function;
			    if (!Profile.IsFunction &&  Profile.IsProcedure) return FunctionType.Procedure;
			    if ( Profile.IsFunction &&  Profile.IsProcedure)
				    if (UserDefinedType == FunctionType.Undefined)
				         return FunctionType.Function;
				    else return UserDefinedType;
			    return FunctionType.Undefined;
		    }
	    }

	    public FunctionDeclarationHeader(SymbolDefinition name, AccessModifier visibility, FunctionType type)
		    : base(CodeElementType.FunctionDeclarationHeader) {
		    this.FunctionName = name;
		    this.Visibility = visibility;
		    this.UserDefinedType = type;
		    this.Profile = new ParametersProfile();
	    }

	    // TO DO : remove this and move to second parsing phase
	    private string libraryName;
	    public string Name { get { return libraryName != null ? libraryName + "." + FunctionName.Name : FunctionName.Name; } }
	    public void SetLibrary(string libname) { libraryName = libname; }

	     // PROFILE
	    /////////////
	    public ParametersProfile Profile { get; private set; }
	    /// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
	    public SyntaxProperty<ParameterPassingDirection> Input { get; internal set; }
	    /// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
	    public SyntaxProperty<ParameterPassingDirection> Output { get; internal set; }
	    /// <summary>INOUT datanames, always passed BY REFERENCE.</summary>
	    public SyntaxProperty<ParameterPassingDirection> Inout { get; internal set; }
	    /// <summary>RETURNING dataname.</summary>
	    public SyntaxProperty<ParameterPassingDirection> Returning { get; internal set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, FunctionName, Profile, Input, Output, Inout, Returning);
        }

        public override string ToString() {
		    var str = new System.Text.StringBuilder();
		    str.Append(Name).Append(Profile).Append(':').Append(Visibility).Append(':');
		    str.Append(UserDefinedType).Append('/').Append(ActualType);
		    return str.ToString();
	    }
    }

    public enum AccessModifier {
	    Public,
	    Private,
    }

    public enum FunctionType: int {
	    Undefined = 0,
	    Function  = 1,
	    Procedure = 2,
    }

    public interface ParameterList {
	    IList<DataType> InputParameters { get; }
	    IList<DataType> InoutParameters { get; }
	    IList<DataType> OutputParameters { get; }
	    DataType ReturningParameter { get; }
    }


    public static class ParameterListHelper
    {
        /// <summary>
        /// Get the signature of the ParameterList as string.
        /// This string is intended to be displayed to the user.
        /// </summary>
        /// <returns></returns>
        public static string GetSignature([NotNull] this ParameterList parameterList)
        {
            var str = new System.Text.StringBuilder();

            if (parameterList.InputParameters.Count > 0)
            {
                str.Append("input(");
                foreach (var p in parameterList.InputParameters) str.Append(p).Append(", ");
                str.Length -= 2;
                str.Append(")");
            }
            if (parameterList.InoutParameters.Count > 0)
            {
                str.Append(" in-out(");
                foreach (var p in parameterList.InoutParameters) str.Append(p).Append(", ");
                str.Length -= 2;
                str.Append(")");
            }
            if (parameterList.OutputParameters.Count > 0)
            {
                str.Append(" output(");
                foreach (var p in parameterList.OutputParameters) str.Append(p).Append(", ");
                str.Length -= 2;
                str.Append(")");
            }
            if (parameterList.ReturningParameter != null) str.Append(" : ").Append(parameterList.ReturningParameter);
            return str.ToString();
        }
    }

    public class ParametersProfile: CodeElement, ParameterList, IVisitable
    {
	    public IList<ParameterDescriptionEntry> InputParameters { get; set; }
	    public IList<ParameterDescriptionEntry> InoutParameters { get; set; }
	    public IList<ParameterDescriptionEntry> OutputParameters { get; set; }
	    public ParameterDescriptionEntry ReturningParameter { get; set; }

	    public ParametersProfile() : base(CodeElementType.ParametersProfile) {
		    InputParameters = new List<ParameterDescriptionEntry>();
		    InoutParameters = new List<ParameterDescriptionEntry>();
		    OutputParameters = new List<ParameterDescriptionEntry>();
		    ReturningParameter = null;
	    }

	    public IList<ParameterDescriptionEntry> Parameters {
		    get {
			    var parameters = new List<ParameterDescriptionEntry>();
			    parameters.AddRange(InputParameters);
			    parameters.AddRange(InoutParameters);
			    parameters.AddRange(OutputParameters);
			    return parameters;
		    }
	    }

	    /// <summary>TCRFUN_NO_INOUT_OR_OUTPUT_FOR_FUNCTIONS</summary>
	    public bool IsFunction  { get { return InoutParameters.Count < 1 && OutputParameters.Count < 1; } }
	    /// <summary>TCRFUN_NO_RETURNING_FOR_PROCEDURES</summary>
	    public bool IsProcedure { get { return ReturningParameter == null; } }

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
		    okay = AreEqual(OutputParameters, o.OutputParameters);
		    return okay;
	    }

	    private static bool AreEqual(IList<ParameterDescriptionEntry> mine, IList<ParameterDescriptionEntry> hers) {
		    if (mine.Count != hers.Count) return false;
		    for (int c = 0; c < mine.Count; c++) {
			    if (!mine[c].Equals(hers[c])) return false;
			    if (!mine[c].Equals(hers[c])) return false;
		    }
		    return true;
	    }

	    public override int GetHashCode() {
		    int hash = 17;
		    foreach (var p in Parameters) hash = hash * 23 + p.GetHashCode();
		    return hash;
	    }

        public new bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, InputParameters,
                                                               InoutParameters,
                                                               OutputParameters) 
                       && this.ContinueVisitToChildren(astVisitor, ReturningParameter);
        }

        public override string ToString()
        {
		    var str = new System.Text.StringBuilder();
		    str.Append('(');
		    foreach (var p in InputParameters) str.Append(p.Name).Append(p.IsOmittable ? " ?" : "").Append(':').Append(p.DataType).Append(", ");
		    if (InputParameters.Count > 0) str.Length -= 2;
		    str.Append("):(");
		    foreach (var p in OutputParameters) str.Append(p.Name).Append(p.IsOmittable ? " ?" : "").Append(':').Append(p.DataType).Append(", ");
		    if (OutputParameters.Count > 0) str.Length -= 2;
		    str.Append("):(");
		    foreach (var p in InoutParameters) str.Append(p.Name).Append(p.IsOmittable ? " ?" : "").Append(':').Append(p.DataType).Append(", ");
		    if (InoutParameters.Count > 0) str.Length -= 2;
		    str.Append("):(");
		    if (ReturningParameter != null) str.Append(ReturningParameter.Name).Append(':').Append(ReturningParameter.DataType);
		    str.Append(')');
		    return str.ToString();
	    }



	    IList<DataType> _icache = null;
	    IList<DataType> ParameterList.InputParameters {
		    get {
			    if (_icache != null) return _icache;
			    _icache = new List<DataType>();
			    foreach(var parameter in this.InputParameters)
				    _icache.Add(parameter.DataType);
			    return _icache;
		    }
	    }
	    IList<DataType> _ycache = null;
	    IList<DataType> ParameterList.InoutParameters {
		    get {
			    if (_ycache != null) return _ycache;
			    _ycache = new List<DataType>();
			    foreach(var parameter in this.InoutParameters)
				    _ycache.Add(parameter.DataType);
			    return _ycache;
		    }
	    }
	    IList<DataType> _ocache = null;
	    IList<DataType> ParameterList.OutputParameters {
		    get {
			    if (_ocache != null) return _ocache;
			    _ocache = new List<DataType>();
			    foreach(var parameter in this.OutputParameters)
				    _ocache.Add(parameter.DataType);
			    return _ocache;
		    }
	    }
	    DataType ParameterList.ReturningParameter {
		    get {
			    if (this.ReturningParameter == null) return null;
			    return this.ReturningParameter.DataType;
		    }
	    }
    }

    public class Passing {
	    public SyntaxProperty<ParameterPassingDirection> PassingMode { get; set; }
    }

    public class FunctionDeclarationEnd: CodeElement {
	    public FunctionDeclarationEnd(): base(CodeElementType.FunctionDeclarationEnd) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
