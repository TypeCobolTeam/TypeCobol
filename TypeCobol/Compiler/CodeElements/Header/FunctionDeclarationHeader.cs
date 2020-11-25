using System;
using System.Text;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;

    /// <summary>TypeCobol function declaration</summary>
    public class FunctionDeclarationHeader: CodeElement, IFormalizedCommentable
    {
	    public SymbolDefinition FunctionName { get; set; }
	    public AccessModifier Visibility { get; set; }
	    public FunctionType UserDefinedType { get; set; }
        public FormalizedCommentDocumentation FormalizedCommentDocumentation { get; set; }
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

        public FunctionDeclarationHeader()
            : base(CodeElementType.FunctionDeclarationHeader)
        {
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
        Local,
        Public,
	    Private,
    }

    public enum FunctionType: int {
	    Undefined = 0,
	    Function  = 1,
	    Procedure = 2,
    }

    public class TypeInfo
    {
        public DataType DataType { get; set; }
        public TypeDefinition TypeDefinition { get; set; }
    }

    public interface IProfile
    {
        IList<TypeInfo> Inputs { get; }
        IList<TypeInfo> Inouts { get; }
        IList<TypeInfo> Outputs { get; }
        TypeInfo Returning { get; }
    }

    public static class ParameterListHelper
    {
        /// <summary>
        /// Get the signature of the profile as string.
        /// This string is intended to be displayed to the user.
        /// </summary>
        /// <returns></returns>
        public static string GetSignature([NotNull] this IProfile profile)
        {
            var str = new System.Text.StringBuilder();

            if (profile.Inputs.Count > 0)
            {
                str.Append("input(");
                foreach (var p in profile.Inputs) str.Append(p.DataType.Name).Append(", ");
                str.Length -= 2;
                str.Append(")");
            }
            if (profile.Inouts.Count > 0)
            {
                str.Append(" in-out(");
                foreach (var p in profile.Inouts) str.Append(p.DataType.Name).Append(", ");
                str.Length -= 2;
                str.Append(")");
            }
            if (profile.Outputs.Count > 0)
            {
                str.Append(" output(");
                foreach (var p in profile.Outputs) str.Append(p.DataType.Name).Append(", ");
                str.Length -= 2;
                str.Append(")");
            }
            if (profile.Returning != null) str.Append(" : ").Append(profile.Returning.DataType.Name);
            return str.ToString();
        }

        public static IEnumerable<TextLineSnapshot> GetSignatureForComment([NotNull] this Nodes.ParametersProfileNode parameterList)
        {
            //Procedure arguments nature
            const string INPUT    = "input";
            const string INOUT    = "in-out";
            const string OUTPUT   = "output";
            const string RETURNS  = "returns";

            var signature = new List<TextLineSnapshot>();

            string paramsDescription;
            
            if (parameterList.InputParameters.Count > 0)
            {
                paramsDescription = $"*     {INPUT}({GetParameterDescriptions(parameterList.InputParameters)})";
                signature.Add(new TextLineSnapshot(-1, paramsDescription, null));
            }
            if (parameterList.InoutParameters.Count > 0)
            {
                paramsDescription = $"*     {INOUT}({GetParameterDescriptions(parameterList.InoutParameters)})";
                signature.Add(new TextLineSnapshot(-1, paramsDescription, null));
            }
            if (parameterList.OutputParameters.Count > 0)
            {
                paramsDescription = $"*     {OUTPUT}({GetParameterDescriptions(parameterList.OutputParameters)})";
                signature.Add(new TextLineSnapshot(-1, paramsDescription, null));
            }
            if (parameterList.ReturningParameter != null) {
                paramsDescription = $"*     {RETURNS}({GetParameterDetails(parameterList.ReturningParameter)})";
                signature.Add(new TextLineSnapshot(-1, paramsDescription, null));
            }
            return signature;
        }

        private static string GetParameterDescriptions(IList<ParameterDescription> parameters)
        {
            StringBuilder str = new StringBuilder();
            foreach (var p in parameters)
            {
                str.Append(GetParameterDetails(p));
                str.Append(", ");
            }
            str.Length -= 2;

            return str.ToString();
        }

        private static string GetParameterDetails(ParameterDescription parameter) {
            
            StringBuilder str = new StringBuilder();
            str.Append(parameter.DataName.Name + ": ");
            bool addSpaceSeparatorBeforeUsage = false;
            if (parameter.DataType.CobolLanguageLevel < CobolLanguageLevel.Cobol2002)
            {
                if (parameter.Picture != null)
                {
                    str.Append("pic ").Append(parameter.Picture);
                    addSpaceSeparatorBeforeUsage = true;
                }
            }
            else
            {
                //Do not write DataType if LanguageLevel is under Cobol 2002 
                //because DataType for Cobol85 is an internal representation of Cobol picture.
                str.Append(parameter.DataType);
                addSpaceSeparatorBeforeUsage = true;
            }


            //Use the Token.Text of usage instead of the DataUsage enum. 
            //Because Cobol developers won't understand enum value (eg for "comp-3" in Token.Text you get "PackedDecimal" in the enum).
            var usageToken = parameter.CodeElement.Usage?.Token;
            if (usageToken != null)
            {
                if (addSpaceSeparatorBeforeUsage)
                {
                    str.Append(" ");
                }
                str.Append(usageToken.Text);
            }

            return str.ToString();
        }
    }

    public class ParametersProfile: CodeElement, IVisitable, IEquatable<ParametersProfile>
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

        public override bool Equals(object other)
        {
            return Equals(other as ParametersProfile);
        }

        public bool Equals(ParametersProfile paramsProfile)
        {
            if (System.Object.ReferenceEquals(this, paramsProfile)) return true;
            if (System.Object.ReferenceEquals(null, paramsProfile)) return false;

            // instead of doing foreach(var mode in Tools.Reflection.GetValues<Passing.Mode>()) ...,
            // only iterate over input+output+inout parameters: returning parameter does not have
            // any impact in conflict between function profiles resolution
            bool okay = AreEqual(InputParameters, paramsProfile.InputParameters);
            if (!okay) return false;
            okay = AreEqual(InoutParameters, paramsProfile.InoutParameters);
            if (!okay) return false;
            return AreEqual(OutputParameters, paramsProfile.OutputParameters);
        }

        private static bool AreEqual(IList<ParameterDescriptionEntry> mine, IList<ParameterDescriptionEntry> hers)
        {
            if (mine.Count != hers.Count) return false;
            for (int c = 0; c < mine.Count; c++) {
                if (!mine[c].Equals(hers[c])) return false;
            } 
            return true;
        }

        public override int GetHashCode() {
            unchecked
            {
                var hashCode = 17;
                foreach (var p in Parameters) hashCode = (hashCode * 397) ^ p.GetHashCode();

                return hashCode;
            }
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
