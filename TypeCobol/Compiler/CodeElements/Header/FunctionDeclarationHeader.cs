using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{

    /// <summary>
    /// TypeCobol function declaration
    /// </summary>
    public class FunctionDeclarationHeader : CodeElement
    {
        public SymbolDefinition FunctionName { get; private set; }
        public AccessModifier Visibility { get; private set; }

        public FunctionDeclarationHeader(SymbolDefinition functionName, AccessModifier Visibility)
            : base(CodeElementType.FunctionDeclarationHeader)
        {
            this.FunctionName = functionName;
            this.Visibility = Visibility;
            this.Profile = new ParametersProfile();
        }

        // TO DO : remove this and move to second parsing phase
        private string libraryName;
        public string Name { get { return libraryName != null ? libraryName + "." + FunctionName.Name : FunctionName.Name; } }
        public void SetLibrary(string libname)
        {
            libraryName = libname;
        }

        // PROFILE
        /////////////
        public ParametersProfile Profile { get; private set; }
        /// <summary>INPUT datanames, as long as wether they are passed BY REFERENCE or BY VALUE.</summary>
        public SyntaxProperty<Passing.Mode> Input { get; internal set; }
        /// <summary>OUTPUT datanames, always passed BY REFERENCE.</summary>
        public SyntaxProperty<Passing.Mode> Output { get; internal set; }
        /// <summary>INOUT datanames, always passed BY REFERENCE.</summary>
        public SyntaxProperty<Passing.Mode> Inout { get; internal set; }
        /// <summary>RETURNING dataname.</summary>
        public SyntaxProperty<Passing.Mode> Returning { get; internal set; }
    }

    public enum AccessModifier
    {
        Public,
        Private
    }

    public class ParametersProfile
    {
        public IList<ParameterDescriptionEntry> InputParameters { get; set; }
        public IList<ParameterDescriptionEntry> OutputParameters { get; set; }
        public IList<ParameterDescriptionEntry> InoutParameters { get; set; }
        public ParameterDescriptionEntry ReturningParameter { get; set; }

        public ParametersProfile()
        {
            InputParameters = new List<ParameterDescriptionEntry>();
            OutputParameters = new List<ParameterDescriptionEntry>();
            InoutParameters = new List<ParameterDescriptionEntry>();
            ReturningParameter = null;
        }

        public IList<ParameterDescriptionEntry> Parameters
        {
            get
            {
                var parameters = new List<ParameterDescriptionEntry>();
                parameters.AddRange(InputParameters);
                parameters.AddRange(InoutParameters);
                parameters.AddRange(OutputParameters);
                return parameters;
            }
        }

        public override bool Equals(object other)
        {
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

        private static bool AreEqual(IList<ParameterDescriptionEntry> mine, IList<ParameterDescriptionEntry> hers)
        {
            if (mine.Count != hers.Count) return false;
            for (int c = 0; c < mine.Count; c++)
            {
                if (!mine[c].Equals(hers[c])) return false;
                if (!mine[c].Equals(hers[c])) return false;
            }
            return true;
        }

        public override int GetHashCode()
        {
            int hash = 17;
            foreach (var p in Parameters) hash = hash * 23 + p.GetHashCode();
            return hash;
        }

        public override string ToString()
        {
            var str = new System.Text.StringBuilder();
            str.Append('(');
            foreach (var p in InputParameters) str.Append(p).Append(", ");
            if (InputParameters.Count > 0) str.Length -= 2;
            str.Append("):(");
            foreach (var p in OutputParameters) str.Append(p).Append(", ");
            if (OutputParameters.Count > 0) str.Length -= 2;
            str.Append("):(");
            foreach (var p in InoutParameters) str.Append(p).Append(", ");
            if (InoutParameters.Count > 0) str.Length -= 2;
            str.Append("):(");
            if (ReturningParameter != null) str.Append(ReturningParameter);
            str.Append(')');
            return str.ToString();
        }
    }

    /// <summary>
    /// TYPECOBOL : data conditions can be decalred inline when a user defined function parameter is described
    /// </summary>
    public class ParameterDescriptionEntry : DataDescriptionEntry
    {
        // TODO#245
        // create an interface shared with DataDeclarationEntry
        // that aggregates all the non-illegal stuff like justified,
        // group usage national, blank when zero and so on

        public IList<DataConditionEntry> DataConditions { get; internal set; }
    }

    public class Passing
    {
        public SyntaxProperty<Mode> PassingMode { get; set; }
        public enum Mode
        {
            Input,
            Output,
            Inout,
            Returning,
        }
    }



    public class FunctionDeclarationEnd : CodeElement
    {
        public FunctionDeclarationEnd() : base(CodeElementType.FunctionDeclarationEnd) { }
    }
}
