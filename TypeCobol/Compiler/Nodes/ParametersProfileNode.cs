using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Nodes
{
    public class ParametersProfileNode : GenericNode<ParametersProfile>, IProfile, IEquatable<ParametersProfileNode>
    {
        public IList<ParameterDescription> InputParameters { get; set; }
        public IList<ParameterDescription> InoutParameters { get; set; }
        public IList<ParameterDescription> OutputParameters { get; set; }
        public ParameterDescription ReturningParameter { get; set; }

        public ParametersProfileNode(ParametersProfile profile): base(profile)
        {
            InputParameters = new List<ParameterDescription>();
            InoutParameters = new List<ParameterDescription>();
            OutputParameters = new List<ParameterDescription>();
            ReturningParameter = null;
        }

        public IList<ParameterDescription> Parameters
        {
            get
            {
                var parameters = new List<ParameterDescription>();
                parameters.AddRange(InputParameters);
                parameters.AddRange(InoutParameters);
                parameters.AddRange(OutputParameters);
                return parameters;
            }
        }

        /// <summary>TCRFUN_NO_INOUT_OR_OUTPUT_FOR_FUNCTIONS</summary>
        public bool IsFunction { get { return InoutParameters.Count < 1 && OutputParameters.Count < 1; } }
        /// <summary>TCRFUN_NO_RETURNING_FOR_PROCEDURES</summary>
        public bool IsProcedure { get { return ReturningParameter == null; } }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) && this.ContinueVisitToChildren(astVisitor, InputParameters, InoutParameters, OutputParameters) && this.ContinueVisitToChildren(astVisitor, ReturningParameter);
        }

        public override string ToString()
        {
            var str = new StringBuilder();
            str.Append('(');
            foreach (var p in InputParameters) str.Append(p.Name).Append(':').Append(((ParameterDescriptionEntry)p.CodeElement).DataType).Append(", ");
            if (InputParameters.Count > 0) str.Length -= 2;
            str.Append("):(");
            foreach (var p in OutputParameters) str.Append(p.Name).Append(':').Append(((ParameterDescriptionEntry)p.CodeElement).DataType).Append(", ");
            if (OutputParameters.Count > 0) str.Length -= 2;
            str.Append("):(");
            foreach (var p in InoutParameters) str.Append(p.Name).Append(':').Append(((ParameterDescriptionEntry)p.CodeElement).DataType).Append(", ");
            if (InoutParameters.Count > 0) str.Length -= 2;
            str.Append("):(");
            if (ReturningParameter != null) str.Append(ReturningParameter.Name).Append(':').Append((((ParameterDescriptionEntry)ReturningParameter.CodeElement).DataType));
            str.Append(')');
            return str.ToString();
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as ParametersProfileNode);
        }

        public bool Equals(ParametersProfileNode parametersProfileNode)
        {
            if (Object.ReferenceEquals(this, parametersProfileNode)) return true;
            if (Object.ReferenceEquals(null, parametersProfileNode)) return false;

            return base.CodeElement.Equals(parametersProfileNode.CodeElement);
        }

        public override int GetHashCode()
        {
            return base.CodeElement.GetHashCode();
        }

        private IList<TypeInfo> _inputsCache;
        public IList<TypeInfo> Inputs => _inputsCache ?? (_inputsCache = InputParameters.Select(MakeTypeInfo).ToList());

        private IList<TypeInfo> _inoutsCache;
        public IList<TypeInfo> Inouts => _inoutsCache ?? (_inoutsCache = InoutParameters.Select(MakeTypeInfo).ToList());

        private IList<TypeInfo> _outputsCache;
        public IList<TypeInfo> Outputs => _outputsCache ?? (_outputsCache = OutputParameters.Select(MakeTypeInfo).ToList());

        private TypeInfo _returningCache;
        public TypeInfo Returning => _returningCache ?? (_returningCache = MakeTypeInfo(ReturningParameter));

        private static TypeInfo MakeTypeInfo(ParameterDescription parameter) =>
            new TypeInfo()
            {
                DataType = parameter.DataType,
                TypeDefinition = parameter.TypeDefinition
            };
    }
}
