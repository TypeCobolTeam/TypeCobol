using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Nodes
{
    public class ParametersProfileNode : GenericNode<ParametersProfile>, ParameterList, IEquatable<ParametersProfileNode>
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

        IList<DataType> _icache = null;
        IList<DataType> ParameterList.InputParameters
        {
            get
            {
                if (_icache != null) return _icache;
                _icache = new List<DataType>();
                foreach (var parameter in this.InputParameters)
                    _icache.Add(parameter.DataType);
                return _icache;
            }
        }
        IList<DataType> _ycache = null;
        IList<DataType> ParameterList.InoutParameters
        {
            get
            {
                if (_ycache != null) return _ycache;
                _ycache = new List<DataType>();
                foreach (var parameter in this.InoutParameters)
                    _ycache.Add(parameter.DataType);
                return _ycache;
            }
        }
        IList<DataType> _ocache = null;
        IList<DataType> ParameterList.OutputParameters
        {
            get
            {
                if (_ocache != null) return _ocache;
                _ocache = new List<DataType>();
                foreach (var parameter in this.OutputParameters)
                    _ocache.Add(parameter.DataType);
                return _ocache;
            }
        }
        DataType ParameterList.ReturningParameter
        {
            get
            {
                if (this.ReturningParameter == null) return null;
                return this.ReturningParameter.DataType;
            }
        }
    }
}
