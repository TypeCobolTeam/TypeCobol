using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Nodes
{
    public class ParametersProfileNode : GenericNode<ParametersProfile>, ParameterList
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



        public override bool Equals(object other)
        {
            if (other == null || GetType() != other.GetType()) return false;
            var o = other as ParametersProfileNode;
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

        private static bool AreEqual(IList<ParameterDescription> mine, IList<ParameterDescription> hers)
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
