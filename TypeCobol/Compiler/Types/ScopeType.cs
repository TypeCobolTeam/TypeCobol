using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The Type of either a Program or a Function.
    /// </summary>
    public class ScopeType : Type
    {
        /// <summary>
        /// Builds a new ScopeType.
        /// </summary>
        /// <param name="parameters">Non-null list of parameters.</param>
        /// <param name="returnVariable">Return variable if any.</param>
        public ScopeType(List<VariableSymbol> parameters, VariableSymbol returnVariable)
            : base(Tags.Scope)
        {
            System.Diagnostics.Debug.Assert(parameters != null);
            Parameters = parameters;
            ReturnVariable = returnVariable;
        }

        /// <summary>
        /// Parameters variables.
        /// </summary>
        public List<VariableSymbol> Parameters { get; }

        /// <summary>
        /// The return variable if any.
        /// </summary>
        public VariableSymbol ReturnVariable { get; }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            if (Parameters != null && Parameters.Count > 0)
            {
                output.Write(indent);
                output.WriteLine($"Parameters: [{string.Join(", ", Parameters.Select(v => v.FullName))}]");//Write reference
            }

            if (ReturnVariable != null)
            {
                output.Write(indent);
                output.WriteLine($"ReturnVariable: {ReturnVariable.FullName}");//Write reference
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitScopeType(this, arg);
        }
    }
}
