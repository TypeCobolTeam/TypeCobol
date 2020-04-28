using System.Collections.Generic;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// The Function Type.
    /// </summary>
    public class FunctionType : ProgramType
    {
        /// <summary>
        /// Empty Constructor.
        /// </summary>
        public FunctionType()
            : base(Tags.Function)
        {
        }

        /// <summary>
        /// Symbol constructor
        /// </summary>
        /// <param name="fun"></param>
        public FunctionType(FunctionSymbol fun)
            : base(Tags.Function)
        {
            base.Symbol = fun;
        }

        /// <summary>
        /// Full constructor
        /// </summary>
        /// <param name="parameters">Function's parameters</param>
        /// <param name="retVar">Function's return variable</param>
        public FunctionType(List<VariableSymbol> parameters, VariableSymbol retVar)
            : base(Tags.Function)
        {
            Parameters = parameters;
            ReturnSymbol = retVar;
        }

        /// <summary>
        ///  Function parameters.
        /// </summary>
        public List<VariableSymbol> Parameters
        {
            get => base.Usings;
            set => base.Usings = value;
        }

        /// <summary>
        /// The returned symbol.
        /// </summary>
        public VariableSymbol ReturnSymbol
        {
            get;
            set;
        }

        /// <summary>
        /// The Parameters Types
        /// </summary>
        public IEnumerable<Type> ParameterTypes
        {
            get { return Parameters?.ConvertAll(s => s.Type); }
        }

        /// <summary>
        /// The returned type.
        /// </summary>
        public Type ReturnType => ReturnSymbol?.Type;

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitFunctionType(this, arg);
        }
    }
}
