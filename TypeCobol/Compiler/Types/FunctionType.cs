using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        /// <param name="returnType">Function's return type</param>
        public FunctionType(List<VariableSymbol> parameters, FunctionType returnType)
            : base(Tags.Function)
        {
            Parameters = parameters;
            ReturnType = returnType;
        }

        /// <summary>
        ///  Function parameters.
        /// </summary>
        public List<VariableSymbol> Parameters
        {
            get
            {
                return base.Usings;
            }
            set
            {
                base.Usings = value;
            }
        }

        /// <summary>
        /// The returned type.
        /// </summary>
        public Type ReturnType
        {
            get;
            set;
        }

    }
}
