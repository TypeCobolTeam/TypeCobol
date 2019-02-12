using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;
using Type = TypeCobol.Compiler.Types.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// The Symbol of a Function declaration
    /// </summary>
    public class FunctionSymbol : ProgramSymbol
    {
        /// <summary>
        /// Name constructor
        /// </summary>
        /// <param name="name">Function's name</param>
        public FunctionSymbol(String name) : base(name)
        {
            //Override the Kind here.
            base.Kind = Kinds.Function;
        }

        /// <summary>
        /// Full constructor.
        /// </summary>
        /// <param name="name">Function's name</param>
        /// <param name="parameters">Function's paramaetrs</param>
        /// <param name="returnType">Function's return type</param>
        public FunctionSymbol(String name, List<VariableSymbol> parameters, FunctionType returnType)
            : base(name)
        {
            this.FunctionType = new FunctionType(parameters, returnType);
        }

        /// <summary>
        /// Function parameters.
        /// </summary>
        public List<VariableSymbol> Parameters => FunctionType?.Parameters;

        /// <summary>
        /// The return type
        /// </summary>
        public Type ReturnType => FunctionType?.ReturnType;

        /// <summary>
        /// The Function's type.
        /// </summary>
        public FunctionType FunctionType
        {
            get => (FunctionType)base.Type;
            set
            {
                //Enter Parameters in the Scope.
                foreach (var param in value.Parameters)
                {
                    LinkageStorageData.Enter(param);
                }
                base.Type = value;                
            }
        }

        /// <summary>
        /// Func
        /// </summary>
        public override Type Type
        {
            get => base.Type;
            set
            {
                //Ensure that this is the type of a function.
                System.Diagnostics.Debug.Assert(value is FunctionType);
                this.FunctionType = (FunctionType)value;
            }
        }
    }
}
