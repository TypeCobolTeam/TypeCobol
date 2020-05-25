//using System.Collections.Generic;

using System;
using System.Collections.Generic;
using System.Linq;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// The Symbol of a Function declaration
    /// </summary>
    public class FunctionSymbol : ScopeSymbol
    {
        /// <summary>
        /// Name constructor
        /// </summary>
        /// <param name="name">Function's name</param>
        /// <remarks>Do not forget to set FunctionType after calling this.</remarks>
        public FunctionSymbol(string name)
            : base(name, Kinds.Function)
        {

        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitFunctionSymbol(this, arg);
        }


        /// <summary>
        /// Check if a profile (e.g. a list of input, inout, output and returning arguments) match this function signature.
        /// 
        /// There are 2 specials cases, where the Type is not the original Type of the VariableSymbol:
        /// 
        /// call myFunction input myArray(1)   => In this case, ArrayType is removed as we only target one element of the array
        /// call myFunction input myVar(1:2)   => In this case, the Type is a pic X(2). TODO check this when myVar is a PIC N.
        /// 
        /// This method don't produce Diagnostics directly. TODO introduce DiagnosticReporter
        /// 
        /// Note : 
        /// At beginning of the mob programming, we decided to pass VariableSymbol and Type because not every properties are on Type.
        /// But at the end of the mob, we discussed to move properties to Type.
        /// So maybe VariableSymbol will not be useful anymore.
        /// </summary>
        /// <param name="inputTypes"></param>
        /// <param name="inoutTypes"></param>
        /// <param name="outputTypes"></param>
        /// <param name="returningType"></param>
        /// <returns></returns>
        public bool CheckProfile(List<Tuple<VariableSymbol, Type>> inputTypes, 
            List<Tuple<VariableSymbol, Type>> inoutTypes, 
            List<Tuple<VariableSymbol, Type>> outputTypes, 
            Tuple<VariableSymbol, Type> returningType)
        {
            var inputParameters = Type.Parameters.Where(v => v.HasFlag(Flags.Input));
            var inoutParameters = Type.Parameters.Where(v => v.HasFlag(Flags.Inout));
            var outputParameters = Type.Parameters.Where(v => v.HasFlag(Flags.Output));

            //if(ReturnType. match returningArgument.Type)
        }


        /// <summary>
        /// Compare 2 Type to see if they match for a call statement.
        /// 
        /// TODO move to a dedicated class
        /// TODO introduce DiagnosticReporter
        /// 
        /// Note:
        /// At beginning of the mob programming, we decided to pass VariableSymbol and Type because not every properties are on Type.
        /// But at the end of the mob, we discussed to move properties to Type.
        /// So maybe VariableSymbol will not be useful anymore.
        /// </summary>
        /// <param name="type1">The caller type ?</param>
        /// <param name="type2">calleE type ?</param>
        /// <returns></returns>
        public bool MatchForCall(Tuple<VariableSymbol, Type> type1, Tuple<VariableSymbol, Type> type2)
        {
            if(type1.Item2.IsEquivalentTo(type2.Item2))
            {
                //Compare flags
                if(type1.Item1.HasFlag(Flags.BlankWhenZero) ^ type2.Item1.HasFlag(Flags.BlankWhenZero))
                {
                    return false;
                }
                //TODO compare all flags
            }
            return false;

        }
    }
}
