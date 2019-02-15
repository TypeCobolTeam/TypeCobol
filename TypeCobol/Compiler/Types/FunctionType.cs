using System;
using System.Collections.Generic;
using System.IO;
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
            get { return Parameters?.ConvertAll<Types.Type>(s => s.Type); }
        }

        /// <summary>
        /// The returned type.
        /// </summary>
        public Type ReturnType => ReturnSymbol?.Type;

        /// <summary>
        /// Dump this type in the given TextWriter instance
        /// </summary>
        /// <param name="tw"></param>
        /// <param name="indentLevel"></param>
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            if (Parameters != null)
            {
                tw.WriteLine();
                tw.Write(s);
                foreach (var p in Parameters)
                {
                    if (p.HasFlag(Symbol.Flags.Input))
                        tw.Write("INPUT ");
                    else if (p.HasFlag(Symbol.Flags.Output))
                        tw.Write("OUTPUT ");
                    else if (p.HasFlag(Symbol.Flags.Inout))
                        tw.Write("INOUT ");
                    p.Dump(tw, 0);
                }
            }
            tw.WriteLine();
            ReturnSymbol?.Dump(tw, indentLevel);
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitFunctionType(this, s); }
    }
}
