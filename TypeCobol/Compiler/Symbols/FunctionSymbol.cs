using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.Types;

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
        /// <remarks>do not forget to set FunctionType after calling this.</remarks>
        public FunctionSymbol(string name) : base(name)
        {
            //Override the Kind here.
            base.Kind = Kinds.Function;
        }

        /// <summary>
        /// Function parameters.
        /// </summary>
        public List<VariableSymbol> Parameters => FunctionType?.Parameters;

        /// <summary>
        /// variable symbol
        /// </summary>
        public VariableSymbol ReturnSymbol => FunctionType?.ReturnSymbol;

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
            set => base.Type = value;
        }

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

        /// <summary>
        /// Function are not considered of Nested Programs.
        /// </summary>
        public override bool IsNested => false;

        /// <summary>
        /// Get the Variable visibility mask.
        /// </summary>
        public override Flags VariableVisibilityMask => Flags.GLOBAL_STORAGE;

        /// <summary>
        /// Get the type visibility mask for a procedure.
        /// </summary>
        public override Flags TypeVisibilityMask => Flags.Private | Flags.Public;

        /// <summary>
        /// Get the function visibility mask for a Procedure.
        /// </summary>
        public override Flags FunctionVisibilityMask => Flags.Private | Flags.Public;

        /// <summary>
        /// Dump this symbol in the given TextWriter instance
        /// </summary>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">Indentation level</param>
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write(s);
            tw.Write("DECLARE PROCEDURE ");
            tw.Write(Name);
            tw.Write(". ");
            DumpSymbolFlags(Flag, tw);
            tw.WriteLine();
            tw.Write(s);
            tw.Write("PROCEDURE DIVISION");
            this.Type?.Dump(tw, indentLevel + 1);
            tw.WriteLine();
            tw.Write("  ");
            tw.Write(s);
            tw.Write('.');
            tw.WriteLine();
            tw.Write(s);
            tw.Write("END-DECLARE");
            tw.Write(".");
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitFunctionSymbol(this, arg);
        }
    }
}
