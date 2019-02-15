using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Types;
using Type = System.Type;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Symbol Expander
    /// </summary>
    public class SymbolExpander : Symbol.AbstractSymbolVisitor<Symbol, Symbol>
    {
        public ProgramSymbol Program
        {
            get;
            private set;
        }

        /// <summary>
        /// The Type Expander to use.
        /// </summary>
        public TypedefExpander TypExpander
        {
            get;
            internal set;
        }

        /// <summary>
        /// This will be an indication if the Symbol shall be level renumbered.
        /// If this Symbol Expander is used for several symbol, it will be necessary for each
        /// Acceptation to reset this variable to false before. 
        /// </summary>
        public bool ShallLeverRenumber
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="program">The program requesting the expansion</param>
        public SymbolExpander(ProgramSymbol program) : this(program, new TypedefExpander(program))
        {
            TypExpander.SymExpander = this;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="program">The program requesting the expansion</param>
        /// <param name="typeExpander">The Type Expander to be used.</param>
        public SymbolExpander(ProgramSymbol program, TypedefExpander typeExpander)
        {
            this.Program = program;
            TypExpander = typeExpander ?? new TypedefExpander(program, this);
        }

        public override Symbol VisitSymbol(Symbol s, Symbol owner)
        {
            return s;
        }

        /// <summary>
        /// Expand all variables within a scope.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="owner"></param>
        private void ExpandScope<T>(Scopes.Scope<T> scope, Symbol owner) where T : Symbol
        {
            foreach (var sym in scope)
            {
                Symbol newSym = sym.Accept(this, owner);
                //We don't support symbol renaming with a scope.
                System.Diagnostics.Debug.Assert(newSym == sym);
            }
        }

        public override Symbol VisitProgramSymbol(ProgramSymbol s, Symbol owner)
        {
            //We don't expand TypeSymbol.
            //Check each storage sections
            ExpandScope<VariableSymbol>(s.FileData, s);
            ExpandScope<VariableSymbol>(s.GlobalStorageData, s);
            ExpandScope<VariableSymbol>(s.WorkingStorageData, s);
            ExpandScope<VariableSymbol>(s.LocalStorageData, s);
            ExpandScope<VariableSymbol>(s.LinkageStorageData, s);

            //Check each Nested Programs
            ExpandScope<ProgramSymbol>(s.Programs, s);

            //Check each procedure
            ExpandScope<FunctionSymbol>(s.Functions, s);
            return s;
        }

        public override Symbol VisitFunctionSymbol(FunctionSymbol s, Symbol owner)
        {
            return VisitProgramSymbol((ProgramSymbol)s, owner);
        }

        public override Symbol VisitVariableSymbol(VariableSymbol s, Symbol owner)
        {
            if (s.Type?.MayExpand ?? false)
            {//Expand thru the Type Maybe a Record or an ArrayType.
                Types.Type newType = s.Type.Accept(TypExpander, s);
                if (newType != s.Type)
                {//The type has changed
                    s.Type = newType;
                    ShallLeverRenumber = true;
                }
            }

            return s;
        }

        /// <summary>
        /// Only VariableTypeSymbol are candidate to expansion.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="owner"></param>
        /// <returns></returns>
        public override Symbol VisitVariableTypeSymbol(VariableTypeSymbol s, Symbol owner)
        {
            Types.Type type = s.Type;
            if (type != null)
            {
                Types.Type newType = type.Accept(TypExpander, s);
                if (newType != type)
                {//The type has changed
                    s.Type = newType;
                    ShallLeverRenumber = true;
                }
            }
            return s;
        }
    }
}
