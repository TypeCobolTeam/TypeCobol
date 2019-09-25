using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Domain.Validator
{
    /// <summary>
    /// Symbol's Type resolver for a program. Only VariableTypeSymbol instance has to be taken in account.
    /// </summary>
    public class SymbolTypeResolver : SymbolValidator
    {
        /// <summary>
        /// The Current Program
        /// </summary>
        private ProgramSymbol CurrentProgram
        {
            get;
            set;
        }

        /// <summary>
        /// The Root Symbol Table instance.
        /// </summary>
        private TypeCobol.Compiler.Scopes.RootSymbolTable Root
        {
            get;
            set;
        }
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="root">The Root Symbol table</param>
        public SymbolTypeResolver(TypeCobol.Compiler.Scopes.RootSymbolTable root)
        {
            System.Diagnostics.Debug.Assert(root != null);
            Root = root;
        }
        public override bool VisitVariableTypeSymbol(VariableTypeSymbol s, object arg)
        {
            if (!s.TypeCompleter(Root))
            {
                base.Unvalidated.Add(s);
            }
            return true;//We return true because we wont to do that for all declarations.        
        }

        /// <summary>
        /// Specialization to keep the current program being visited.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="arg"></param>
        /// <returns></returns>
        public override bool VisitProgramSymbol(ProgramSymbol s, object arg)
        {
            ProgramSymbol saveCurrentProgram = CurrentProgram;
            try
            {                
                this.CurrentProgram = s;
                return base.VisitProgramSymbol(s, arg);
            }
            finally
            {
                CurrentProgram = saveCurrentProgram;
            }
        }
    }
}
