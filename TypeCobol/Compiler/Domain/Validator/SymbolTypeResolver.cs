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
        public SymbolTypeResolver(TypeCobol.Compiler.Scopes.RootSymbolTable root = null)
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
            return true;//We return true because we want to do that for all declarations.        
        }
    }
}
