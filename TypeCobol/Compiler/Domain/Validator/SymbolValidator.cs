using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Domain.Validator
{
    /// <summary>
    /// Symbol Validator.
    /// </summary>
    public class SymbolValidator : Symbol.AbstractSymbolVisitor<bool, Object>
    {
        /// <summary>
        /// List of validated symbol
        /// </summary>
        public List<Symbol> Unvalidated { get; set; }

        /// <summary>
        /// The TypeValidator instance that is used.
        /// </summary>
        public TypeValidator TypValidator { get; internal set; }

        /// <summary>
        /// Constructor based on a TypeValidator.
        /// </summary>
        public SymbolValidator() : this(new TypeValidator())
        {
            TypValidator.SymValidator = this;
        }

        /// <summary>
        /// TypeValidator Constructor
        /// </summary>
        public SymbolValidator(TypeValidator typeValidator)
        {
            Unvalidated = new List<Symbol>();
            TypValidator = typeValidator ?? new TypeValidator(this);
        }

        /// <summary>
        /// Default implementation we only check that the type isn't null.
        /// </summary>
        /// <param name="s">The symbol to check</param>
        /// <param name="arg">Any argument</param>
        /// <returns></returns>
        public override bool VisitSymbol(Symbol s, object arg)
        {
            if (s.Type == null)
            {
                Unvalidated.Add(s);
                return false;
            }
            else if (!s.Type.Accept(TypValidator, arg))
            {
                Unvalidated.Add(s);
                return false;
            }
            return true;
        }

        /// <summary>
        /// Validates all variables within a scope.
        /// </summary>
        /// <param name="scope"></param>
        /// <param name="arg"></param>
        private bool ValidateScope<T>(Scopes.Scope<T> scope, object arg) where T : Symbol
        {
            bool bResult = true;
            foreach (var sym in scope)
            {
                if (!sym.Accept(this, arg)) bResult = false;
            }

            return bResult;
        }

        public override bool VisitProgramSymbol(ProgramSymbol s, object arg)
        {
            bool bResult = false;
            if (VisitSymbol(s, arg))
            {
                bResult = true;
                //Check all Types
                if (!ValidateScope(s.Types, arg)) bResult = false;

                //Check each storage sections
                if (!ValidateScope<VariableSymbol>(s.FileData, arg)) bResult = false;
                if (!ValidateScope<VariableSymbol>(s.GlobalStorageData, arg)) bResult = false;
                if (!ValidateScope<VariableSymbol>(s.WorkingStorageData, arg)) bResult = false;
                if (!ValidateScope<VariableSymbol>(s.LinkageStorageData, arg)) bResult = false;

                //Visit Paragraph/section
                if (!ValidateScope(s.Paragraphs, arg)) bResult = false;
                if (!ValidateScope(s.Sections, arg)) bResult = false;

                //Check each Nested Programs
                if (!ValidateScope<ProgramSymbol>(s.Programs, arg)) bResult = false;

                //Check each procedure
                if (!ValidateScope<FunctionSymbol>(s.Functions, arg)) bResult = false;
            }

            return bResult;
        }

        public override bool VisitFunctionSymbol(FunctionSymbol s, object arg)
        {            
            return VisitProgramSymbol((ProgramSymbol) s, arg);
        }

        public override bool VisitNamespaceSymbol(NamespaceSymbol s, object arg)
        {
            bool bResult = ValidateScope<TypedefSymbol>(s.Types, arg);
            if (!ValidateScope<ProgramSymbol>(s.Programs, arg)) bResult = false;
            if (!ValidateScope<NamespaceSymbol>(s.Namespaces, arg)) bResult = false;            
            return bResult; 

        }
        public override bool VisitParagraphSymbol(ParagraphSymbol s, object arg) { return true; }
        public override bool VisitSectionSymbol(SectionSymbol s, object arg) { return true; }

        public override bool VisitRedefinesSymbol(RedefinesSymbol s, object arg)
        {
            bool bResult = VisitSymbol(s, arg);
            if (s.Redefined == null && bResult)
            {
                Unvalidated.Add(s);
                bResult = false;//No Symbol Redefined
            }
            return bResult;
        }
    }
}
