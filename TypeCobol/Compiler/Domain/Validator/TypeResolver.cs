using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Domain.Validator
{
    /// <summary>
    /// Symbol's Type resolver for a program. Only VariableTypeSymbol instance has to be taken in account.
    /// </summary>
    public class TypeResolver
    {
        private class Validator : AbstractSymbolAndTypeVisitor<bool, RootSymbolTable>
        {
            public List<Symbol> UnvalidatedSymbols { get; }
            public List<Type> UnvalidatedTypes { get; }

            public Validator()
            {
                UnvalidatedSymbols = new List<Symbol>();
                UnvalidatedTypes = new List<Type>();
            }

            /// <summary>
            /// Validates all variables within a scope.
            /// </summary>
            /// <param name="scope">Scope to validate.</param>
            /// <param name="context">RootSymbolTable used to resolve types.</param>
            /// <returns>True if successfully validated, False otherwise.</returns>
            private bool ValidateScope<TSymbol>(Scope<TSymbol> scope, RootSymbolTable context)
                where TSymbol : Symbol
            {
                bool result = true;
                foreach (var symbol in scope)
                {
                    if (!symbol.Accept(this, context)) result = false;
                }
                return result;
            }

            #region Symbol visit

            public override bool VisitSymbol(Symbol symbol, RootSymbolTable context)
            {
                if (symbol.Type == null || !symbol.Type.Accept(this, context))
                {
                    UnvalidatedSymbols.Add(symbol);
                    return false;
                }
                return true;
            }

            public override bool VisitProgramSymbol(ProgramSymbol program, RootSymbolTable context)
            {
                //Check all Types
                bool result = ValidateScope(program.Types, context);

                //Check each storage sections
                if (!ValidateScope(program.FileData, context)) result = false;
                if (!ValidateScope(program.GlobalStorageData, context)) result = false;
                if (!ValidateScope(program.WorkingStorageData, context)) result = false;
                if (!ValidateScope(program.LocalStorageData, context)) result = false;
                if (!ValidateScope(program.LinkageStorageData, context)) result = false;

                //Visit Paragraph/section
                if (!ValidateScope(program.Paragraphs, context)) result = false;
                if (!ValidateScope(program.Sections, context)) result = false;

                //Check each Nested Programs
                if (!ValidateScope(program.Programs, context)) result = false;

                //Check each procedure
                if (!ValidateScope(program.Functions, context)) result = false;

                return result;
            }

            public override bool VisitFunctionSymbol(FunctionSymbol function, RootSymbolTable context)
            {
                return VisitProgramSymbol(function, context);
            }

            public override bool VisitNamespaceSymbol(NamespaceSymbol @namespace, RootSymbolTable context)
            {
                bool result = ValidateScope(@namespace.Types, context);
                if (!ValidateScope(@namespace.Programs, context)) result = false;
                if (!ValidateScope(@namespace.Namespaces, context)) result = false;
                return result;
            }

            public override bool VisitParagraphSymbol(ParagraphSymbol paragraph, RootSymbolTable context) { return true; }

            public override bool VisitSectionSymbol(SectionSymbol section, RootSymbolTable context) { return true; }

            public override bool VisitRedefinesSymbol(RedefinesSymbol redefines, RootSymbolTable context)
            {
                bool result = VisitSymbol(redefines, context);
                if (redefines.Redefined == null && result)
                {
                    UnvalidatedSymbols.Add(redefines);
                    result = false;//No Symbol Redefined
                }
                return result;
            }

            public override bool VisitVariableTypeSymbol(VariableTypeSymbol typedVariable, RootSymbolTable context)
            {
                /* TypeCompletion not required anymore
                if (!typedVariable.TypeCompleter(context))
                {
                    UnvalidatedSymbols.Add(typedVariable);
                }
                */
                return true; //We return true because we want to do that for all declarations.        
            }

            #endregion

            #region Type visit

            public override bool VisitType(Type type, RootSymbolTable context)
            {
                return true;
            }

            public override bool VisitArrayType(ArrayType array, RootSymbolTable context)
            {
                if (!(array.ElementType?.Accept(this, context) ?? false))
                {
                    UnvalidatedTypes.Add(array);
                    return false;
                }
                return true;
            }
            public override bool VisitPointerType(PointerType pointer, RootSymbolTable context)
            {
                if (!(pointer.ElementType?.Accept(this, context) ?? false))
                {
                    UnvalidatedTypes.Add(pointer);
                    return false;
                }
                return true;
            }

            public override bool VisitGroupType(GroupType group, RootSymbolTable context)
            {
                bool result = group.LeadingType?.Accept(this, context) ?? true;
                foreach (var s in group.Scope)
                {
                    if (!s.Accept(this, context)) result = false;
                }

                if (!result)
                {
                    UnvalidatedTypes.Add(group);
                }
                return result;
            }

            public override bool VisitRenamesType(RenamesType renames, RootSymbolTable context)
            {
                bool result = VisitGroupType(renames, context);
                if (result && (renames.DataName1 == null || renames.DataName2 == null))
                {
                    UnvalidatedTypes.Add(renames);
                    result = false;
                }
                return result;
            }

            public override bool VisitTypedefType(TypedefType typedef, RootSymbolTable context)
            {
                if (!(typedef.TargetType?.Accept(this, context) ?? false))
                {
                    UnvalidatedTypes.Add(typedef);
                    return false;
                }
                return true;
            }

            #endregion
        }

        private readonly RootSymbolTable _rootSymbolTable;

        public TypeResolver([NotNull] RootSymbolTable rootSymbolTable)
        {
            System.Diagnostics.Debug.Assert(rootSymbolTable != null);
            _rootSymbolTable = rootSymbolTable;
        }

        public bool ResolveTypes([NotNull] ProgramSymbol program, out List<Symbol> unvalidatedSymbols, out List<Type> unvalidatedTypes)
        {
            System.Diagnostics.Debug.Assert(program != null);
            Validator validator = new Validator();
            bool result = program.Accept(validator, _rootSymbolTable);
            unvalidatedSymbols = validator.UnvalidatedSymbols;
            unvalidatedTypes = validator.UnvalidatedTypes;
            return result;
        }
    }
}
