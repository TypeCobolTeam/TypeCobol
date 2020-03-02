using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Domain.Validator;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    public class ProgramExpander
    {
        /// <summary>
        /// Go through a symbol to expand it.
        /// </summary>
        private class SymbolExpander : Symbol.AbstractSymbolVisitor<Symbol, object>
        {
            private readonly ProgramExpander _parentExpander;
            private readonly LevelRenumber _levelRenumber;

            public SymbolExpander(ProgramExpander parentExpander)
            {
                _parentExpander = parentExpander;
                _levelRenumber = new LevelRenumber();
            }

            /// <summary>
            /// Expand all variables within a scope.
            /// </summary>
            /// <typeparam name="TSymbol">Type of symbols in the scope.</typeparam>
            /// <param name="scope">Scope to expand.</param>
            private void ExpandScope<TSymbol>(Scope<TSymbol> scope) 
                where TSymbol : Symbol
            {
                foreach (var symbol in scope)
                {
                    Symbol newSymbol = symbol.Accept(this, null);
                    //We don't support symbol renaming with a scope.
                    System.Diagnostics.Debug.Assert(newSymbol == symbol);
                }
            }

            /// <summary>
            /// Performs a type expansion for the supplied variable.
            /// </summary>
            /// <param name="variable">Variable whose type needs to be expanded.</param>
            /// <param name="type">Type to expand.</param>
            private void ExpandType(VariableSymbol variable, Type type)
            {
                //Forward expansion to the type expander using the variable as the owner !
                Type newType = type.Accept(_parentExpander._typeExpander, variable);
                if (newType != type)
                {
                    //The type has changed : update variable type and perform level renumbering
                    variable.Type = newType;
                    variable.SetFlag(Symbol.Flags.SymbolExpanded, true, true);
                    _levelRenumber.Renumber(variable, _parentExpander._errorReporter);
                }
            }

            public override Symbol VisitSymbol(Symbol symbol, object _)
            {
                //Default behavior, do nothing.
                return symbol;
            }

            public override Symbol VisitProgramSymbol(ProgramSymbol programToExpand, object _)
            {
                //No need to expand the Program if it has already been done.
                if (programToExpand.HasFlag(Symbol.Flags.SymbolExpanded))
                    return programToExpand;

                //Save current expanding program and swap with the new one
                ProgramSymbol previousExpandingProgram = _parentExpander._currentExpandingProgram;
                _parentExpander._currentExpandingProgram = programToExpand;

                try
                {
                    //Check each storage sections
                    ExpandScope(programToExpand.FileData);
                    ExpandScope(programToExpand.GlobalStorageData);
                    ExpandScope(programToExpand.WorkingStorageData);
                    ExpandScope(programToExpand.LocalStorageData);
                    ExpandScope(programToExpand.LinkageStorageData);

                    //Check each Nested Programs
                    ExpandScope(programToExpand.Programs);

                    //Check each procedure
                    ExpandScope(programToExpand.Functions);
                }
                finally
                {
                    //Mark this program has being expanded
                    programToExpand.SetFlag(Symbol.Flags.SymbolExpanded, true);

                    //Restore original expanding program
                    _parentExpander._currentExpandingProgram = previousExpandingProgram;
                }

                return programToExpand;
            }

            public override Symbol VisitFunctionSymbol(FunctionSymbol function, object _)
            {
                return VisitProgramSymbol(function, null);
            }

            public override Symbol VisitVariableSymbol(VariableSymbol variable, object _)
            {
                if (variable.Type?.MayExpand ?? false)
                {
                    ExpandType(variable, variable.Type);
                }

                return variable;
            }

            public override Symbol VisitVariableTypeSymbol(VariableTypeSymbol typedVariable, object _)
            {
                if (typedVariable.Type != null)
                {
                    ExpandType(typedVariable, typedVariable.Type);
                }

                return typedVariable;
            }

            public override Symbol VisitRedefinesSymbol(RedefinesSymbol redefines, object _)
            {
                return VisitVariableSymbol(redefines, null);
            }
        }

        /// <summary>
        /// Go through a type to expand it.
        /// </summary>
        private class TypeExpander : Type.AbstractTypeVisitor<Type, Symbol>
        {
            private readonly ProgramExpander _parentExpander;
            private readonly CyclicTypeChecker _cyclicTypeChecker;

            public TypeExpander(ProgramExpander parentExpander)
            {
                _parentExpander = parentExpander;
                _cyclicTypeChecker = new CyclicTypeChecker();
            }

            public override Type VisitType(Type type, Symbol owner)
            {
                //Default behavior, do nothing.
                return type;
            }

            public override Type VisitArrayType(ArrayType array, Symbol owner)
            {
                if (!array.MayExpand)
                {
                    return array;
                }

                ArrayType newType = (ArrayType) array.Clone();
                newType.ElementType = array.ElementType.Accept(this, owner);
                return newType;
            }

            public override Type VisitPointerType(PointerType pointer, Symbol owner)
            {
                if (!pointer.MayExpand)
                {
                    return pointer;
                }

                PointerType newType = (PointerType) pointer.Clone();
                newType.ElementType = pointer.ElementType.Accept(this, owner);
                return newType;
            }

            /// <summary>
            /// A Group Type is cloned with a new Scope and new fresh fields created for fields coming from a TypeDef.
            /// </summary>
            /// <param name="group">The Group type to be cloned.</param>
            /// <param name="owner">The current owner symbol.</param>
            /// <returns>The new cloned Group type.</returns>
            public override Type VisitGroupType(GroupType group, Symbol owner)
            {
                GroupType newType = (GroupType) group.Clone();
                newType.Scope = new Scope<VariableSymbol>(owner);
                foreach (var field in group.Scope)
                {
                    //Clone only variables that are inside a TYPEDEF
                    bool isInTypedef = field.HasFlag(Symbol.Flags.InsideTypedef);
                    VariableSymbol newField = isInTypedef ? (VariableSymbol) field.Clone() : field;
                    if (isInTypedef)
                    {
                        newField.GlobalIndex = 0;
                    }

                    //Normalize the new field
                    newField.NormalizeExpandedSymbol(newType.Scope);
                    newField.Accept(_parentExpander._symbolExpander, null);

#if !DOMAIN_CHECKER
                    System.Diagnostics.Debug.Assert(newField.Type != null);
#endif

                    newType.Scope.Enter(newField);
                    //Set the new owner
                    newField.Owner = owner;
                    if (isInTypedef)
                    {
                        //Important add to the domain the new field that was cloned.
                        _parentExpander._currentExpandingProgram.AddToDomain(newField);
                    }
                }
                return newType;
            }

            /// <summary>
            /// A typedef is expanded through its TypeComponent.
            /// This method also checks for cyclic type definitions.
            /// </summary>
            /// <param name="typedef">Typedef to expand.</param>
            /// <param name="owner">The current owner symbol.</param>
            /// <returns>The expanded type.</returns>
            public override Type VisitTypedefType(TypedefType typedef, Symbol owner)
            {
                //Check first for cyclic definition.
                if (_cyclicTypeChecker.IsCyclic(typedef, out var firstCycleFound))
                {
                    //Report error, no expansion performed.
                    var detail = firstCycleFound != null
                        ? string.Join(" -> ", firstCycleFound.Select(t => t.Symbol.Name))
                        : typedef.Symbol.Name;
                    _parentExpander._errorReporter?.Report(new ValidationError(typedef.Symbol, string.Format(TypeCobolResource.ExpandingCyclicType, detail)));
                    return typedef;
                }

                //Continue expansion through the TargetType.
                return typedef.TargetType.Accept(this, owner);
            }
        }

        private ProgramSymbol _currentExpandingProgram;
        private readonly SymbolExpander _symbolExpander;
        private readonly TypeExpander _typeExpander;
        private readonly IValidationErrorReporter _errorReporter;

        /// <summary>
        /// Creates a new ProgramExpander.
        /// </summary>
        /// <param name="errorReporter">Custom error reporter.</param>
        public ProgramExpander(IValidationErrorReporter errorReporter)
        {
            _currentExpandingProgram = null;
            _symbolExpander = new SymbolExpander(this);
            _typeExpander = new TypeExpander(this);
            _errorReporter = errorReporter;
        }

        /// <summary>
        /// Performs types and symbols expansion on the given program.
        /// </summary>
        /// <param name="programToExpand">Program to expand.</param>
        public void Expand([NotNull] ProgramSymbol programToExpand)
        {
            System.Diagnostics.Debug.Assert(programToExpand != null);
            programToExpand.Accept(_symbolExpander, null);
        }
    }
}
