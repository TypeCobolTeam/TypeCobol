using JetBrains.Annotations;
using TypeCobol.Compiler.Domain.Validator;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Given a variable symbol, this class renumbers all levels except levels 66, 77 and 88.
    /// By default, the renumbering starts at the original level value (and increases by 1 at each level transition).
    /// </summary>
    public class LevelRenumber : AbstractSymbolAndTypeVisitor<object, LevelRenumber.Context>
    {
        public class Context
        {
            /// <summary>
            /// Keeps track of the current target level.
            /// </summary>
            public int TargetLevel { get; set; }

            /// <summary>
            /// Allows external error report.
            /// </summary>
            public IValidationErrorReporter ErrorReporter { get; }

            public Context(int initialLevel, IValidationErrorReporter errorReporter)
            {
                TargetLevel = initialLevel;
                ErrorReporter = errorReporter;
            }
        }

        #region Symbols renumbering

        private void Renumber(VariableSymbol variable, Context context)
        {
            int targetLevel = context.TargetLevel;
            if (targetLevel <= 49)
            {
                //Change the level for real.
                variable.Level = targetLevel;
            }
            else
            {
                //Report an error
                context.ErrorReporter?.Report(new ValidationError(variable, string.Format(TypeCobolResource.LevelExceededDuringRenumber, variable.Name, targetLevel)));
            }
        }

        public override object VisitSymbol(Symbol symbol, Context context)
        {
            //Do nothing.
            return null;
        }

        public override object VisitVariableSymbol(VariableSymbol variable, Context context)
        {
            System.Diagnostics.Debug.Assert(context.TargetLevel > 0);

            if (!variable.HasFlag(Symbol.Flags.SymbolExpanded))
            {
                //Do not attempt renumbering of a non-expanded variable.
                context.ErrorReporter?.Report(new ValidationError(variable, string.Format(TypeCobolResource.RenumberingNonExpandedVariable, variable.Name)));
            }
            else
            {
                switch (variable.Level)
                {
                    case 66:
                    case 77:
                    case 88:
                        //Do not renumber
                        break;
                    default:
                        Renumber(variable, context);
                        break;
                }

                //Continue visit through variable's type.
                variable.Type?.Accept(this, context);
            }

            return null;
        }

        public override object VisitVariableTypeSymbol(VariableTypeSymbol typedVariable, Context context)
        {
            VisitVariableSymbol(typedVariable, context);
            return null;
        }

        #endregion

        #region Types renumbering

        public override object VisitType(Type type, Context context)
        {
            //Continue visit through type component.
            type.TypeComponent?.Accept(this, context);
            return null;
        }

        public override object VisitGroupType(GroupType group, Context context)
        {
            //Continue visit through fields.
            context.TargetLevel++;
            foreach (var field in group.Fields)
            {
                field.Accept(this, context);
            }
            context.TargetLevel--;

            return null;
        }

        public override object VisitTypedefType(TypedefType typedef, Context context)
        {
            //Protection against cyclic typedefs.
            if (typedef.HasFlag(Symbol.Flags.CheckedForCycles))
            {
                if (typedef.HasFlag(Symbol.Flags.IsCyclic))
                {
                    context.ErrorReporter?.Report(new ValidationError(typedef.Symbol, string.Format(TypeCobolResource.RenumberingCyclicType, typedef.Symbol.Name)));
                }
                else
                {
                    VisitType(typedef, context);
                }
            }
            else
            {
                context.ErrorReporter?.Report(new ValidationError(typedef.Symbol, string.Format(TypeCobolResource.RenumberingUnsafeType, typedef.Symbol.Name)));
            }

            return null;
        }

        #endregion

        /// <summary>
        /// Renumbers the given variable starting at its original level value.
        /// </summary>
        /// <param name="variable">Variable to renumber.</param>
        /// <param name="errorReporter">Custom error reporter.</param>
        public void Renumber([NotNull] VariableSymbol variable, IValidationErrorReporter errorReporter = null)
        {
            System.Diagnostics.Debug.Assert(variable != null);
            var context = new Context(variable.Level, errorReporter);
            variable.Accept(this, context);
        }
    }
}
