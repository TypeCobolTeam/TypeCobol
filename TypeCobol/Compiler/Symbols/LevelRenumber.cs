using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// Given a variable symbol, this class renumbers all levels except levels 66, 77 and 88.
    /// The renumbering starts at the original level value and increases by 1 at each level transition.
    /// </summary>
    public class LevelRenumber
    {
        private class RenumberingContext
        {
            /// <summary>
            /// Keeps track of the current target level.
            /// </summary>
            public int TargetLevel { get; set; }

            /// <summary>
            /// Stores variables that would get an invalid level (> 49) if renumbering would actually be applied.
            /// </summary>
            public Dictionary<VariableSymbol, int> ExcessiveLevelVariables { get; set; }
        }

        private class Visitor : AbstractSymbolAndTypeVisitor<bool, RenumberingContext>
        {
            #region Symbols renumbering

            private bool Renumber(VariableSymbol variable, RenumberingContext context)
            {
                int targetLevel = context.TargetLevel;
                if (targetLevel <= 49)
                {
                    //Change the level for real
                    variable.Level = targetLevel;
                    return true;
                }

                //Memorize excessive level in dictionary
                context.ExcessiveLevelVariables?.Add(variable, targetLevel);
                return false;
            }

            public override bool VisitSymbol(Symbol symbol, RenumberingContext renumberingContext)
            {
                //Do nothing.
                return true;
            }

            public override bool VisitVariableSymbol(VariableSymbol variable, RenumberingContext context)
            {
                System.Diagnostics.Debug.Assert(context.TargetLevel > 0);
                bool result;
                switch (variable.Level)
                {
                    case 66:
                    case 77:
                    case 88:
                        //Do not renumber
                        result = true;
                        break;
                    default:
                        result = Renumber(variable, context);
                        break;
                }

                //Continue visit through variable type
                return (variable.Type?.Accept(this, context) ?? true) && result;
            }

            public override bool VisitVariableTypeSymbol(VariableTypeSymbol typedVariable, RenumberingContext context)
            {
                return VisitVariableSymbol(typedVariable, context);
            }

            #endregion

            #region Types renumbering

            public override bool VisitType(Type type, RenumberingContext renumberingContext)
            {
                //Continue visit through type component.
                return type.TypeComponent?.Accept(this, renumberingContext) ?? true;
            }

            public override bool VisitGroupType(GroupType group, RenumberingContext context)
            {
                bool result = true;

                //Continue visit through fields.
                context.TargetLevel++;
                foreach (var field in group.Fields)
                {
                    if (!field.Accept(this, context))
                    {
                        result = false;
                    }
                }
                context.TargetLevel--;

                return result;
            }

            public override bool VisitTypedefType(TypedefType typedef, RenumberingContext context)
            {
                //Protection against cyclic typedefs.

                if (!typedef.HasFlag(Symbol.Flags.CheckedForCycles))
                {
                    throw new System.InvalidOperationException($"Typedef '{typedef.Symbol.Name}' must be checked for cycles before attempting level renumbering.");
                }

                if (typedef.HasFlag(Symbol.Flags.IsCyclic))
                {
                    throw new System.NotSupportedException($"Typedef '{typedef.Symbol.Name}' is cyclic, level renumbering cannot be performed.");
                }

                return VisitType(typedef, context);
            }

            #endregion
        }

        private readonly Visitor _visitor;

        public LevelRenumber()
        {
            _visitor = new Visitor();
        }

        /// <summary>
        /// Attempts to renumber the given variable. Renumbering may fail on excessive levels reached because of expanded type variables.
        /// Caller can request a list of variables that failed to renumber by passing a non-null Dictionary as second parameter.
        /// </summary>
        /// <param name="variable">Variable to renumber, must be expanded first.</param>
        /// <param name="excessiveLevelVariables">Dictionary of failed variables associated with its target level (beyond 49).
        /// Pass a non-null instance to have it populated.</param>
        /// <returns>True if renumbering fully succeeded, False if at least one variable got an excessive level.</returns>
        public bool TryRenumber([NotNull] VariableSymbol variable, Dictionary<VariableSymbol, int> excessiveLevelVariables)
        {
            System.Diagnostics.Debug.Assert(variable != null);

            if (!variable.HasFlag(Symbol.Flags.SymbolExpanded))
            {
                throw new System.InvalidOperationException("Level renumbering is allowed only on expanded variables.");
            }

            var context = new RenumberingContext()
                          {
                              TargetLevel = variable.Level,
                              ExcessiveLevelVariables = excessiveLevelVariables
                          };
            return variable.Accept(_visitor, context);
        }
    }
}
