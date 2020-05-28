using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Domain.Validator;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Cyclic Type Checker : determine if a type is cyclic.
    /// A cyclic type cannot be traversed by regular type visitors, so this visitor has to be called first.
    /// Other type visitors can then use flags <see cref="Symbol.Flags.CheckedForCycles" /> and <see cref="Symbol.Flags.IsCyclic" />
    /// to adapt their behavior.
    /// </summary>
    public class CyclicTypeChecker : Type.AbstractTypeVisitor<object, IValidationErrorReporter>
    {
        private readonly Stack<TypedefType> _typedefStack;

        public CyclicTypeChecker()
        {
            _typedefStack = new Stack<TypedefType>();
        }

        public override object VisitType(Type type, IValidationErrorReporter errorReporter)
        { 
            //Continue visit through TypeComponent if any.
            type.TypeComponent?.Accept(this, errorReporter);
            return null;
        }

        public override object VisitGroupType(GroupType groupType, IValidationErrorReporter errorReporter)
        {
            //Continue visit through fields
            foreach (var field in groupType.Fields)
            {
                field.Type?.Accept(this, errorReporter);
            }

            return null;
        }

        public override object VisitTypedefType(TypedefType typedefType, IValidationErrorReporter errorReporter)
        {
            if (typedefType.HasFlag(Symbol.Flags.CheckedForCycles))
            {
                //Type has already been checked but we have to update the stack if the current type is actually cyclic.
                if (typedefType.HasFlag(Symbol.Flags.IsCyclic))
                {
                    FlagParentsAsCyclic();
                }
                return null;
            }

            if (_typedefStack.Contains(typedefType))
            {
                //Type is cyclic : flag it, report error with cycle and mark all visited parent types as cyclic.
                typedefType.SetFlag(Symbol.Flags.IsCyclic, true, false);
                if (errorReporter != null)
                {
                    //Report error with first cycle found during visit.
                    var cycle = _typedefStack.Reverse().SkipWhile(typedef => typedef != typedefType).ToList();
                    cycle.Add(typedefType);
                    var chain = string.Join(" -> ", cycle.Select(t => t.Symbol.Name));
                    errorReporter.Report(new ValidationError(typedefType.Symbol, string.Format(TypeCobolResource.CyclicTypeDetected, chain)));
                }
                FlagParentsAsCyclic();
            }
            else
            {
                //Continue visit through TargetType.
                _typedefStack.Push(typedefType);
                typedefType.TargetType.Accept(this, errorReporter);
                _typedefStack.Pop();
            }

            //typedef has been fully checked.
            typedefType.SetFlag(Symbol.Flags.CheckedForCycles, true, false);
            return null;

            //Local function to update parent typedefs and mark them as cyclic.
            void FlagParentsAsCyclic()
            {
                foreach (var visitedTypedef in _typedefStack.Where(typedef => typedef != typedefType))
                {
                    if (visitedTypedef.HasFlag(Symbol.Flags.IsCyclic)) continue;

                    visitedTypedef.SetFlag(Symbol.Flags.IsCyclic, true, false);
                    errorReporter?.Report(new ValidationError(visitedTypedef.Symbol, string.Format(TypeCobolResource.DependsOnCyclicType, visitedTypedef.Symbol.Name, typedefType.Symbol.Name)));
                }
            }
        }

        /// <summary>
        /// Check for cycles the given Typedef.
        /// </summary>
        /// <param name="typedef">Typedef instance to be checked.</param>
        /// <param name="errorReporter">Custom error reporter.</param>
        /// <returns>True if the given typedef is cyclic, False otherwise.</returns>
        public bool Check([NotNull] TypedefType typedef, IValidationErrorReporter errorReporter = null)
        {
            typedef.Accept(this, errorReporter);
            return typedef.HasFlag(Symbol.Flags.IsCyclic);
        }
    }
}
