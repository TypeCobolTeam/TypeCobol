using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Cyclic Type Checker : determine if a type is cyclic.
    /// A cyclic type cannot be traversed by regular type visitors, so this visitor has to be called first.
    /// Other type visitors can then use flags <see cref="Symbol.Flags.CheckedForCycles" /> and <see cref="Symbol.Flags.IsCyclic" />
    /// to adapt their behavior.
    /// </summary>
    public class CyclicTypeChecker : Type.AbstractTypeVisitor<object, object>
    {
        private readonly Stack<TypedefType> _typedefStack;

        public CyclicTypeChecker()
        {
            _typedefStack = new Stack<TypedefType>();
        }

        public override object VisitType(Type type, object _)
        { 
            //Continue visit through TypeComponent if any.
            type.TypeComponent?.Accept(this, null);
            return null;
        }

        public override object VisitGroupType(GroupType groupType, object _)
        {
            //Continue visit through fields
            foreach (var field in groupType.Fields)
            {
                field.Type?.Accept(this, null);
            }

            return null;
        }

        public override object VisitTypedefType(TypedefType typedefType, object _)
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
                //Type is cyclic : mark all parent typedefs as cyclic (that includes current type).
                FlagParentsAsCyclic();
            }
            else
            {
                //Continue visit through TargetType.
                _typedefStack.Push(typedefType);
                typedefType.TargetType.Accept(this, null);
                _typedefStack.Pop();
            }

            //typedef has been fully checked.
            typedefType.SetFlag(Symbol.Flags.CheckedForCycles, true, false);
            return null;

            //Local function to update parent typedefs and mark them as cyclic.
            void FlagParentsAsCyclic()
            {
                foreach (var visitedTypedef in _typedefStack)
                {
                    visitedTypedef.SetFlag(Symbol.Flags.IsCyclic, true, false);
                }
            }
        }

        public bool IsCyclic([NotNull] TypedefType typedef)
        {
            typedef.Accept(this, null);
            return typedef.HasFlag(Symbol.Flags.IsCyclic);
        }
    }
}
