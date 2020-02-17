using System.Collections.Generic;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Cyclic Type Checker. Determine if for instance a type contains a TYPEDEF type
    /// which is cyclic.
    /// An exception of type <exception cref="Type.CyclicTypeException">CyclicTypeException</exception> is thrown
    /// by this visitor if a cyclic definition is found
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
            if (_typedefStack.Contains(typedefType))
            {
                _typedefStack.Clear();
                throw new Type.CyclicTypeException(typedefType);
            }

            _typedefStack.Push(typedefType);
            typedefType.TargetType.Accept(this, null);
            _typedefStack.Pop();

            return null;
        }

        /// <summary>
        /// Tests if the given Type is cyclic.
        /// </summary>
        /// <param name="type">Type to test</param>
        /// <param name="cyclicType">Type on which the cycle has been detected</param>
        /// <returns>True if the type is cyclic, False otherwise</returns>
        public bool IsCyclic([NotNull] Type type, out Type cyclicType)
        {
            System.Diagnostics.Debug.Assert(type != null);
            try
            {
                type.Accept(this, null);
                cyclicType = null;
                return false;
            }
            catch (Type.CyclicTypeException cyclicTypeException)
            {
                cyclicType = cyclicTypeException.TargetType;
                return true;
            }
        }
    }
}
