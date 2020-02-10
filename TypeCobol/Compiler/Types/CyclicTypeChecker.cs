using System.Collections.Generic;

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
            bool pop = false;
            try
            {
                if (_typedefStack.Contains(typedefType))
                {
                    throw new Type.CyclicTypeException(typedefType);
                }

                _typedefStack.Push(typedefType);
                pop = true;
                typedefType.TargetType.Accept(this, null);
            }
            finally
            {
                if (pop)
                {
                    _typedefStack.Pop();
                }
            }

            return null;
        }
    }
}
