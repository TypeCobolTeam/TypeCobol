using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Cyclic Type Checker. Determine if for instance a type contains a TYPEDEF type
    /// which is cyclic.
    /// </summary>
    public class CyclicTypeChecker : Type.AbstractTypeVisitor<bool, object>
    {
        /// <summary>
        /// For checking Cyclic Typedef.
        /// </summary>
        /// 
        private readonly HashSet<TypedefType> _cyclicCheck = new HashSet<TypedefType>();

        /// <summary>
        /// The type which is cyclic if any.
        /// </summary>
        public Type CyclicType
        {
            get;
            private set;
        }

        /// <summary>
        /// Any last variable with a cyclic type.
        /// </summary>
        public VariableSymbol LastSymbol
        {
            get;
            set;
        }

        public override bool VisitType(Type t, object s)
        {
            return false;
        }

        public override bool VisitArrayType(ArrayType t, object _) => t.MayExpand && t.ElementType.Accept(this, _);
        public override bool VisitPointerType(PointerType t, object _) => t.MayExpand && t.ElementType.Accept(this, _);
        public override bool VisitRecordType(RecordType t, object _)
        {
            foreach (var field in t.Scope)
            {
                if (field.Type != null && field.Type.Accept(this, _))
                {
                    LastSymbol = field;
                    return true;
                }
            }
            return false;
        }
        public override bool VisitTypedefType(TypedefType t, object _)
        {
            try
            {
                if (_cyclicCheck.Contains(t))
                {
                    this.CyclicType = t;
                    return true;
                }
                _cyclicCheck.Add(t);
                return t.TargetType?.Accept(this, _) ?? false;
            }
            finally
            {
                _cyclicCheck.Remove(t);
            }
        }
    }
}
