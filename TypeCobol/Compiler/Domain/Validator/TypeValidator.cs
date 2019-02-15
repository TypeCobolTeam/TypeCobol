using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Domain.Validator
{
    public class TypeValidator : Types.Type.AbstractTypeVisitor<bool, object>
    {
        /// <summary>
        /// Unvalidated types.
        /// </summary>
        public List<Types.Type> Unvalidated
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Symbol Validator to use.
        /// </summary>
        public SymbolValidator SymValidator { get; internal set; }

        /// <summary>
        /// Constructor
        /// </summary>
        public TypeValidator() : this(new SymbolValidator())
        {
            this.SymValidator.TypValidator = this;
        }

        /// <summary>
        /// Constructor based on a SymbolValidator instance.
        /// </summary>
        /// <param name="symbolValidator"></param>
        public TypeValidator(SymbolValidator symbolValidator)
        {
            this.SymValidator = symbolValidator??new SymbolValidator(this);
        }

        /// <summary>
        /// Validator for Type.
        /// </summary>
        /// <param name="t"></param>
        /// <param name="s"></param>
        /// <returns></returns>
        public override bool VisitType(Types.Type t, object s)
        {
            return true;
        }

        public override bool VisitArrayType(ArrayType t, object arg)
        {
            if (!(t.ElementType?.Accept(this, arg) ?? false))
            {
                Unvalidated.Add(t);
                return false;
            }
            return true;
        }
        public override bool VisitPointerType(PointerType t, object arg)
        {
            if (!(t.ElementType?.Accept(this, arg) ?? false))
            {
                Unvalidated.Add(t);
                return false;
            }
            return true;
        }

        public override bool VisitRecordType(RecordType t, object arg)
        {
            bool bResult = t.LeadingType?.Accept(this, arg) ?? true;
            foreach (var s in t.Scope)
            {
                if (!s.Accept(SymValidator, arg)) bResult = false;
            }

            if (!bResult)
            {
                Unvalidated.Add(t);
            }
            return bResult;
        }

        public override bool VisitRenamesType(RenamesType t, object arg)
        {
            bool bResult = VisitRecordType((RecordType) t, arg);
            if (bResult && (t.DataName1 == null || t.DataName2 == null))
            {
                Unvalidated.Add(t);
                bResult = false;
            }
            return bResult;
        }

        public override bool VisitTypedefType(TypedefType t, object arg)
        {
            if (!(t.TargetType?.Accept(this, arg) ?? false))
            {
                Unvalidated.Add(t);
                return false;
            }
            return true;
        }

    }
}
