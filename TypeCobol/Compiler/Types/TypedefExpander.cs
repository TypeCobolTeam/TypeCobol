using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Perform the expansion of a TypeCobol type to a plain Cobol85 type.
    /// TYPEDEF are expanded, Record Types are cloned with fresh fields.
    /// </summary>
    public class TypedefExpander : Type.AbstractTypeVisitor<Type, Symbol>
    {
        public ProgramSymbol Program
        {
            get;
            internal set;
        }

        public SymbolExpander SymExpander
        {
            get;
            internal set;
        }

        /// <summary>
        /// For checking Cyclic Typedef.
        /// </summary>
        /// 
        private HashSet<TypedefType> _cyclicCheck = new HashSet<TypedefType>();
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="program">The program requesting the expansion</param>
        public TypedefExpander(ProgramSymbol program, SymbolExpander symExpander = null)
        {
            this.Program = program;
            this.SymExpander = symExpander??new SymbolExpander(program, this);
        }
        /// <summary>
        /// By default no expansion.
        /// </summary>
        /// <param name="t">The type to expand</param>
        /// <param name="owner">The Owner Symbol requesting the type expansion</param>
        /// <returns></returns>
        public override Type VisitType(Type t, Symbol owner)
        {
            if (t.HasFlag(Symbol.Flags.BuiltinType))
            {
                return ExpandCustomType(t, owner);
            }
            else
            {
                return t;
            }
        }

        public override Type VisitArrayType(ArrayType t, Symbol owner)
        {
            if (!t.MayExpand)
                return t;
            ArrayType newType =  (ArrayType)t.Clone();
            newType.ElementType = t.ElementType.Accept(this, owner);
            return newType;
        }

        public override Type VisitPointerType(PointerType t, Symbol owner)
        {
            if (!t.MayExpand)
                return t;
            PointerType newType = (PointerType)t.Clone();
            newType.ElementType = t.ElementType.Accept(this, owner);
            return newType;
        }

        /// <summary>
        /// A Record Type is cloned with a a new Scope with new fresh field is created.
        /// </summary>
        /// <param name="t"></param>
        /// <param name="owner"></param>
        /// <returns></returns>
        public override Type VisitGroupType(GroupType t, Symbol owner)
        {
            GroupType newType = (GroupType) t.Clone();
            newType.Scope = new Scope<VariableSymbol>(owner);
            foreach (var field in t.Scope)
            {
                //Clone only variables that are inside a TYPEDEF
                bool isInTypedef = field.HasFlag(Symbol.Flags.InsideTypdef);
                VariableSymbol newField =  isInTypedef ? (VariableSymbol)field.Clone() : field;
                if (isInTypedef)
                {
                    newField.GlobalIndex = 0;
                }

                newField.Accept(SymExpander, owner);
                
                System.Diagnostics.Debug.Assert(newField.Type != null);
                //Normalize the new field
                newField.NormalizeExpandedSymbol(newType.Scope);
                newType.Scope.Enter(newField);
                //Set the new owner
                newField.Owner = owner;
                if (isInTypedef)
                {
                    //Important add to the domain the new field that was cloned.
                    Program.AddToDomain(newField);
                }
            }
            return newType;
        }

        /// <summary>
        /// ooohhh a Typedef :-)
        /// </summary>
        /// <param name="t"></param>
        /// <param name="s"></param>
        /// <exception cref="Type.CyclicTypeException">If the type t is cyclic</exception>
        /// <returns>The expanded type</returns>
        public override Type VisitTypedefType(TypedefType t, Symbol s)
        {
            try
            {
                if (_cyclicCheck.Contains(t))
                    throw new Type.CyclicTypeException(t);
                _cyclicCheck.Add(t);
                //Take the representing type.
                Type typRepr = t.TypeComponent;
                return (typRepr == null || !typRepr.MayExpand) ? typRepr : typRepr.Accept(this, s);
            }
            finally
            {
                _cyclicCheck.Remove(t);
            }
        }

        /// <summary>
        /// The Cobol85 representation off a date type.
        /// </summary>
        public static GroupType DateCobol85 { get; private set;  }
        public static Type DateYYYYType = new PictureType(new PictureValidator("9(04)", false));
        public static Type DateMMType = new PictureType(new PictureValidator("9(02)", false));
        public static Type DateDDType = DateMMType;
        public static Type PicX = new PictureType(new PictureValidator("X", false));


        /// <summary>
        /// Expand a Custom Type.
        /// </summary>
        /// <param name="t"></param>
        /// <param name="s"></param>
        /// <returns></returns>
        private Type ExpandCustomType(Type t, Symbol s)
        {
            System.Diagnostics.Debug.Assert(s.Kind == Symbol.Kinds.Variable);
            VariableSymbol varSym = (VariableSymbol) s;
            if (t == BuiltinTypes.BooleanType)
            {//Do nothing
            }
            else if (t == BuiltinTypes.DateType)
            {                
                GroupType recType = new GroupType(s);
                VariableSymbol yyyy = new VariableSymbol("YYYY") {Level = varSym.Level + 1, Type = DateYYYYType, Owner = s};
                recType.Scope.Enter(yyyy);
                //Inherits flags
                yyyy.Flag = s.Flag;
                //Important add to the domain the new field.
                Program.AddToDomain(yyyy);

                VariableSymbol mm = new VariableSymbol("MM") {Level = varSym.Level + 1, Type = DateMMType, Owner = s };
                recType.Scope.Enter(mm);
                mm.Flag = s.Flag;
                Program.AddToDomain(mm);

                VariableSymbol dd = new VariableSymbol("DD") {Level = varSym.Level + 1, Type = DateDDType, Owner = s };
                recType.Scope.Enter(dd);
                dd.Flag = s.Flag;
                Program.AddToDomain(dd);

                return recType;
            }
            return t;
        }
    }
}
