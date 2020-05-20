using System;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;

using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Class that represents a group type.
    /// A GroupType is different from a Cobol group, it is used to represent
    /// COBOL groups but more generally speaking it is used to represent data descriptions
    /// having children. It means that an item with data condition(s) is also represented
    /// using a GroupType.
    /// Use <see cref="IsElementary"/> property to distinguish between true COBOL groups
    /// and elementary items having data condition(s).
    /// </summary>
    public class GroupType : Type
    {
        /// <summary>
        /// The fields of this GroupType.
        /// </summary>
        public Domain<VariableSymbol> Fields { get; internal set; }

        /// <summary>
        /// Scope Owner constructor
        /// </summary>
        /// <param name="owner">Owner of the group scope if any</param>
        public GroupType(Symbol owner)
            : base(Tags.Group)
        {
            Fields = new Domain<VariableSymbol>(owner);
        }

        /// <summary>
        /// Indicates whether this group is elementary.
        /// A group is still considered as an elementary item if all
        /// of its children are DataCondition (level-88).
        /// </summary>
        public bool IsElementary => Fields.All(f => f.IsCondition);

        internal override void SetFlag(Flags flag, bool value, bool propagate = false)
        {
            base.SetFlag(flag, value, propagate);
            if (propagate)
            {
                foreach (var varSym in Fields)
                {
                    varSym.SetFlag(flag, value, true);
                }
            }
        }

        /// <summary>
        /// Using Cobol some records can have a leading type
        /// which can be a USAGE type or a PICTURE Type, for instance;
        /// 
        /// 10  checkToDo-value PIC X VALUE LOW-VALUE.
        ///        88  checkToDo VALUE 'T'.
        ///        88  checkToDo-false VALUE 'F'
        ///             X'00' thru 'S'
        ///             'U' thru X'FF'.
        ///
        /// The leading type of the group is PIC X and the group is also
        /// considered as elementary item in this case.
        /// </summary>
        public Type LeadingType
        {
            get;
            set;
        }

        public override int Length
        {
            get {
                return Fields.Sum(f => f.Type.Length);
             }
        }

        /// <summary>
        /// 01 group1. => DerivedType can be "pic X(2)"
        ///   05 var1a pic X.
        ///   05 var1b pic X.
        ///   
        /// 01 group2. => DerivedType can be "pic X(5)" or maybe "pic 9(5)"
        ///   05 var2 pic 9(5).
        ///   
        /// 01 group3 pointer. => DerivedType ?
        ///   05 var3 pointer.
        /// 
        /// TODO also check memory alignment
        /// </summary>
        public Tuple<Type, Flags> DerivedType()
        {
            //1st version, we always return alphanumeric
            return new Tuple<Type, Flags>(new PictureType($"X({this.Length})", false), 0);
            //TODO be more precise
            return null;
        }

        /// <summary>
        /// A Record may always expand to another records because it is related to a new Symbol owner.
        /// </summary>
        public override bool MayExpand => true;

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            var level = indentLevel + 1;
            if (LeadingType != null)
            {
                output.Write(indent);
                output.WriteLine("LeadingType:");
                LeadingType.Dump(output, level);
            }

            if (Fields != null && Fields.Count > 0)
            {
                output.Write(indent);
                output.WriteLine("Fields:");
                foreach (var field in Fields)
                {
                    field.Dump(output, level);
                }
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitGroupType(this, arg);
        }
    }
}
