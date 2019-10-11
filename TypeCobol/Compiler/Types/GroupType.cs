using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Symbols;
using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Class that represents a group type
    /// </summary>
    public class GroupType : Type
    {
        /// <summary>
        /// The Scope of variables in this record.
        /// </summary>
        private Scope<VariableSymbol> _scope;

        /// <summary>
        /// Scope Owner constructor
        /// </summary>
        /// <param name="owner">Owner of the group scope if any</param>
        public GroupType(Symbol owner) : base(Tags.Group)
        {
            _scope = new Scope<VariableSymbol>(owner);
            _scope.Owner = owner;
        }

        /// <summary>
        /// Fields in this Records.
        /// </summary>
        public IList<VariableSymbol> Fields => Scope.Symbols;

        /// <summary>
        /// Get the scope of this record.
        /// </summary>
        public Scope<VariableSymbol> Scope
        {
            get => _scope;
            internal set => _scope = value;
        }

        internal override void SetFlag(Flags flag, bool value)
        {
            base.SetFlag(flag, value);
            foreach (var varSym in Scope.Symbols)
            {
                varSym.SetFlag(flag, value, true);
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
        /// the leading type is PIC X.
        /// </summary>
        public Type LeadingType
        {
            get;
            set;
        }
        public override Type TypeComponent => this;

        /// <summary>
        /// A Record may always expand to another records because it is related to a new Symbol owner.
        /// </summary>
        public override bool MayExpand => true;

        public override void Dump(TextWriter tw, int indentLevel)
        {
            indentLevel++;            
            foreach (var field in Scope)
            {                
                string s = new string(' ', 2 * indentLevel);
                tw.Write(s);
                field.Dump(tw, indentLevel);
            }
            --indentLevel;
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitGroupType(this, s); }
    }
}
