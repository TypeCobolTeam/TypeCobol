﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scopes;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents an Index.
    /// The tupe of an index is : S9(8) comp
    /// </summary>
    public class IndexSymbol : VariableSymbol
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        /// <param name="name">The Index name</param>
        public IndexSymbol(String name)
            : base(name)
        {
            Kind = Kinds.Index;
            Type = BuiltinTypes.IndexType;
        }

        /// <summary>
        /// The Indexed Variable.
        /// </summary>
        public VariableSymbol Indexed { get; set; }        

        public override string IndexedName => Indexed != null && Indexed.Name .Length != 0 ? Indexed.Name + "::" + Name : Name;
        public override string IndexedOFName => Indexed != null && Indexed.Name.Length != 0 ? Name +  " OF " + Indexed.Name  : Name;
        public override string IndexedDotName => Indexed != null && Indexed.Name.Length != 0 ? Indexed.Name + '.' + Name : Name;

        public override Symbol LookupParentOfName(string name)
        {
            if (Indexed == null)
                return base.LookupParentOfName(name);
            if (Indexed.Name.Equals(name, StringComparison.OrdinalIgnoreCase))
                return Indexed;
            return Indexed.LookupParentOfName(name);
        }

        public override bool HasParent(Symbol parent)
        {
            if (Indexed == null)
                return base.HasParent(parent);
            if (Indexed == parent)
                return true;
            if (Indexed == null || parent == null)
                return false;
            return Indexed.HasParent(parent);
        }

        /// <summary>
        /// When an IndexSymbol is normalized it IndexedSymbol must changed.
        /// </summary>
        /// <param name="scope"></param>
        internal override void NormalizeExpandedSymbol(Scope<VariableSymbol> scope)
        {
            base.NormalizeExpandedSymbol(scope);
            Scope<VariableSymbol>.Entry entry = scope.Lookup(Indexed.Name);
            System.Diagnostics.Debug.Assert(entry != null);
            System.Diagnostics.Debug.Assert(entry.Count == 1);
            Indexed = entry.Symbol;            
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitIndexSymbol(this, arg); }
    }
}
