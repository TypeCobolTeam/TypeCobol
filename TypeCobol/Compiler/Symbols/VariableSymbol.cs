using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// The Symbol of a Variable declaration
    /// </summary>
    public class VariableSymbol : Symbol
    {
        /// <summary>
        /// Named constructor, for standard COBOL variables.
        /// </summary>
        /// <param name="name">Name of the new variable.</param>
        public VariableSymbol(string name)
            : this(name, Kinds.Variable)
        {

        }

        /// <summary>
        /// Named constructor, for inheritors.
        /// </summary>
        /// <param name="name">Name of the new variable.</param>
        /// <param name="kind">Variable kind.</param>
        protected VariableSymbol(string name, Kinds kind)
            : base(name, kind)
        {
            Indexes = new Domain<IndexSymbol>(this);
        }

        private int _level;
        /// <summary>
        /// Level of this variable.
        /// A number between (01 and 49 for groups and their elements),
        /// 77 for independent variables , 88 for condition, 66 for RENAMES.
        /// </summary>
        public int Level
        {
            get => _level;
            set => _level = value;
        }

        /// <summary>
        /// Is this variable an isolate (independent) variable. Isolate variable have 77 as level rather than 01
        /// and has no sub level declaration.
        /// Level 77 can't be subdivided to another item nor they can't de sub divided by themselves.
        ///
        /// It Represents non-contiguous data items or constants that are not subdivided and no hierarchical relationship
        /// to another data items.
        /// These data items are only defined in the WORKING-STORAGE, LOCAL-STORAGE and LINKAGE SECTIONS.
        /// Each name used for a non-contiguous data item must be unique since it cannot be qualified.
        /// 
        /// Rules:
        ///     1. Is used for independent data items.
        ///     2. Must be declared in AREA A
        ///     
        /// Example:
        ///     77 TOTAL-DAY  pic 9(6)V99.
        /// </summary>
        public bool IsIndependent => _level == 77;

        /// <summary>
        /// Is this variable a condition variable.
        /// Conditions or not variable but conditions associated to variables.
        /// 
        /// So A conditional variable is always subordinate to another data item.
        /// Conditional name identifies the particular value associated to verify during the flow.
        /// VALUE clause should be associated with conditional names.
        ///     
        /// Rules:
        ///     1. No PICTURE clauses is associated with 88 level number
        ///     2. 88 level number always associated with any other level number 0-49.
        ///     3. Any conditional keywords(IF, EVALUATE) using the 88 level number mostly.
        /// 
        /// Example:
        ///    01 RESPONSE pic X.
        ///     88 YES value "Y", "y".
        ///     88 NO value "N", "n".
        ///     
        /// </summary>
        public bool IsCondition => _level == 88;

        /// <summary>
        /// 66 level number/RENAMES used to regrouping the elementary items in a group item.
        /// 66 level number/RENAMES will create a logical group from the group of elementary terms.
        /// </summary>
        public bool IsRenames => _level == 66;

        /// <summary>
        /// Is this variable a Filler variable, that is to say variables that are used for filling this initial value.
        /// Example:
        ///     01 PRINT-LINE.
        ///         02 filler pic XX value "* ".
        ///         02 NAME pic X(20).
        ///         02 filler pic XXX value " * ".
        ///         02 AVERAGE pic Z9.99.
        ///         02 filler pic XX value " *".
        /// </summary>
        public bool IsFiller
        {
            get;
            set;
        }

        /// <summary>	
        /// All Symbol that redefines this Symbol.	
        /// </summary>	
        public List<VariableSymbol> Redefines
        {
            get;
            private set;
        }

        /// <summary>	
        /// Add a redefines symbol to this symbol.	
        /// </summary>	
        /// <param name="symbol"></param>	
        public void AddRedefines(VariableSymbol symbol)
        {
            System.Diagnostics.Debug.Assert(symbol != null);
            if (Redefines == null)
            {
                Redefines = new List<VariableSymbol>();
            }
            Redefines.Add(symbol);
        }

        /// <summary>
        /// If this variable is an array, VariableSymbol of the DEPENDING ON clause (if any).
        /// </summary>
        public VariableSymbol DependingOn
        {
            get;
            set; //TODO perform resolution at appropriate time ?
        }

        /// <summary>
        /// If this variable is an array, contains all indexes of this array.
        /// </summary>
        public Domain<IndexSymbol> Indexes { get; }

        /// <summary>
        /// Lookup for the parent having the given Level
        /// </summary>
        /// <param name="level">Target level</param>
        /// <param name="inclusive">true if this symbol must be taken in account, false otherwise</param>
        /// <returns>The parent symbol of the level if one exists, null otherwise</returns>
        public override Symbol LookupParentLevelSymbol(int level, bool inclusive)
        {
            if (this.Level == level && inclusive)
                return this;
            if (Owner == null)
                return null;
            if (Owner.Kind != Kinds.Variable)
                return null;

            return Owner.LookupParentLevelSymbol(level, true);
        }

        public override void Dump(TextWriter output, int indentLevel)
        {
            base.Dump(output, indentLevel);
            string indent = new string(' ', 2 * indentLevel);
            output.Write(indent);
            output.WriteLine("Level: " + Level);
            output.Write(indent);
            output.WriteLine("IsFiller: " + IsFiller);

            if (Redefines != null && Redefines.Count > 0)
            {
                output.Write(indent);
                output.WriteLine("Redefines:");
                indent += "  ";
                foreach (var redefines in Redefines)
                {
                    output.Write(indent);
                    output.WriteLine(redefines.FullName);//Write reference	
                }
            }

            if (DependingOn != null)
            {
                output.Write(indent);
                output.WriteLine($"DependingOn: {DependingOn.FullName}");//Write reference
            }

            if (Indexes.Count > 0)
            {
                output.Write(indent);
                output.WriteLine("Indexes:");
                var level = indentLevel + 1;
                foreach (var index in Indexes)
                {
                    index.Dump(output, level);
                }
            }
        }

        public override TResult Accept<TResult, TParameter>(IVisitor<TResult, TParameter> v, TParameter arg)
        {
            return v.VisitVariableSymbol(this, arg);
        }
    }
}
