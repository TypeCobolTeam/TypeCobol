using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// The Symbol of a Variable declaration
    /// </summary>
    public class VariableSymbol : Symbol
    {
        /// <summary>
        /// Named constructor
        /// </summary>
        /// <param name="name"></param>
        public VariableSymbol(String name)
            : this(name, 0)
        {
        }

        /// <summary>
        /// Index Name creator
        /// </summary>
        /// <param name="name">Variable's name</param>
        /// <param name="varIndex">Internal Variable Index</param>
        internal VariableSymbol(String name, uint varIndex)
            : base(name, Kinds.Variable)
        {
            GlobalIndex = varIndex;
        }

        /// <summary>
        /// The Gloabal index associated to this variable.
        /// </summary>
        public uint GlobalIndex
        {
            get;
            internal set;
        }

        private int m_Level;
        /// <summary>
        /// Level of this variable.
        /// Un number beetwen (01 and 49 for groups and their elements),
        /// 77 for isolate variables , 88 for condition, 66 for RENAMES.
        /// </summary>
        public int Level
        {
            get
            {
                return m_Level;
            }
            set
            {
                System.Diagnostics.Contracts.Contract.Requires((value >= 1 && value <= 49) || (value == 77) || (value == 88) || (value == 66));
                m_Level = value;
            }
        }

        /// <summary>
        /// The Offset of this symbol in its hierachy.
        /// </summary>
        public int Offset
        {
            get;
            set;
        }

        /// <summary>
        /// Is this variable an isolate (independent) variable. Isolate variable have 77 as level rather than 01
        /// and has no sub level declaration.
        /// Level 77 can't be subdivided to another item nor they can't de sub divided by themselves.
        ///
        /// It Represents Noncontiguous data items or constants that are not subdivided and no hierarchical relationship
        /// to another data items.
        /// These data items are only defined in the WORKING-STORAGE and LINKAGE SECTIONS.
        /// Each name used for a noncontiguous data item must be unique since it cannot be qualified.
        /// 
        /// Rules:
        ///     1. Is used for independent data items.
        ///     2. Must be declared in AREA A
        ///     
        /// Exemple:
        ///     77 TOTAL-DAY  pic 9(6)V99.
        /// </summary>
        public bool IsIsoloate
        {
            get
            {
                return m_Level == 77;
            }
        }

        /// <summary>
        /// Is this variable a condition variable.
        /// Conditions or not variable but conditions associated to variables.
        /// 
        /// So A condtional variable is always subordinate to another data item.
        /// Condtional name identifies the particular value associated to verify during the flow.
        /// VALUE clause should be associated with conditional names.
        ///     
        /// Rules:
        ///     1. No PICTURE clauses is associated with 88 level number
        ///     2. 88 level number always associated with any other level number 0-49.
        ///     3. Any conditional keywords(IF, EVALUATE) using the 88 level number mostly.
        /// 
        /// Exemple:
        ///    01 RESPONSE pic X.
        ///     88 YES value "Y", "y".
        ///     88 NO value "N", "n".
        ///     
        /// </summary>
        public bool IsCondition
        {
            get
            {
                return m_Level == 88;
            }
        }

        /// <summary>
        /// 66 level number/RENAMES used to regrouping the elementary items in a group item.
        /// 66 level number/RENAMES will create a logical group from the group of elementary tems.
        /// </summary>
        public bool IsRenames
        {
            get
            {
                return m_Level == 66;
            }
        }

        /// <summary>
        /// Is this variable a Filler variable, that is to say variables that are used for filling this initial value.
        /// Exemple:
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
            if (symbol == null)
                return;
            if (Redefines == null)
            {
                Redefines = new List<VariableSymbol>();
            }
            Redefines.Add(symbol);
        }
    }
}
