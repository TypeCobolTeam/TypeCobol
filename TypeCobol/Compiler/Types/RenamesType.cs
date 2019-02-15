using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// This is a renamed type, a specialization of a RecordType.
    /// 
    /// RENAMES symbol used to regrouping elementary items in a group item.
    /// RENAMES will create a logical group from the group of elementary items.
    /// 
    /// Syntax:
    ///     66 data-name-1 RENAMES data-name-2
    ///         [THROUGH/THRU data-name-3]
    ///  
    /// data-name-1 is the alternative name for the logcal group.
    /// data-name-2 is the starting elementary item and data-name-3 is the ending elementary item
    ///             in the baic group.
    ///  When data-name-3 is not specified, the data-name-2 must be a group item
    ///  and all the elementary items under this RENAMED to data-name-1.
    ///  
    /// Rules:
    ///     1. RENAMES entries must be in a sequential order
    ///     2. 66 level numbers didn't have a PIC clause
    ///     3. RENAMES clause mut be coded at the end of the group
    ///     4. Level 66 entry cannot rename level-01, level-77, level-88, or another level 66-entry.
    ///     5. Elementary items with OCCURS clause should not be RENAMED.
    ///     
    /// Exemples:
    ///     01 A.
    ///         05 ITEM1 PIC X(5).
    ///         05 ITEM2 PIC X(5).
    ///         05 ITEM3 PIC X(5).
    ///         05 ITEM4 PIC X(5).
    ///         05 ITEM5 PIC X(5).
    ///         05 ITEM6 PIC X(5).
    ///         05 ITEM7 PIC X(5).
    ///         05 ITEM8 PIC X(5).
    ///         05 ITEM9 PIC X(5).
    ///         05 ITEM10 PIC X(5).
    ///     66 B RENAMES ITEM1 THRU ITEM6.
    /// </summary>
    /// </summary>
    public class RenamesType : RecordType
    {
        /// <summary>
        /// Constructor for the Syntax:
        ///     66 data-name-1 RENAMES data-name-2
        /// 
        /// 
        /// </summary>
        /// <param name="owner">Owner of the scope</param>
        /// <param name="containerType"></param>
        /// <param name="data_name_2"></param>
        /// <param name="data_name_3"></param>
        public RenamesType(Symbol owner, RecordType containerType, VariableSymbol data_name_2) : this(owner, containerType, data_name_2, data_name_2)
        {
        }

        /// <summary>
        /// Constructor for the Syntax:
        ///     66 data-name-1 RENAMES data-name-2
        ///         [THROUGH/THRU] data-name-3
        /// 
        /// 
        /// </summary>
        /// <param name="owner">Owner of the scope</param>
        /// <param name="containerType"></param>
        /// <param name="data_name_2"></param>
        /// <param name="data_name_3"></param>
        public RenamesType(Symbol owner, RecordType containerType, VariableSymbol data_name_2, VariableSymbol data_name_3) : base(owner)
        {
            ContainerType = containerType;
            DataName1 = data_name_2;
            DataName2 = data_name_3;
        }

        public VariableSymbol DataName1
        {
            get;
            internal set;
        }

        public VariableSymbol DataName2
        {
            get;
            internal set;
        }
        /// <summary>
        /// The Container type of the RENAMES
        /// </summary>
        public RecordType ContainerType
        {
            get;
            private set;
        }

        /// <summary>
        /// Dump this symbol in the given TextWriter instance
        /// </summary>
        /// <param name="tw">TextWriter instance</param>
        /// <param name="indentLevel">Indentation level</param>
        public override void Dump(TextWriter tw, int indentLevel)
        {
            string s = new string(' ', 2 * indentLevel);
            tw.Write("RENAMES ");
            tw.Write(DataName1.Name);
            if (DataName2 != null && DataName1 != DataName2)
            {
                tw.Write(" THRU ");
                tw.Write(DataName2.Name);
            }            
        }

        public override TR Accept<TR, TS>(IVisitor<TR, TS> v, TS s) { return v.VisitRenamesType(this, s); }
    }
}
