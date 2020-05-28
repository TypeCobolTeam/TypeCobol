namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
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
    public class RenamesSymbol : VariableSymbol
    {
        public RenamesSymbol(string name) : base(name)
        {
            base.SetFlag(Flags.Renames, true);
        }

        public override TR Accept<TR, TP>(IVisitor<TR, TP> v, TP arg) { return v.VisitRenamesSymbol(this, arg); }
    }
}
