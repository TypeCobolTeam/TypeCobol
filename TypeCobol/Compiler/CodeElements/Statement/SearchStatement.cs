using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p406:
    /// The SEARCH statement searches a table for an element that satisfies the specified
    /// condition and adjusts the associated index to indicate that element.
    /// </summary>
    public abstract class SearchStatement : StatementElement
    {
        public SearchStatement(StatementType statementType) : base(CodeElementType.SearchStatement, statementType)
        { }
    }

    /// <summary>
    /// p408: Format 1: SEARCH statement for serial search
    /// Use format 1 (serial search) when the table that you want to search has not been
    /// sorted. Use format 1 to search a sorted table when you want to search serially
    /// through the table or you want to control subscripts or indexes.
    /// </summary>
    public class SearchSerialStatement : SearchStatement
    { 
        public SearchSerialStatement() : base(StatementType.SearchSerialStatement)
        { }

         /// <summary>
        /// p409:
        /// identifier-1 (serial search)
        /// identifier-1 identifies the table that is to be searched. identifier-1 references
        /// all occurrences within that table.
        /// The data description entry for identifier-1 must contain an OCCURS clause.
        /// The data description entry for identifier-1 should contain an OCCURS
        /// clause with the INDEXED BY phrase, but a table can be searched using an
        /// index defined for an appropriately described different table.
        /// identifier-1 can reference a data item that is subordinate to a data item that
        /// is described with an OCCURS clause (that is, identifier-1 can be a
        /// subordinate table within a multidimensional table). In this case, the data
        /// description entries must specify an INDEXED BY phrase for each
        /// dimension of the table.
        /// identifier-1 must not be subscripted or reference-modified.
        /// </summary>
        public Identifier Element = null;
        /// <summary>
        /// p411:
        /// Must be either an index data item or an elementary integer item. identifier-2
        /// cannot be subscripted by the first (or only) index-name specified for
        /// identifier-1. During the search, one of the following actions applies:
        /// * If identifier-2 is an index data item, then, whenever the search index is
        /// increased, the specified index data item is simultaneously increased by
        /// the same amount.
        /// * If identifier-2 is an integer data item, then, whenever the search index is
        /// increased, the specified data item is simultaneously increased by 1.
        /// </summary>
        public Identifier VaryingIdentifier = null;
        /// <summary>
        /// p411:
        /// One of the following actions applies:
        /// * If index-name-1 is an index for identifier-1, this index is used for the
        /// search. Otherwise, the first (or only) index-name is used.
        /// * If index-name-1 is an index for another table element, then the first (or
        /// only) index-name for identifier-1 is used for the search; the occurrence
        /// number represented by index-name-1 is increased by the same amount as
        /// the search index-name and at the same time.
        ///
        /// When the VARYING index-name-1 phrase is omitted, the first (or only)
        /// index-name for identifier-1 is used for the search.
        ///
        /// If indexing is used to search a table without an INDEXED BY phrase,
        /// correct results are ensured only if both the table defined with the index
        /// and the table defined without the index have table elements of the same
        /// length and with the same number of occurrences.
        ///
        /// When the object of the VARYING phrase is an index-name for another
        /// table element, one serial SEARCH statement steps through two table
        /// elements at once.
        /// </summary>
        public IndexName VaryingIndex = null;
        /// <summary>
        /// p408:
        /// Use format 1 (serial search) when the table that you want to search has not been
        /// sorted. Use format 1 to search a sorted table when you want to search serially
        /// through the table or you want to control subscripts or indexes.
        /// </summary>
        public bool IsVarying { get { return VaryingIdentifier != null || VaryingIndex != null; } }
        
    }

    /// <summary>
    /// p408: Format 2: SEARCH statement for binary search
    /// Use format 2 (binary search) when you want to efficiently search across all
    /// occurrences in a table. The table must previously have been sorted.
    /// </summary>
    public class SearchBinaryStatement : SearchStatement
    {
        public SearchBinaryStatement() : base(StatementType.SearchBinaryStatement)
        { }

        /// <summary>
        /// p409:
        /// Use format 2 (binary search) when you want to efficiently search across all
        /// occurrences in a table. The table must previously have been sorted.
        /// </summary>
        public bool All = false;

    }
}
