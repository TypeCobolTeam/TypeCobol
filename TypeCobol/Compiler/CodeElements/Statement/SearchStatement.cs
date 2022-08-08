namespace TypeCobol.Compiler.CodeElements {
/// <summary>
/// p406:
/// The SEARCH statement searches a table for an element that satisfies the specified
/// condition and adjusts the associated index to indicate that element.
/// </summary>
public abstract class SearchStatement: StatementElement {
    protected SearchStatement(StatementType statementType): base(CodeElementType.SearchStatement, statementType) { }

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
	///
	/// p412:
	/// identifier-1 (binary search)
	/// identifier-1 identifies the table that is to be searched. identifier-1 references
	/// all occurrences within that table.
	/// The data description entry for identifier-1 must contain an OCCURS clause
	/// with the INDEXED BY and KEY IS phrases.
	/// identifier-1 can reference a data item that is subordinate to a data item that
	/// contains an OCCURS clause (that is, identifier-1 can be a subordinate table within
	/// a multidimensional table). In this case, the data description entry must specify
	/// an INDEXED BY phrase for each dimension of the table. identifier-1 must not be
	/// subscripted or reference-modified.
	/// </summary>
	public Variable TableToSearch { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, TableToSearch);
        }
    }

/// <summary>
/// p408: Format 1: SEARCH statement for serial search
/// Use format 1 (serial search) when the table that you want to search has not been
/// sorted. Use format 1 to search a sorted table when you want to search serially
/// through the table or you want to control subscripts or indexes.
/// </summary>
public class SearchSerialStatement: SearchStatement {
	public SearchSerialStatement(): base(StatementType.SearchSerialStatement) { }

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
	public ReceivingStorageArea VaryingSearchIndex { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, VaryingSearchIndex);
        }
    }

/// <summary>
/// p408: Format 2: SEARCH statement for binary search
/// Use format 2 (binary search) when you want to efficiently search across all
/// occurrences in a table. The table must previously have been sorted.
/// </summary>
public class SearchBinaryStatement: SearchStatement {
	public SearchBinaryStatement(): base(StatementType.SearchBinaryStatement) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }

/// <summary>Conditional expression case for the SEARCH statement.</summary>
public class WhenSearchCondition: StatementElement {
	public WhenSearchCondition(): base((CodeElementType)int.MaxValue, (StatementType)int.MaxValue) { }

	public ConditionalExpression Condition { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, Condition);
        }
    }

}
