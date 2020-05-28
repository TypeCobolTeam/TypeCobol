using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The ALLOCATE statement obtains dynamic storage.
    /// The allocated storage persists until explicitly released with a FREE statement or the
    /// run unit is terminated, whichever occurs first.
    /// </summary>
    public class AllocateStatement : StatementElement
    {
        public AllocateStatement()
            : base(CodeElementType.AllocateStatement, StatementType.AllocateStatement)
        {

        }

        /// <summary>
        /// Specifies a number of bytes of storage to be allocated:
        /// - If arithmetic-expression-1 does not evaluate to an integer, the result is
        /// rounded up to the next whole number.
        /// - If arithmetic-expression-1 evaluates to 0 or a negative value, the data item
        /// referenced by data-name-2 is set to the predefined address NULL.
        /// <remarks>Mutually exclusive with <see cref="AllocatedArea"/></remarks>
        /// </summary>
        public ArithmeticExpression AllocatedSize { get; set; }

        /// <summary>
        /// Must be a level-01 or level-77 item defined in the LINKAGE SECTION.
        /// If data-name-1 is specified, the RETURNING phrase can be omitted.
        /// Otherwise, the RETURNING phrase must be specified.
        /// Cannot be reference modified.
        /// Cannot be a group item that contains an unbounded table.
        /// <remarks>Mutually exclusive with <see cref="AllocatedSize"/></remarks>
        /// </summary>
        public ReceivingStorageArea AllocatedArea { get; set; }

        /// <summary>
        /// The INITIALIZED phrase initializes the allocated storage:
        /// - If the INITIALIZED phrase is not specified, the content of the allocated storage
        /// is undefined.
        /// - If both arithmetic-expression-1 and the INITIALIZED phrase are specified, all
        /// bytes of the allocated storage are initialized to binary zeros.
        /// - If both data-name-1 and the INITIALIZED phrase are specified, the allocated
        /// storage is initialized as if an INITIALIZE data-name-1 WITH FILLER ALL TO
        /// VALUE THEN TO DEFAULT statement were executed.
        /// <remarks>Property value is null when INITIALIZED keyword is absent.</remarks>
        /// </summary>
        public SyntaxProperty<bool> Initialized { get; set; }


        /// <summary>
        /// Must be an unsigned integer with either the value of 24 or 31.
        /// The LOC phrase controls how ALLOCATE acquires storage:
        /// - LOC 24 causes ALLOCATE to acquire storage from below the 16 MB line,
        ///   regardless of the setting of the DATA compiler option.
        /// - LOC 31 causes ALLOCATE to attempt to acquire storage from above the 16 MB
        ///    line, regardless of the setting of the DATA compiler option.
        /// - Note: It is still possible that storage is acquired below the 16 MB line with LOC
        /// 31 if storage above the 16 MB line is exhausted.
        /// </summary>
        [CanBeNull]
        public IntegerVariable LocValue { get; set; }

        /// <summary>
        /// Must be defined as USAGE IS POINTER.
        /// Can be qualified or subscripted.
        /// If arithmetic-expression-1 CHARACTERS is specified, the RETURNING data-item-2
        /// will be set to the address of the obtained storage.
        /// If data-name-1 is specified, the address of the data item is set to the address of the
        /// obtained storage, as if the "SET ADDRESS OF data-name-1 TO address" statement
        /// was used. If a RETURNING data item is also specified, the pointer data item will
        /// contain that address.
        /// </summary>
        public ReceivingStorageArea ReturningPointer { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor)
                   && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, this.AllocatedSize, this.AllocatedArea, this.Initialized, this.ReturningPointer);
        }
    }
}
