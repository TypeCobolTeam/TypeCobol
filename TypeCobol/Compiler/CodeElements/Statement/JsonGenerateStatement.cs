namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The JSON GENERATE statement converts data to JSON format.
    /// </summary>
    public class JsonGenerateStatement : StatementElement
    {
        public JsonGenerateStatement()
            : base(CodeElementType.JsonGenerateStatement, StatementType.JsonGenerateStatement)
        {

        }

        /// <summary>
        /// The receiving area for the generated JSON text. identifier-1 must reference
        /// one of the following items:
        /// - An elementary data item of category alphanumeric
        /// - An alphanumeric group item
        /// - An elementary data item of category national
        /// - A national group item
        /// When identifier-1 references a national group item, identifier-1 is processed
        /// as an elementary data item of category national. When identifier-1
        /// references an alphanumeric group item, identifier-1 is treated as though it
        /// were an elementary data item of category alphanumeric.
        /// identifier-1 must not be defined with the JUSTIFIED clause, and cannot be a
        /// function identifier. identifier-1 can be subscripted or reference modified.
        /// identifier-1 must not overlap identifier-2 or identifier-3.
        /// The generated JSON text is encoded in UTF-8 (CCSID 1208) unless
        /// identifier-1 is of category national, in which case it is encoded in UTF-16
        /// big-Endian (CCSID 1200). Conversion of the data values and NAME
        /// phrase literals is done according to the compiler CODEPAGE option in
        /// effect for the compilation. Conversion of original data names is always
        /// done using CCSID 1140.
        /// identifier-1 must be large enough to contain the generated JSON text.
        /// Typically, it should be from 2 to 3 times the size of identifier-2, depending
        /// on the lengths of the data-names within identifier-2. If identifier-1 is not
        /// large enough, an exception condition exists at the end of the JSON
        /// GENERATE statement. If the COUNT phrase is specified, identifier-3
        /// contains the number of character encoding units that were actually
        /// generated.
        /// </summary>
        public ReceivingStorageArea Destination { get; set; }

        /// <summary>
        /// The group or elementary data item to be converted to JSON format.
        /// identifier-2 cannot be a function identifier or be reference modified, but it
        /// can be subscripted.
        /// identifier-2 must not overlap identifier-1 or identifier-3.
        /// The data description entry for identifier-2 must not contain a RENAMES
        /// clause.
        /// The following data items that are specified by identifier-2 are ignored by
        /// the JSON GENERATE statement:
        /// - Any subordinate unnamed elementary data items or elementary FILLER
        /// data items
        /// - Any slack bytes inserted for SYNCHRONIZED items
        /// - Any data item subordinate to identifier-2 that is defined with the
        /// REDEFINES clause or that is subordinate to such a redefining item
        /// - Any data item subordinate to identifier-2 that is defined with the
        /// RENAMES clause
        /// - Any group data item whose subordinate data items are all ignored
        /// All data items specified by identifier-2 that are not ignored according to the
        /// previous rules must satisfy the following conditions:
        /// - Each elementary data item must have a USAGE other than POINTER,
        /// FUNCTION-POINTER, PROCEDURE-POINTER, or OBJECT
        /// REFERENCE.
        /// - There must be at least one such elementary data item.
        /// - Each non-FILLER data-name must have a unique identifier within
        /// identifier-2.
        /// For example, consider the following data declaration:
        /// 01 STRUCT.
        ///   02 STAT PIC X(4).
        ///   02 IN-AREA PIC X(100).
        ///   02 OK-AREA REDEFINES IN-AREA.
        ///     03 FLAGS PIC X.
        ///     03 PIC X(3).
        ///     03 COUNTER USAGE COMP-5 PIC S9(9).
        ///     03 ASFNPTR REDEFINES COUNTER USAGE FUNCTION-POINTER.
        ///     03 UNREFERENCED PIC X(92).
        ///   02 NG-AREA1 REDEFINES IN-AREA.
        ///     03 FLAGS PIC X. 03 PIC X(3).
        ///     03 PTR USAGE POINTER.
        ///     03 ASNUM REDEFINES PTR USAGE COMP-5 PIC S9(9).
        ///     03 PIC X(92).
        ///   02 NG-AREA2 REDEFINES IN-AREA.
        ///     03 FN-CODE PIC X.
        ///     03 UNREFERENCED PIC X(3).
        ///     03 QTYONHAND USAGE BINARY PIC 9(5).
        ///     03 DESC USAGE NATIONAL PIC N(40).
        ///     03 UNREFERENCED PIC X(12).
        /// The following data items from the example can be specified as identifier-2:
        /// - STRUCT, of which subordinate data items STAT and IN-AREA would be
        /// converted to JSON format. (OK-AREA, NG-AREA1, and NG-AREA2 are ignored
        /// because they specify the REDEFINES clause.)
        /// - OK-AREA, of which subordinate data items FLAGS, COUNTER, and
        /// UNREFERENCED would be converted. (The item whose data description
        /// entry specifies 03 PIC X(3) is ignored because it is an elementary
        /// FILLER data item. ASFNPTR is ignored because it specifies the
        /// REDEFINES clause.)
        /// - Any of the elementary data items that are subordinate to STRUCT except:
        ///   – ASFNPTR or PTR (disallowed usage)
        ///   – UNREFERENCED OF NG-AREA2 (nonunique names for data items that are
        /// otherwise eligible)
        ///   – Any FILLER data items
        /// The following data items cannot be specified as identifier-2:
        /// - NG-AREA1, because subordinate data item PTR specifies USAGE POINTER
        /// but does not specify the REDEFINES clause. (PTR would be ignored if it
        /// is defined with the REDEFINES clause.)
        /// - NG-AREA2, because subordinate elementary data items have the
        /// nonunique name UNREFERENCED.
        /// </summary>
        public Variable Source { get; set; }

        /// <summary>
        /// If the COUNT phrase is specified, identifier-3 contains (after successful
        /// execution of the JSON GENERATE statement) the count of generated JSON
        /// character encoding units. If identifier-1 (the receiver) is of category national,
        /// the count is in double-bytes. Otherwise, the count is in bytes.
        /// identifier-3
        /// The data count field. Must be an integer data item defined without
        /// the symbol P in its picture string.
        /// identifier-3 must not overlap identifier-1, identifier-2, identifier-4, or
        /// identifier-5.
        /// </summary>
        public ReceivingStorageArea CharactersCount { get; set; }

        /// <summary>
        /// Allows you to override the default JSON names derived from identifier-2 or
        /// its subordinate data items.
        /// identifier-4 must reference identifier-2 or one of its subordinate data items.
        /// It cannot be a function identifier and cannot be reference modified or
        /// subscripted. It must not specify any data item which is ignored by the
        /// JSON GENERATE statement. For more information about identifier-2, see
        /// the description of identifier-2. If identifier-4 is specified more than once in
        /// the NAME phrase, the last specification is used.
        /// literal-1 must be an alphanumeric or national literal containing the name
        /// to be generated in the JSON text corresponding to identifier-4.
        /// With PTF for APAR PH18641 installed, alternatively, you can specify
        /// OMITTED to generate an anonymous JSON object, whose top-level parent name is not generated.
        /// When you specify OMITTED, identifier-4 must reference identifier-2.
        /// </summary>
        public JsonNameMapping[] NameMappings { get; set; }

        /// <summary>
        /// SUPPRESS phrase
        /// Allows you to identify and unconditionally exclude items that are
        /// subordinate to identifier-2, and thus selectively generate output for the
        /// JSON GENERATE statement. If the SUPPRESS phrase is specified,
        /// identifier-1 must be large enough to contain the generated JSON document
        /// before any suppression.
        /// </summary>
        public JsonSuppressDirective[] JsonSuppressDirectives { get; set; }

        /// <summary>
        /// Allows you to specify items that will be generated as JSON BOOLEAN name/value pairs.
        /// </summary>
        public JsonGenerateConvertingDirective[] JsonConvertingDirectives { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor)
                   && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, this.Destination, this.Source, this.CharactersCount)
                   && this.ContinueVisitToChildren(astVisitor, this.NameMappings, this.JsonSuppressDirectives, this.JsonConvertingDirectives);
        }
    }

    /// <summary>
    /// Represents an association between a data item in source and a name to be used to represent
    /// this data item in the resulting generated JSON (for GENERATE and PARSE statement).
    /// </summary>
    public class JsonNameMapping : IVisitable
    {
        /// <summary>
        /// Data item to be renamed.
        /// </summary>
        public Variable DataItem { get; set; }

        /// <summary>
        /// Expected name in JSON.
        /// </summary>
        public AlphanumericValue OutputName { get; set; }

        /// <summary>
        /// Is expected name omitted?
        /// To generate an anonymous JSON object, whose top-level parent name is not generated.
        /// </summary>
        public SyntaxProperty<bool> Omitted { get; set; }

        public bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return this.ContinueVisitToChildren(astVisitor, this.DataItem, this.OutputName, this.Omitted);
        }
    }

    /// <summary>
    /// For SUPPRESS clauses using data item type matching
    /// </summary>
    public enum JsonItemType
    {
        /// <summary>
        /// Suppress only NUMERIC items
        /// </summary>
        NUMERIC,

        /// <summary>
        /// Suppress only NON-NUMERIC items
        /// </summary>
        NONNUMERIC
    }

    /// <summary>
    /// SUPPRESS phrase
    /// Allows you to identify items that are subordinate to identifier-2 and must
    /// be suppressed when generating the XML if they contain values that are
    /// specified in the WHEN clause. If the SUPPRESS phrase is specified,
    /// identifier-1 must be large enough to contain the generated XML document
    /// before any suppression.
    /// </summary>
    public class JsonSuppressDirective : IVisitable
    {
        /// <summary>
        /// With the generic-suppression-phrase, elementary items subordinate to identifier-2 that are not
        /// otherwise ignored by JSON GENERATE operations are identified generically for potential suppression.
        /// Either items of class numeric, if the NUMERIC keyword is specified, or items that are not of
        /// class numeric, if the NONNUMERIC keyword is specified, or both if neither is specified, might be
        /// suppressed.
        /// If multiple generic-suppression-phrase are specified, the effect is cumulative.
        /// </summary>
        public SyntaxProperty<JsonItemType> JsonItemTypeToSuppress { get; set; }

        /// <summary>
        /// identifier-5 explicitly identifies items for potential suppression. identifier-5 must reference a data item
        /// that is subordinate to identifier-2 and that is not otherwise ignored by the operation of the JSON
        /// GENERATE statement.identifier-5 cannot be a function identifier and cannot be reference modified or
        /// subscripted.If the WHEN phrase is specified, identifier-5 must reference an elementary data item. If
        /// the WHEN phrase is omitted, identifier-5 may be a group item.If identifier-5 specifies a group data
        /// item, that group data item and all data items that are subordinate to the group item are excluded.
        /// Duplicate specifications of identifier-5 are permitted.
        /// If identifier-5 is specified, the following rules apply to it:
        /// * If ZERO, ZEROES, or ZEROS is specified in the WHEN phrase, identifier-5 must not be of USAGE
        /// DISPLAY-1.
        /// * If SPACE or SPACES is specified in the WHEN phrase, identifier-5 must be of USAGE DISPLAY,
        /// DISPLAY-1, or NATIONAL. If identifier-5 is a zoned or national decimal item, it must be an integer.
        /// * If LOW-VALUE, LOW-VALUES, HIGH-VALUE, or HIGH-VALUES is specified in the WHEN phrase,
        /// identifier-5 must be of USAGE DISPLAY or NATIONAL.If identifier-5 is a zoned or national decimal
        /// item, it must be an integer.
        /// </summary>
        public Variable DataItemName { get; set; }

        /// <summary>
        /// The comparison operation that determines whether an item will be suppressed is a relation condition
        /// as shown in the table of Comparisons involving figurative constants.That is, the comparison is a
        /// numeric comparison if the value specified is ZERO, ZEROS, or ZEROES, and the item is of class
        /// numeric. For all other cases, the comparison operation is an alphanumeric, DBCS, or national
        /// comparison, depending on whether the item is of usage DISPLAY, DISPLAY-1, or NATIONAL,
        /// respectively.
        /// When the SUPPRESS phrase is specified, a group item subordinate to identifier-2 is excluded from
        /// the generated JSON text if all the eligible items subordinate to the group item are excluded.The
        /// outermost object that corresponds to identifier-2 itself is always generated, even if all the items
        /// subordinate to identifier-2 are excluded.
        /// </summary>
        public RepeatedCharacterValue[] ItemValuesToSuppress;

        public bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return this.ContinueVisitToChildren(astVisitor, JsonItemTypeToSuppress, DataItemName)
                && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)ItemValuesToSuppress);
        }
    }

    /// <summary>
    /// Represents an association between a data item in source and its TRUE value when converting it to a JSON boolean.
    /// </summary>
    public class JsonGenerateConvertingDirective : IVisitable
    {
        /// <summary>
        /// identifier-6 must be a single-byte alphanumeric elementary data item whose data definition entry
        /// contains PICTURE X.
        /// </summary>
        public Variable DataItem { get; set; }

        /// <summary>
        /// condition-name-1 and literal-2 represent values of identifier-6 that will be generated as a JSON
        /// BOOLEAN true value.All other values of identifier-6 will be generated as a JSON BOOLEAN false value.
        /// condition-name-1 must be a level-88 item directly subordinate to identifier-6 and can be specified
        /// with multiple values or value ranges.literal-2 must be a single-byte alphanumeric literal.
        /// </summary>
        public Variable TrueValue { get; set; }

        public JsonGenerateConvertingDirective(Variable dataItem, Variable trueValue) =>
            (DataItem, TrueValue) = (dataItem, trueValue);

        public bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return this.ContinueVisitToChildren(astVisitor, this.DataItem, this.TrueValue);
        }
    }
}
