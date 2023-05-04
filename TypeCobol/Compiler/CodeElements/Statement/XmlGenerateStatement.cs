using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
	/// <summary>
	/// p457:
	/// The XML GENERATE statement converts data to XML format.
	/// </summary>
	public class XmlGenerateStatement : StatementElement
	{
		public XmlGenerateStatement() : base(CodeElementType.XmlGenerateStatement, StatementType.XmlGenerateStatement) { }

		/// <summary>
		/// pp458-459:
		/// identifier-1
		/// The receiving area for a generated XML document. identifier-1 must
		/// reference one of the following items:
		/// * An elementary data item of category alphanumeric
		/// * An alphanumeric group item
		/// * An elementary data item of category national
		/// * A national group item
		///
		/// When identifier-1 references a national group item, identifier-1 is processed
		/// as an elementary data item of category national. When identifier-1
		/// references an alphanumeric group item, identifier-1 is treated as though it
		/// were an elementary data item of category alphanumeric.
		///
		/// identifier-1 must not be described with the JUSTIFIED clause, and cannot be
		/// a function identifier. identifier-1 can be subscripted or reference modified.
		/// identifier-1 must not overlap identifier-2, identifier-3, codepage (if an
		/// identifier), identifier-4, or identifier-5.
		///
		/// The generated XML output is encoded as described in the documentation
		/// of the ENCODING phrase.
		///
		/// identifier-1 must reference a data item of category national, or the
		/// ENCODING phrase must specify 1208, if any of the following statements
		/// are true:
		/// * The CODEPAGE compiler option specifies an EBCDIC DBCS code page.
		/// * identifier-4 or identifier-5 references a data item of category national.
		/// * literal-4 or literal-5 is of category national.
		/// * The generated XML includes data from identifier-2 for:
		///   – Any data item of class national or class DBCS
		///   – Any data item with a DBCS name (that is, a data item whose name
		///     consists of DBCS characters)
		///   – Any data item of class alphanumeric that contains DBCS characters
		///
		/// identifier-1 must be large enough to contain the generated XML document.
		/// Typically, it must be from 5 to 10 times the size of identifier-2, depending
		/// on the length of the data-name or data-names within identifier-2. If
		/// identifier-1 is not large enough, an error condition exists at the end of the
		/// XML GENERATE statement.
		/// </summary>
		public ReceivingStorageArea ReceivingField { get; set; }

		/// <summary>
		/// p459:
		/// identifier-2
		/// The group or elementary data item to be converted to XML format.
		///
		/// If identifier-2 references a national group item, identifier-2 is processed as a
		/// group item. When identifier-2 includes a subordinate national group item,
		/// that subordinate item is processed as a group item.
		///
		/// identifier-2 cannot be a function identifier or be reference modified, but it
		/// can be subscripted.
		///
		/// identifier-2 must not overlap identifier-1 or identifier-3.
		/// identifier-2 must not specify the RENAMES clause.
		///
		/// The following data items that are specified by identifier-2 are ignored by
		/// the XML GENERATE statement:
		/// * Any subordinate unnamed elementary data items or elementary FILLER
		/// data items
		/// * Any slack bytes inserted for SYNCHRONIZED items
		/// * Any data item subordinate to identifier-2 that is described with the
		/// REDEFINES clause or that is subordinate to such a redefining item
		/// * Any data item subordinate to identifier-2 that is described with the
		/// RENAMES clause
		/// * Any group data item all of whose subordinate data items are ignored
		///
		/// All data items specified by identifier-2 that are not ignored according to the
		/// previous rules must satisfy the following conditions:
		/// * Each elementary data item must either have class alphabetic,
		/// alphanumeric, numeric, or national, or be an index data item. (That is,
		/// no elementary data item can be described with the USAGE POINTER,
		/// USAGE FUNCTION-POINTER, USAGE PROCEDURE-POINTER, or
		/// USAGE OBJECT REFERENCE phrase.)
		/// * There must be at least one such elementary data item.
		/// * Each non-FILLER data-name must be unique within any immediately
		/// superordinate group data item.
		/// * Any DBCS data-names, when converted to Unicode, must be legal as
		/// names in the XML specification, version 1.0. For details about the XML
		/// specification, see XML specification.
		/// </summary>
		public Variable DataItemToConvertToXml { get; set; }

		/// <summary>
		/// p460:
		/// If the COUNT IN phrase is specified, identifier-3 contains (after execution
		/// of the XML GENERATE statement) the count of generated XML character
		/// encoding units. If identifier-1 (the receiver) has category national, the count
		/// is in UTF-16 character encoding units. For all other encodings (including
		/// UTF-8), the count is in bytes.
		///
		/// identifier-3
		/// The data count field. Must be an integer data item defined without
		/// the symbol P in its picture string.
		/// identifier-3 must not overlap identifier-1, identifier-2, codepage (if an
		/// identifier), identifier-4, or identifier-5.
		/// </summary>
		public ReceivingStorageArea GeneratedXmlCharsCount { get; set; }

		/// <summary>
		/// pp460-461:
		/// The ENCODING phrase, if specified, determines the encoding of the
		/// generated XML document.
		///
		/// codepage
		/// Must be an unsigned integer data item or unsigned integer literal
		/// and must represent a valid coded character set identifier (CCSID).
		///
		/// Must identify one of the code pages supported for COBOL XML
		/// processing as described in The encoding of XML documents in the
		/// Enterprise COBOL Programming Guide.
		///
		/// If identifier-1 references a data item of category national, codepage
		/// must specify 1200, the CCSID for Unicode UTF-16.
		///
		/// If identifier-1 references a data item of category alphanumeric,
		/// codepage must specify 1208 or the CCSID of a supported EBCDIC
		/// code page as listed in The encoding of XML documents in the
		/// Enterprise COBOL Programming Guide.
		///
		/// If codepage is an identifier, it must not overlap identifier-1 or
		/// identifier-3.
		///
		/// If the ENCODING phrase is omitted and identifier-1 is of category national,
		/// the document encoding is Unicode UTF-16, CCSID 1200.
		///
		/// If the ENCODING phrase is omitted and identifier-1 is of category
		/// alphanumeric, the XML document is encoded using the code page
		/// specified by the CODEPAGE compiler option in effect when the source
		/// code was compiled.
		/// </summary>
		public IntegerVariable CodePage { get; set; }

		/// <summary>
		/// p461:
		/// XML-DECLARATION phrase
		/// If the XML-DECLARATION phrase is specified, the generated XML
		/// document starts with an XML declaration that includes the XML version
		/// information and an encoding declaration.
		///
		/// If identifier-1 is of category national, the encoding declaration has the value
		/// UTF-16 (encoding="UTF-16").
		///
		/// If identifier-1 is of category alphanumeric, the encoding declaration is
		/// derived from the ENCODING phrase, if specified, or from the CODEPAGE
		/// compiler option in effect for the program if the ENCODING phrase is not
		/// specified. See the description of the ENCODING phrase for further details.
		/// For an example of the effect of coding the XML-DECLARATION phrase,
		/// see Generating XML output in the Enterprise COBOL Programming Guide.
		/// If the XML-DECLARATION phrase is omitted, the generated XML
		/// document does not include an XML declaration.
		/// </summary>
		public SyntaxProperty<bool> StartWithXMLDeclaration { get; set; }

		/// <summary>
		/// p461:
		/// ATTRIBUTES phrase
		/// If the ATTRIBUTES phrase is specified, each eligible item included in the
		/// generated XML document is expressed as an attribute of the XML element
		/// that corresponds to the data item immediately superordinate to that
		/// eligible item, rather than as a child element of the XML element. To be
		/// eligible, a data item must be elementary, must have a name other than
		/// FILLER, and must not specify an OCCURS clause in its data description
		/// entry.
		///
		/// If the TYPE phrase is specified for particular identifiers, the TYPE phrase
		/// takes precedence for those identifiers over the WITH ATTRIBUTES phrase.
		/// For an example of the effect of the ATTRIBUTES phrase, see Generating
		/// XML output in the Enterprise COBOL Programming Guide.
		/// </summary>
		public SyntaxProperty<bool> GenerateElementaryItemsAsAttributes { get; set; }

		/// <summary>
		/// pp461-462:
		/// Use the NAMESPACE phrase to identify a namespace for the generated
		/// XML document. If the NAMESPACE phrase is not specified, or if
		/// identifier-4 has length zero or contains all spaces, the element names of
		/// XML documents produced by the XML GENERATE statement are not in
		/// any namespace.
		///
		/// identifier-4, literal-4: The namespace identifier, which must be a
		/// valid Uniform Resource Identifier (URI) as defined in Uniform
		/// Resource Identifier (URI): Generic Syntax.
		///
		/// identifier-4 must reference a data item of category alphanumeric or national.
		///
		/// identifier-4 must not overlap identifier-1 or identifier-3.
		///
		/// literal-4 must be of category alphanumeric or national,
		/// and must not be a figurative constant.
		/// </summary>
		public AlphanumericVariable Namespace { get; set; }

		/// <summary>
		/// p462:
		/// Use the NAMESPACE-PREFIX phrase to qualify the start and end tag of
		/// each element in the generated XML document with a prefix.
		///
		/// If the NAMESPACE-PREFIX phrase is not specified, or if identifier-5 is of
		/// length zero or contains all spaces, the namespace specified by the
		/// NAMESPACE phrase specifies the default namespace for the document. In
		/// this case, the namespace declared on the root element applies by default to
		/// each element name in the document, including that of the root element.
		/// (Default namespace declarations do not apply directly to attribute names.)
		///
		/// If the NAMESPACE-PREFIX phrase is specified, and identifier-5 is not of
		/// length zero and does not contain all spaces, then the start and end tag of
		/// each element in the generated document is qualified with the specified
		/// prefix. The prefix should therefore preferably be short. When the XML
		/// GENERATE statement is executed, the prefix must be a valid XML name,
		/// but without the colon (:), as defined in Namespaces in XML 1.0. The prefix
		/// can have trailing spaces, which are removed before use.
		///
		/// identifier-5, literal-5: The namespace prefix, which serves as an alias
		/// for the namespace identifier.
		///
		/// identifier-5 must reference a data item of category alphanumeric or national.
		///
		/// identifier-5 must not overlap identifier-1 or identifier-3.
		///
		/// literal-5 must be of category alphanumeric or national,
		/// and must not be a figurative constant.
		/// </summary>
		public AlphanumericVariable NamespacePrefix { get; set; }

        /// <summary>
        /// p462:
        /// NAME phrase
        /// Allows you to supply element and attribute names.
        /// </summary>
        public XmlNameMapping[] XmlNameMappings { get; set; }		

		/// <summary>
		/// p462:
		/// TYPE phrase
		/// Allows you to control attribute and element generation.
		/// </summary>
		public XmlTypeMapping[] XmlTypeMappings { get; set; }
        
		/// <summary>
		/// p463:
		/// SUPPRESS phrase
		/// Allows you to identify items that are subordinate to identifier-2 and must
		/// be suppressed when generating the XML if they contain values that are
		/// specified in the WHEN clause. If the SUPPRESS phrase is specified,
		/// identifier-1 must be large enough to contain the generated XML document
		/// before any suppression.
		/// </summary>
		public XmlSuppressDirective[] XmlSuppressDirectives { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, ReceivingField, DataItemToConvertToXml, GeneratedXmlCharsCount,
                   CodePage, StartWithXMLDeclaration, GenerateElementaryItemsAsAttributes, Namespace, NamespacePrefix)
                   && this.ContinueVisitToChildren(astVisitor, XmlNameMappings, XmlTypeMappings, XmlSuppressDirectives);
        }
    }

    /// <summary>
    /// p462:
    /// NAME phrase
    /// Allows you to supply element and attribute names.
    /// </summary>
    public class XmlNameMapping : IVisitable
    {
        /// <summary>
        /// p462:
        /// identifier-6 must reference identifier-2 or one of its subordinate data items.
        /// It cannot be a function identifier and cannot be reference modified or
        /// subscripted. It must not specify any data item which is ignored by the
        /// XML GENERATE statement. For more information about identifier-2, see
        /// the description of identifier-2. If identifier-6 is specified more than once in
        /// the NAME phrase, the last specification is used.
        /// </summary>
        public Variable DataItemName { get; set; }

        /// <summary>
        /// p462:
        /// literal-6 must be an alphanumeric or national literal containing the
        /// attribute or element name to be generated in the XML document
        /// corresponding to identifier-6. It must be a valid XML local name. If literal-6
        /// is a national literal, identifier-1 must reference a data item of category
        /// national or the encoding phrase must specify 1208.
        /// </summary>
        public AlphanumericValue XmlNameToGenerate;

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return this.ContinueVisitToChildren(astVisitor, DataItemName, XmlNameToGenerate);
        }
    }

    /// <summary>
    /// p462:
    /// TYPE phrase
    /// Allows you to control attribute and element generation.
    /// </summary>
    public class XmlTypeMapping : IVisitable
    {
        /// <summary>
        /// p463:
        /// identifier-7 must reference an elementary data item that is subordinate to
        /// identifier-2. It cannot be a function identifier and cannot be reference
        /// modified or subscripted. It must not specify any data item which is
        /// ignored by the XML GENERATE statement. For more information about
        /// identifier-2, see the description of identifier-2. If identifier-7 is specified more
        /// than once in the TYPE phrase, the last specification is used.
        ///
        /// If the XML GENERATE statement also includes a WITH ATTRIBUTES
        /// phrase, the TYPE phrase has precedence for identifier-7.
        /// </summary>
        public Variable DataItemName { get; set; }

        public SyntaxProperty<XmlSyntaxType> XmlSyntaxTypeToGenerate { get; set; }

        public enum XmlSyntaxType
        {
            UNKNOWN,
            /// <summary>
            /// p463:
            /// When ATTRIBUTE is specified, identifier-7 must be eligible to be an XML
            /// attribute. identifier-7 is expressed in the generated XML as an attribute of
            /// the XML element immediately superordinate to identifier-7 rather than as
            /// a child element.
            /// </summary>
            ATTRIBUTE,
            /// <summary>
            /// p463:
            /// When ELEMENT is specified, identifier-7 is expressed in the generated
            /// XML as an element. The XML element name is derived from identifier-7
            /// and the element character content is derived from the converted content
            /// of identifier-7 as described in “Operation of XML GENERATE” on page 465.
            /// </summary>
            ELEMENT,
            /// <summary>
            /// p463:
            /// When CONTENT is specified, identifier-7 is expressed in the generated
            /// XML as element character content of the XML element that corresponds
            /// to the data item immediately superordinate to identifier-7. The value of
            /// the element character content is derived from the converted content of
            /// identifier-7 as described in “Operation of XML GENERATE” on page 465.
            ///
            /// When CONTENT is specified for multiple identifiers all corresponding
            /// to the same superordinate identifier, the multiple contributions to the
            /// element character content are concatenated.
            /// </summary>
            CONTENT
        }

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return this.ContinueVisitToChildren(astVisitor, DataItemName, XmlSyntaxTypeToGenerate);
        }
    }

    /// <summary>
    /// p463:
    /// SUPPRESS phrase
    /// Allows you to identify items that are subordinate to identifier-2 and must
    /// be suppressed when generating the XML if they contain values that are
    /// specified in the WHEN clause. If the SUPPRESS phrase is specified,
    /// identifier-1 must be large enough to contain the generated XML document
    /// before any suppression.
    /// </summary>
    public class XmlSuppressDirective : IVisitable
    {
        /// <summary>
        /// p463:
        /// With the generic-suppression-phrase, elementary items subordinate to
        /// identifier-2 that are not otherwise ignored by XML GENERATE operations
        /// are identified generically for potential suppression. Either items of class
        /// numeric, if the NUMERIC keyword is specified, or items that are not of
        /// class numeric, if the NONNUMERIC keyword is specified, or both, may be
        /// suppressed. If the ATTRIBUTE keyword is specified, only items that would
        /// be expressed in the generated XML document as an XML attribute are
        /// identified for potential suppression. If the ELEMENT keyword is specified,
        /// only items that would be expressed in the generated XML document as an
        /// XML element are identified.
        ///
        /// If multiple generic-suppression-phrase are specified, the effect is cumulative.
        /// </summary>        
        public SyntaxProperty<XmlSyntaxType> XmlSyntaxTypeToSuppress { get; set; }

        /// <summary>
        /// pp463-464:
        /// identifier-8 explicitly identifies items for potential suppression. It must
        /// reference an elementary data item that is subordinate to identifier-2 and that
        /// is not otherwise ignored by the XML GENERATE operations. For more
        /// information about that element, see the description of identifier-2. It cannot
        /// be a function identifier and cannot be reference modified or subscripted. If
        /// identifier-8 is specified more than once in the SUPPRESS phrase, the last
        /// specification is used. The explicit suppression specification for identifier-8
        /// overrides the suppression specification that is implied by any
        /// generic-suppression-phrase, if identifier-8 is also one of the identifiers
        /// generically identified.
        /// * If ZERO, ZEROES, or ZEROS is specified in the WHEN phrase,
        /// identifier-8 or all the data items that are identified by the
        /// generic-suppression-phrase must not be of USAGE DISPLAY-1.
        /// * If SPACE or SPACES is specified in the WHEN phrase, identifier-8 or all
        /// the data items that are identified by the generic-suppression-phrase must
        /// be of USAGE DISPLAY, DISPLAY-1 or NATIONAL.
        /// * If LOW-VALUE, LOW-VALUES, HIGH-VALUE, or HIGH-VALUES is
        /// specified in the WHEN phrase, identifier-8 or all the data items that are
        /// identified by the generic-suppression-phrase must be of class alphanumeric
        /// or national.
        /// </summary>
        public Variable DataItemName { get; set; }

        /// <summary>
        /// p464:
        /// The comparison operation that determines if an item will be suppressed is
        /// a numeric comparison if the value specified is ZERO, ZEROS, or ZEROES,
        /// and the item is of category numeric or internal floating-point. For all other
        /// cases, the comparison operation is an alphanumeric, DBCS, or national
        /// comparison depending on whether the item is of usage DISPLAY,
        /// DISPLAY-1 or NATIONAL respectively.
        ///
        /// When the SUPPRESS phrase is specified, a group item subordinate to
        /// identifier-2 is suppressed in the generated XML document if all the eligible
        /// items subordinate to the group item are suppressed or if the group item
        /// has zero length. The root element is always generated, even if all the items
        /// subordinate to identifier-2 are suppressed.
        /// </summary>
        public RepeatedCharacterValue[] ItemValuesToSuppress;

        public enum XmlSyntaxType
        {
            UNKNOWN,
            NUMERIC_ATTRIBUTE,
            NUMERIC_ELEMENT,
            NONNUMERIC_ATTRIBUTE,
            NONNUMERIC_ELEMENT,
            ATTRIBUTE,
            ELEMENT,
        }

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return this.ContinueVisitToChildren(astVisitor, XmlSyntaxTypeToSuppress, DataItemName)
                && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) ItemValuesToSuppress);
        }
    }
}
