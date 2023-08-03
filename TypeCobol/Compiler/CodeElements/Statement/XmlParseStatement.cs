using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p469:
    /// The XML PARSE statement is the COBOL language interface to the high-speed
    /// XML parser that is part of the COBOL run time.
    /// The XML PARSE statement parses an XML document into its individual pieces and
    /// passes each piece, one at a time, to a user-written processing procedure.
    /// </summary>
    public class XmlParseStatement : StatementElement
    {
        public XmlParseStatement() : base(CodeElementType.XmlParseStatement, StatementType.XmlParseStatement) { }

        /// <summary>
        /// p469:
        /// identifier-1
        /// identifier-1 must be an elementary data item of category national, a national
        /// group, an elementary data item of category alphanumeric, or an
        /// alphanumeric group item. identifier-1 cannot be a function-identifier.
        /// identifier-1 contains the XML document character stream.
        ///
        /// If identifier-1 is a national group item, identifier-1 is processed as an
        /// elementary data item of category national.
        ///
        /// If identifier-1 is of category national, its content must be encoded using
        /// Unicode UTF-16BE (CCSID 1200). identifier-1 must not contain any
        /// character entities that are represented using multiple encoding units. Use a
        /// character reference to represent any such characters, for example:
        /// * "&#x67603;" or
        /// * "&#x10813;"
        /// The letter x must be lowercase.
        ///
        /// If identifier-1 is of category alphanumeric, its content must be encoded
        /// using one of the character sets listed in Coded character sets for XML
        /// documents in the Enterprise COBOL Programming Guide. If identifier-1 is
        /// alphanumeric and contains an XML document that does not specify an
        /// encoding declaration, the XML document is parsed with the code page
        /// specified by the CODEPAGE compiler option.
        /// </summary>
        public Variable XmlTextToParse { get; set; }

        /// <summary>
        /// pp470-471:
        /// ENCODING phrase
        /// The ENCODING phrase specifies an encoding that is assumed for the
        /// source XML document in identifier-1. codepage must be an unsigned integer
        /// data item or an unsigned integer literal that represents a valid coded
        /// character set identifier (CCSID). The ENCODING phrase specification
        /// overrides the encoding specified by the CODEPAGE compiler option. The
        /// encoding specified in any XML declaration is always ignored.
        ///
        /// If identifier-1 references a data item of category national, codepage must
        /// specify CCSID 1200, for Unicode UTF-16.
        ///
        /// If identifier-1 references a data item of category alphanumeric, codepage
        /// must specify CCSID 1208 for UTF-8 or a CCSID for a supported EBCDIC
        /// or ASCII codepage. See Coded character sets for XML documents in the
        /// Enterprise COBOL Programming Guide for details.
        /// </summary>
        public IntegerVariable CodePage { get; set; }

        /// <summary>
        /// p470:
        /// RETURNING NATIONAL phrase
        /// When identifier-1 references a data item of category alphanumeric and the
        /// RETURNING NATIONAL phrase is specified, XML document fragments
        /// are automatically converted to Unicode UTF-16 representation and
        /// returned to the processing procedure in the national special registers
        /// XML-NTEXT, XML-NNAMESPACE, and XML-NNAMESPACE-PREFIX.
        ///
        /// When the RETURNING NATIONAL phrase is not specified and identifier-1
        /// references a data item of category alphanumeric, the XML document
        /// fragments are returned to the processing procedure in the alphanumeric
        /// special registers XML-TEXT, XML-NAMESPACE, and XML-NAMESPACEPREFIX.
        ///
        /// When identifier-1 references a national data item, XML document fragments
        /// are always returned in Unicode UTF-16 representation in the national
        /// special registers XML-NTEXT, XML-NNAMESPACE, and
        /// XML-NNAMESPACE-PREFIX.
        /// </summary>
        public SyntaxProperty<bool> ReturningNational { get; set; }
        public bool IsReturningNational  { get { return ReturningNational != null && ReturningNational.Value; } }

        /// <summary>
        /// p470:
        /// VALIDATING phrase
        /// The VALIDATING phrase specifies that the parser should validate the
        /// XML document against an XML schema while parsing it. In Enterprise
        /// COBOL, the schema used for XML validation is in a preprocessed format
        /// known as Optimized Schema Representation or OSR.
        ///
        /// See Parsing XML documents with validation in the Enterprise COBOL
        /// Programming Guide for details.
        ///
        /// If the FILE keyword is not specified, identifier-2 must reference a data item
        /// that contains the optimized XML schema. identifier-2 must be of category
        /// alphanumeric and cannot be a function-identifier.
        /// </summary>
        public Variable OptimizedXmlSchema { get; set; }

        /// <summary>
        /// p470:
        /// VALIDATING phrase
        /// The VALIDATING phrase specifies that the parser should validate the
        /// XML document against an XML schema while parsing it. In Enterprise
        /// COBOL, the schema used for XML validation is in a preprocessed format
        /// known as Optimized Schema Representation or OSR.
        ///
        /// See Parsing XML documents with validation in the Enterprise COBOL
        /// Programming Guide for details.
        //
        /// If the FILE keyword is specified, xml-schema-name-1 identifies an existing
        /// z/OS UNIX file or MVS data set that contains the optimized XML schema.
        /// xml-schema-name-1 must be associated with the external file name of the
        /// schema by using the XML-SCHEMA clause. For more information about
        /// the XML-SCHEMA clause, see “SPECIAL-NAMES paragraph” on page 112.
        /// </summary>
        public SymbolReference OptimizedXmlSchemaFile { get; set; }

        /// <summary>
        /// p471:
        /// PROCESSING PROCEDURE phrase
        /// Specifies the name of a procedure to handle the various events that the
        /// XML parser generates.
        ///
        /// procedure-name-1, procedure-name-2
        /// Must name a section or paragraph in the PROCEDURE DIVISION.
        /// 
        /// procedure-name-1
        /// Specifies the first (or only) section or paragraph in the processing
        /// procedure.
        /// </summary>
        public SymbolReference ProcessingProcedure { get; set; }

        /// <summary>
        /// procedure-name-2
        /// Specifies the last section or paragraph in the processing procedure.
        /// 
        /// When both procedure-name-1 and procedure-name-2 are specified, if
        /// either is a procedure name in a declarative procedure, both must
        /// be procedure names in the same declarative procedure.
        /// </summary>
        public SymbolReference ThroughProcessingProcedure { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, CodePage, ReturningNational, OptimizedXmlSchema,
                   OptimizedXmlSchemaFile, ProcessingProcedure, ThroughProcessingProcedure);
        }
    }
}
