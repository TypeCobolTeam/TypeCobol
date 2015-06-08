using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The XML-SCHEMA clause provides the means of relating xml-schema-name-1 to an
    /// external file identifier: a ddname or environment variable that identifies the actual
    /// external file that contains the optimized XML schema.
    /// xml-schema-name-1 can be referenced only in an XML PARSE statement.
    /// </summary>
    public class XmlSchemaName : Symbol
    {
        public XmlSchemaName(Token userDefinedWord) :
            base(userDefinedWord, SymbolType.XmlSchemaName)
        { }
    }
}
