using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Schema;

namespace TypeCobol.TemplateCore.SaxParser
{
    /// <summary>
    /// Base class of a SaxParser
    /// </summary>
    public abstract class SaxParser
    {

        /// <summary>
        /// Parsing Exception
        /// </summary>
        public class ParsingException : Exception
        {
            public ParsingException()
            {
            }
            public ParsingException(String msg)
                : base(msg)
            {
            }
        }

        /// <summary>
        /// Constructor file
        /// </summary>
        public SaxParser(String xmlFile)
        {
            XmlFile = xmlFile;
            Scanner = new SaxScanner(XmlReader.Create(xmlFile));

        }

        /// <summary>
        /// Constructor file with a schema to apply.
        /// </summary>
        public SaxParser(String xmlFile, String schema)
        {
            XmlFile = xmlFile;
            XmlReaderSettings settings = new XmlReaderSettings();
            settings.Schemas.Add(null, schema);
            settings.ValidationFlags |= XmlSchemaValidationFlags.ProcessInlineSchema;
            settings.ValidationFlags |= XmlSchemaValidationFlags.ProcessSchemaLocation;
            settings.ValidationFlags |= XmlSchemaValidationFlags.ReportValidationWarnings;            
            settings.ValidationType = ValidationType.Schema;
            settings.ValidationEventHandler += XMLValidationEventHandler;

            Scanner = new SaxScanner(XmlReader.Create(xmlFile, settings));

        }

        /// <summary>
        /// The Xml File
        /// </summary>
        public String XmlFile
        {
            get;
            protected set;
        }

        /// <summary>
        /// The Scanner
        /// </summary>
        public SaxScanner Scanner
        {
            get;
            protected set;
        }

        /// <summary>
        /// The Lookahed Token.
        /// </summary>
        public SaxToken? Lookahead
        {
            get;
            set;
        }

        /// <summary>
        /// Matches the current Lookahead Sax Token with the given XmlNode's type
        /// </summary>
        /// <param name="type">The Xml Node's type to match</param>
        /// <returns>The matched token</returns>
        /// <exception cref="ParsingException">If there is no matching</exception>
        protected SaxToken? Match(XmlNodeType type)
        {
            SaxToken? token = Lookahead;
            if (Lookahead != null)
            {
                if (Lookahead.Value.type == type)
                {
                    NextToken();
                    return token;
                }
            }
            throw new ParsingException(String.Format(Resource.ParserErrorExpectedToken, type.ToString()));
        }

        /// <summary>
        /// Matches the current Lookahead Sax Token with the given XmlNode's type and name
        /// </summary>
        /// <param name="type">The Xml Node's type to match</param>
        /// <param name="name">The Xml Node's name to match</param>
        /// <returns>The matched token</returns>
        /// <exception cref="ParsingException">If there is no matching</exception>
        protected SaxToken? Match(XmlNodeType type, String name)
        {
            SaxToken? token = OptionalMatch(type, name);
            if (token != null)
            {
                return token;
            }
            throw new ParsingException(String.Format(Resource.ParserErrorExpected, name));
        }

        /// <summary>
        /// Matches the current Lookahead Sax Optional Token with the given XmlNode's type and name
        /// </summary>
        /// <param name="type">The Xml Node's type to match</param>
        /// <param name="name">The Xml Node's name to match</param>
        /// <returns>The Matched token, null otherwise.</returns>
        protected SaxToken? OptionalMatch(XmlNodeType type, String name)
        {
            SaxToken? token = Lookahead;
            if (Lookahead != null)
            {
                if (Lookahead.Value.type == type && Lookahead.Value.name.Equals(name))
                {
                    NextToken();
                    return token;
                }
            }
            return null;
        }

        /// <summary>
        /// Test That the given Xml Node's type matches the current Lookahead
        /// </summary>
        /// <param name="type">Xml Node's type</param>
        /// <returns>Return true if the lookahead matches, false otherwise.</returns>
        protected bool TestLookahead(XmlNodeType type)
        {
            if (Lookahead == null)
                return false;
            return Lookahead.Value.type == type;
        }

        /// <summary>
        /// Test That the given Xml Node's type and name matches the current Lookahead
        /// </summary>
        /// <param name="type">Xml Node's type</param>
        /// <param name="name">Xml Node's name</param>
        /// <returns>Return true if the lookahead matches, false otherwise.</returns>
        protected bool TestLookahead(XmlNodeType type, String name)
        {
            if (Lookahead == null)
                return false;
            return Lookahead.Value.type == type && Lookahead.Value.name.Equals(name);
        }

        /// <summary>
        /// Set the LookaHead to the the Next Token
        /// </summary>
        protected void NextToken()
        {
            Lookahead = Scanner.NextToken;
        }

        /// <summary>
        /// The parsing function
        /// </summary>
        public abstract void Parse();

        /// <summary>
        /// Validate the XML file by Scanning the entire document.
        /// </summary>
        /// <returns>Return true if validation is ok, false otherwise</returns>
        /// <remarks>If the Validation was not ok, the Properties ValidationWarningCount and ValidationErrorCount will give the count of warning and errors.</remarks>
        public abstract bool Validate();

        /// <summary>
        /// The Count of Schema Validation warnings
        /// </summary>
        public int ValidationWarningCount
        {
            get;
            private set;
        }

        /// <summary>
        /// The Count of Schema Validation errors
        /// </summary>
        public int ValidationErrorCount
        {
            get;
            private set;
        }

        /// <summary>
        /// Validation Error Event Handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected virtual void XMLValidationEventHandler(object sender, ValidationEventArgs e)
        {
            if (ValidationMessage == null)
                ValidationMessage = new StringBuilder();
            if (e.Severity == XmlSeverityType.Warning)
            {
                var xml_e = e.Exception;
                String msg = String.Format(Resource.WarningMsg, e.Message, xml_e.LineNumber);
                ValidationMessage.AppendLine(msg);
                ValidationWarningCount++;
            }
            else if (e.Severity == XmlSeverityType.Error)
            {
                var xml_e = e.Exception;
                String msg = String.Format(Resource.ErrorMsg, e.Message, xml_e.LineNumber);
                ValidationMessage.AppendLine(msg);
                ValidationErrorCount++;
            }
        }

        /// <summary>
        /// Validation Message.
        /// </summary>
        public StringBuilder ValidationMessage
        {
            get;
            set;
        }
    }
}
