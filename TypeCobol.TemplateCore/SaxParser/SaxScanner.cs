using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;

namespace TypeCobol.TemplateCore.SaxParser
{
    /// <summary>
    /// A Sax Scanner.
    /// </summary>
    public class SaxScanner
    {
        /// <summary>
        /// The Constructore
        /// </summary>
        /// <param name="reader">The reade</param>
        public SaxScanner(XmlReader reader)
        {
            Reader = reader;
        }

        /// <summary>
        /// The Xml Reader Instance.
        /// </summary>
        public XmlReader Reader
        { get; protected set; }

        /// <summary>
        /// Getter on the next Token.
        /// </summary>
        public SaxToken? NextToken
        {
            get
            {
                bool has_token = Reader.Read();
                //Ignore Whitespace and comment.
                while (has_token && (Reader.NodeType == XmlNodeType.Whitespace ||
                    Reader.NodeType == XmlNodeType.SignificantWhitespace ||
                    Reader.NodeType == XmlNodeType.Comment))
                {
                    has_token = Reader.Read();
                }
                if (!has_token)
                    return null;
                SaxToken token = new SaxToken();
                token.type = Reader.NodeType;
                token.name = Reader.Name;
                token.value = Reader.Value;
                switch (Reader.NodeType)
                {
                    case XmlNodeType.Element:
                    case XmlNodeType.XmlDeclaration:
                        token.attributes = new Dictionary<String, String>();
                        if (Reader.MoveToFirstAttribute())
                        {
                            do
                            {
                                String name = Reader.Name;
                                String value = Reader.Value;
                                token.attributes[name] = value;
                            } while (Reader.MoveToNextAttribute());
                        }
                        break;
                    case XmlNodeType.EndElement:
                        break;
                    case XmlNodeType.ProcessingInstruction:
                        //example
                        //<?xml version="1.0" encoding="utf-8" ?>
                        token.attributes = new Dictionary<String, String>();
                        if (Reader.MoveToFirstAttribute())
                        {
                            do
                            {
                                String name = Reader.Name;
                                String value = Reader.Value;
                                token.attributes[name] = value;
                            } while (Reader.MoveToNextAttribute());
                        }
                        break;
                    case XmlNodeType.Attribute:
                        break;
                }
                return token;
            }
        }
    }
}
