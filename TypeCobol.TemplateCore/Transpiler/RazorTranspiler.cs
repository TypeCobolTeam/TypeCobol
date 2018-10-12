using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web.Razor.Parser;

namespace TypeCobol.TemplateCore.Transpiler
{
    /// <summary>
    /// This the Razor Transpiler class
    /// </summary>
    public class RazorTranspiler
    {
        /// <summary>
        /// The underlying Razor parser built on the CodeParser and the MarkupParser
        /// </summary>
        public RazorParser Parser
        {
            get;
            protected set;
        }

        /// <summary>
        /// The Underlying parser visitor on recognized Razor tree elements during the parsing of a script.
        /// </summary>
        public BaseParserVisitor Visitor
        {
            get;
            protected set;
        }

        /// <summary>
        /// Constructoe
        /// </summary>
        /// <param name="codeParser">The Script source code parser instance</param>
        /// <param name="markupParser">The Markup parser</param>
        /// <param name="parserVisitor">The Parser visitor on recognized Razor tree elements</param>
        public RazorTranspiler(ParserBase codeParser, ParserBase markupParser, BaseParserVisitor parserVisitor)
        {
            if (codeParser == null || markupParser == null)
            {
                throw new NullReferenceException(codeParser == null ? "codeParser" : "markupParser");
            }
            Parser = new RazorParser(codeParser, markupParser);
            Visitor = parserVisitor == null ? new BaseParserVisitor() : parserVisitor;
        }

        /// <summary>
        /// Parses the given script
        /// </summary>
        /// <param name="script">The Script to be parsed</param>
        /// <returns>true if ok, false in case of errors the Vistor.Error list will contains all encounered errors.</returns>
        public bool Parse(String script)
        {
            return Parse(new StringReader(script));            
        }

        /// <summary>
        /// Parses the given script
        /// </summary>
        /// <param name="script">The Script to be parsed</param>
        /// <returns>true if ok, false in case of errors the Vistor.Error list will contains all encountered errors.</returns>
        public bool Parse(TextReader script)
        {
            Parser.Parse(script, Visitor);
            return Visitor.Errors.Count == 0;
        }
    }
}
