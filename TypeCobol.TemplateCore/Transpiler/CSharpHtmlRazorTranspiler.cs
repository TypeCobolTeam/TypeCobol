using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web.Razor.Parser;

namespace TypeCobol.TemplateCore.Transpiler
{
    /// <summary>
    /// This the C#/HTML Razor Script transpiler.
    /// </summary>
    public class CSharpHtmlRazorTranspiler : RazorTranspiler
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parserVisitor">The Parser visitor on recognized Razor tree elements</param>
        public CSharpHtmlRazorTranspiler(BaseParserVisitor parserVisitor) : base(new CSharpCodeParser(), new HtmlMarkupParser(), parserVisitor)
        {

        }
    }
}
