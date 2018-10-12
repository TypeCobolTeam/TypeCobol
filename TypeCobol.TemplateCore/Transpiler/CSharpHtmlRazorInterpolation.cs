using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web.Razor.Parser;
using System.Web.Razor.Parser.SyntaxTree;
using System.Web.Razor.Text;
using System.Web.Razor.Tokenizer;
using System.Web.Razor.Tokenizer.Symbols;

namespace TypeCobol.TemplateCore.Transpiler
{
    /// <summary>
    /// The C#/HTML parser visitor to create interpolation strings.
    /// </summary>
    public class CSharpHtmlRazorInterpolation : BaseParserVisitor
    {
        //
        // Summary:
        //     Visits the specified block before parsing.
        //
        // Parameters:
        //   block:
        //     The block to visit.
        public override void VisitStartBlock(Block block)
        {
            base.VisitStartBlock(block);
        }

        //
        // Summary:
        //     Visits the specified block.
        //
        // Parameters:
        //   block:
        //     The block to visit.
        public override void VisitBlock(Block block)
        {
            base.VisitBlock(block);
        }
        //
        // Summary:
        //     Visits the specified black after parsing.
        //
        // Parameters:
        //   block:
        //     The block to visit.
        public override void VisitEndBlock(Block block)
        {
            base.VisitEndBlock(block);
        }

        //
        // Summary:
        //     .Visits the specified span.
        //
        // Parameters:
        //   span:
        //     The span to visit.
        public override void VisitSpan(Span span)
        {
            base.VisitSpan(span);
            switch (span.Kind)
            {
                case SpanKind.Transition:
                    break;
                case SpanKind.MetaCode:
                    break;
                case SpanKind.Comment:
                    break;
                case SpanKind.Code:
                    break;
                case SpanKind.Markup:
                    break;
            }
            string content = span.Content;
            SeekableTextReader reader = new SeekableTextReader(new StringReader(content));
            CSharpRazorTokenizer tokenizer = new CSharpRazorTokenizer(reader);
            CSharpSymbol csSym = null;
            while ((csSym = tokenizer.NextSymbol()) != null)
            {
                System.Diagnostics.Debug.Write(csSym.Content);
            }
        }
    }
}
