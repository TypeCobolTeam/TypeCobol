using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web.Razor.Parser;
using System.Web.Razor.Parser.SyntaxTree;

namespace TypeCobol.TemplateCore.Transpiler
{
    /// <summary>
    /// This is the Basic Parser Visitor that only collects errors.
    /// </summary>
    public class BaseParserVisitor : ParserVisitor
    {
        /// <summary>
        /// Razor Errors.
        /// </summary>
        public List<RazorError> Errors
        { get; private set; }

        /// <summary>
        /// Empty constructor
        /// </summary>
        public BaseParserVisitor()
        {
            Errors = new List<RazorError>();
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
        //     Visits the given razor error.
        //
        // Parameters:
        //   err:
        //     The error to visit.
        public override void VisitError(RazorError err)
        {
            base.VisitError(err);
            Errors.Add(err);
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
        }
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
    }
}
