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
        /// <summary>
        /// The resulting interpolation String
        /// </summary>
        public StringBuilder InterpolationString
        {
            get;
            set;
        }

        /// <summary>
        /// The Code String
        /// </summary>
        public StringBuilder CodeString
        {
            get;
            set;
        }

        /// <summary>
        /// Mixed code: All codes are mixed in this buffer
        /// </summary>
        public StringBuilder MixedCodeInterpolationString
        {
            get;
            set;
        }

        /// <summary>
        /// The Real C# interpolation String literal using $ and @ special string characters.
        /// </summary>
        public string CSharpInterpolationString
        {
            get
            {
                StringBuilder body = new StringBuilder();                                
                body.Append(@"$@""");
                body.Append(InterpolationString.ToString());
                body.Append(@"""");
                return body.ToString();
            }
        }

        /// <summary>
        /// MetaCode @{... } toogle flag.
        /// </summary>
        private bool MetaCodeToggleFlag;

        /// <summary>
        /// Constructor.
        /// </summary>
        public CSharpHtmlRazorInterpolation()
        {
            InterpolationString = new StringBuilder();
            CodeString = new StringBuilder();
            MixedCodeInterpolationString = new StringBuilder();
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
                    MetaCodeToggleFlag = !MetaCodeToggleFlag;
                    break;
                case SpanKind.Comment:
                    if (MetaCodeToggleFlag)
                    {
                        this.CodeString.Append(span.Content);                        
                    }
                    else
                    {
                        this.InterpolationString.Append(span.Content);
                    }
                    this.MixedCodeInterpolationString.Append(span.Content);
                    break;
                case SpanKind.Code:
                    if (MetaCodeToggleFlag)
                    {
                        this.CodeString.Append(span.Content);
                        this.MixedCodeInterpolationString.Append(span.Content);
                    }
                    else
                    {   //Code translated to argument of an interpolation String.                        
                        this.InterpolationString.Append("{@");
                        this.InterpolationString.Append(span.Content);
                        this.InterpolationString.Append('}');

                        //For the mixed argument of an interpoation string of Appended in a Result Buffer
                        this.MixedCodeInterpolationString.Append("@SelfResult.Append(");
                        this.MixedCodeInterpolationString.Append(@"$@""{@");
                        this.MixedCodeInterpolationString.Append(span.Content);
                        this.MixedCodeInterpolationString.Append('}');
                        this.MixedCodeInterpolationString.Append(@"""");
                        this.MixedCodeInterpolationString.Append(");");
                    }
                    break;
                case SpanKind.Markup:
                    //Markup always goes in the interpolation string
                    this.InterpolationString.Append(span.Content);
                    this.MixedCodeInterpolationString.Append("@SelfResult.Append(");
                    this.MixedCodeInterpolationString.Append(@"@""");
                    this.MixedCodeInterpolationString.Append(span.Content);
                    this.MixedCodeInterpolationString.Append(@"""");
                    this.MixedCodeInterpolationString.Append(");");
                    break;
            }
            //DumpTokens(span.Content);
        }

        /// <summary>
        /// Static method for dumping the tokenization of a string content.
        /// </summary>
        /// <param name="content">The string content to dump the tokenization</param>
        public static void DumpTokens(string content)
        {
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
