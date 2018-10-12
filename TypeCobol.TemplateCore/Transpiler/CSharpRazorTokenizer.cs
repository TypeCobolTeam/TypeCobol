using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web.Razor.Parser.SyntaxTree;
using System.Web.Razor.Text;
using System.Web.Razor.Tokenizer;
using System.Web.Razor.Tokenizer.Symbols;

namespace TypeCobol.TemplateCore.Transpiler
{
    /// <summary>
    /// This class is the specialization of a C# Razor Tokenizer.
    /// </summary>
    public class CSharpRazorTokenizer : CSharpTokenizer
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="source">The source document</param>
        public CSharpRazorTokenizer(ITextDocument source) : base(source)
        {

        }
        /// <summary>
        /// This method is called to create a C# Symbol as a token
        /// </summary>
        /// <param name="start">Start position in the source document of the Token</param>
        /// <param name="content">the lexeme of the token</param>
        /// <param name="type">The Token's type</param>
        /// <param name="errors">Any encountered error</param>
        /// <returns>The Token's symbol</returns>
        protected override CSharpSymbol CreateSymbol(SourceLocation start, string content, CSharpSymbolType type,
            IEnumerable<RazorError> errors)
        {
            CSharpSymbol symbol = base.CreateSymbol(start, content, type, errors);
            return symbol;
        }
    }
}
