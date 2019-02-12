using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Symbols
{
    /// <summary>
    /// A Symbol that represents a Cobol Paragraph
    /// </summary>
    public class ParagraphSymbol : Symbol
    {
        /// <summary>
        /// Named Constructor
        /// </summary>
        /// <param name="name"></param>
        public ParagraphSymbol(String name)
            : base(name, Kinds.Paragraph)
        {
        }
    }
}
