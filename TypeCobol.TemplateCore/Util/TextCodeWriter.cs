using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Util
{
    /// <summary>
    /// Specialized IndentedTextWriter for Indent/Outdent
    /// </summary>
    public class TextCodeWriter : System.CodeDom.Compiler.IndentedTextWriter
    {
        public TextCodeWriter(TextWriter writer) : base(writer)
        {            
        }

        public TextCodeWriter(TextWriter writer, string tabString) : base(writer, tabString)
        {
        }

        //Indent
        public new void Indent()
        {
            base.Indent += System.CodeDom.Compiler.IndentedTextWriter.DefaultTabString.Length;
        }

        /// <summary>
        /// Oudenting
        /// </summary>
        public void Outdent()
        {
            base.Indent -= System.CodeDom.Compiler.IndentedTextWriter.DefaultTabString.Length;
        }
    }
}
