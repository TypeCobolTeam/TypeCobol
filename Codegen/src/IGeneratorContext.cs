using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// Interface of any instance that can have is Generated context to be set.
    /// </summary>
    public interface IGeneratorContext
    {
        IGenerator Generator { set; }
        /// <summary>
        /// A Source text buffer if any
        /// </summary>
        Compiler.Source.SourceText SourceTextBuffer { set; }
        /// <summary>
        /// Determine if yes or no the SourceTextBuffer property refers a global source text buffer or a local source text buffer.
        /// </summary>
        bool IsGlobalSourceTextBuffer { set; }
    }
}
