using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServices.Editor
{
    /// <summary>
    /// Any Language Server instance, that can communicate with a Client must implements this interface.
    /// </summary>
    public interface ILanguageServer : ILanguageEditor
    {
        /// <summary>
        /// Are we supporting Syntax Coloring Notifications.    
        /// </summary>
        bool UseSyntaxColoring { get; set; }

        /// <summary>
        /// Called when a token scanning has been performed.
        /// </summary>
        /// <param name="changes">The list of document change instances, if this parameter is null then the whole document has been rescanned.</param>
        /// <param name="compilationDocument">The underlying CompilationDocument instance</param>
        void UpdateTokensLines(IList<DocumentChange<ITokensLine>> changes, CompilationDocument compilationDocument);
    }
}
