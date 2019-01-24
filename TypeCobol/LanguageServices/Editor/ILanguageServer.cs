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
        /// Event handler when some tokens lines has changed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="eventArgs"></param>
        void TokensLinesChanged(object sender, EventArgs eventArgs);

        /// <summary>
        /// Event Handler when the whole document has changed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="eventArgs"></param>
        void WholeDocumentChanged(object sender, EventArgs eventArgs);
    }
}
