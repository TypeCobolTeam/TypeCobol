using System;
using TypeCobol.LanguageServer.Context;
using TypeCobol.LanguageServices.Editor;

namespace TypeCobol.LanguageServer.Interfaces
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
        /// Method to update the Outline data.
        /// </summary>
        /// <param name="programClassDocument"></param>
        /// <param name="bForced"></param>
        TypeCobolCustomLanguageServerProtocol.RefreshOutlineParams UpdateOutline(Compiler.Parser.ProgramClassDocument programClassDocument, bool bForced);

        /// <summary>
        /// Method to update CFG/DFA information.
        /// </summary>
        /// <param name="fileCompiler">The underlying File Compiler</param>
        /// <param name="writeToFile">True if CfgDfaParams shall write the dot content to a temporary file, 
        /// false if the dot content shall be put in the CfgDfaParams.dotContent field.</param>
        /// <returns>CFG/DFA Data information</returns>
        TypeCobolCustomLanguageServerProtocol.CfgDfaParams UpdateCfgDfaInformation(DocumentContext docContext, bool writeToFile);

        /// <summary>
        /// Event Handler when the whole document has changed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="eventArgs"></param>
        void WholeDocumentChanged(object sender, EventArgs eventArgs);
    }
}
