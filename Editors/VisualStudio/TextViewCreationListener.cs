using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;

namespace TypeCobol.Editor
{
    [Export(typeof(IWpfTextViewCreationListener))] // Make our class visible to Visual Studio
    [ContentType("text")] // Types of files the code formatter should be bound to the text editor
    [TextViewRole(PredefinedTextViewRoles.Editable)] // change editable code and not the colors or adornments around it
    internal sealed class TextViewCreationListener : IWpfTextViewCreationListener
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="textView">Instance of text editor that has been created.</param>
        public void TextViewCreated(IWpfTextView textView)
        {
            new Formatter(textView);
        }
    }
}
