using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Text changes are tracked at the line level.
    /// This class models a change which can span several lines of text.
    /// </summary>
    public class TextChangedEvent : EventArgs
    {
        public TextChangedEvent()
        {
            TextChanges = new List<TextChange>();
        }

        /// <summary>
        /// List of text lines which were simultaneoulsy changed in the document
        /// </summary>
        public IList<TextChange> TextChanges { get; private set; }

    }
}
