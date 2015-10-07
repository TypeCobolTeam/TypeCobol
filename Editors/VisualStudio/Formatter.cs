using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using System;

namespace TypeCobol.Editor
{
    struct Config
    {
        public bool CompileAfterWhitespace { get; set; }
        public double CompileMsAfterType { get; set; }
    }

    class Formatter
    {
        public Config Config { get; set; }

        private IWpfTextView view { get; set; }
        private bool isEditing;
        private bool isDirty;
        private Timer timer;

        public Formatter(IWpfTextView view) {
            this.view = view;
            this.view.TextBuffer.Changed += new EventHandler<TextContentChangedEventArgs>(OnChanged);
            this.view.TextBuffer.PostChanged += new EventHandler(PostChanged);

            this.Config = new Config {
                    CompileAfterWhitespace = false,
                    CompileMsAfterType = 500,
                };

            if (this.Config.CompileMsAfterType > 0)
                this.timer = new Timer(Compile, this.Config.CompileMsAfterType);
        }

        /// <summary>
        /// Called every time an edit has been made: typing a char, pasting code or programmatical changes.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnChanged(object sender, TextContentChangedEventArgs e)
        {
            // because our method also reacts on programmatical changes, use a bool to determine 
            // if either our extension or the user / anything else is changing the code at the moment 
            // and call the formatting method only if our extension is not already editing.
            // Otherwise this method would be called recursively, thus crashing V$
            if (!isEditing)
            {
                isEditing = true;
                FormatCode(e);
                timer.start();
            }
        }

        private void PostChanged(object sender, EventArgs e)
        {
            isEditing = false;
        }

        /// <summary>
        /// Handle the "changed" event.
        /// Edits are stored in the array e.Changes of the type INormalizedTextChangeCollection.
        /// </summary>
        /// <param name="e">What has changed since the last edit.</param>
        private void FormatCode(TextContentChangedEventArgs e)
        {
            if (e.Changes != null)
            {
                isDirty = e.Changes.Count > 0;
                foreach (var c in e.Changes)
                {
                    //DumpChange(c);
                    //HandleChange(c);
                    if (Config.CompileAfterWhitespace) CompileIfWhitespace(c);
                }
            }
        }

        private void DumpChange(ITextChange c)
        {
            System.Text.StringBuilder s = new System.Text.StringBuilder();
            s.AppendLine(">>>>> HandleChange:");
            s.Append("Delta=").Append(c.Delta).Append("   LineCountDelta=").Append(c.LineCountDelta).AppendLine();
            s.Append("Position=").Append(c.OldPosition).Append('>').Append(c.NewPosition).AppendLine();
            s.Append("Length=").Append(c.OldLength).Append('>').Append(c.NewLength).AppendLine();
            s.Append("End=").Append(c.OldEnd).Append('>').Append(c.NewEnd).AppendLine();
            s.Append("Span=").Append(c.OldSpan).Append('>').Append(c.NewSpan).AppendLine();
            s.Append("Text=\"").Append(c.OldText).Append("\">\"").Append(c.NewText).AppendLine("\"");
            s.Append("<<<<<<<<<<");
            System.Console.WriteLine(s.ToString());
        }

        private void HandleChange(ITextChange c)
        {
            /*
            ITextEdit edit = this.view.TextBuffer.CreateEdit();
            edit.Insert(c.NewEnd-1, "!");
            edit.Apply();
             */
        }

        private void CompileIfWhitespace(ITextChange c)
        {
            if (c.Delta == 1)
            {
                // we do nothing if there's just one letter typed (no space or CR)
                byte[] bytes = System.Text.Encoding.ASCII.GetBytes(c.NewText);
                if (bytes != null && bytes[0] != 13 && bytes[0] != 32) return;
            }
            timer.stop(); // this will call Compile()
        }

        public void Compile()
        {
            if (!isDirty) return;
            System.Console.WriteLine("TODO: Compile!");
            isDirty = false;
        }
    }
}
