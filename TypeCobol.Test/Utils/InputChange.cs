using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.UtilsNew
{
    /// <summary>
    /// Describe one incremental change to be applied to a document during a test.
    /// </summary>
    internal class InputChange
    {
        public static List<InputChange> Load(string inputChangesFilePath)
        {
            var result = new List<InputChange>();

            string currentChangeId = null;
            TextChangedEvent currentTextChangeEvent = null;

            string[] lines = File.ReadAllLines(inputChangesFilePath);
            foreach (var line in lines)
            {
                string[] parts = line.Split('\t');
                switch (parts.Length)
                {
                    case 1:
                        // This is a change id, flush current and initialize a new one
                        FlushCurrentTextChange();
                        Debug.Assert(parts[0].Last() == ':');
                        currentChangeId = parts[0].Substring(0, parts[0].Length - 1);
                        currentTextChangeEvent = new TextChangedEvent();
                        break;
                    case 2:
                    case 3:
                        // This is a text change, part of a TextChangedEvent
                        var type = (TextChangeType)Enum.Parse(typeof(TextChangeType), parts[0]);
                        int lineIndex = int.Parse(parts[1]);
                        string text = parts.Length == 3 ? parts[2].Replace("\\n", Environment.NewLine) : null;
                        Debug.Assert(currentTextChangeEvent != null);
                        var newLine = text != null ? new TextLineSnapshot(lineIndex, text, null) : null;
                        currentTextChangeEvent.TextChanges.Add(new TextChange(type, lineIndex, newLine));
                        break;
                }
            }

            // Flush remaining text change, if any
            FlushCurrentTextChange();

            return result;

            void FlushCurrentTextChange()
            {
                if (currentChangeId != null)
                {
                    Debug.Assert(currentTextChangeEvent != null);
                    result.Add(new InputChange(currentChangeId, currentTextChangeEvent));
                }
            }
        }

        public string Id { get; }

        public TextChangedEvent TextChangedEvent { get; }

        public InputChange(string id, TextChangedEvent textChangedEvent)
        {
            Id = id;
            TextChangedEvent = textChangedEvent;
        }
    }
}
