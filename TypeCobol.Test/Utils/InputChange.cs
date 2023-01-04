using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
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
            var rangeUpdates = new List<RangeUpdate>();

            string[] lines = File.ReadAllLines(inputChangesFilePath);
            foreach (var line in lines)
            {
                string[] parts = line.Split('\t');
                switch (parts.Length)
                {
                    case 1:
                        // This is a change id, flush current change and initialize a new id
                        FlushCurrentInputChange();
                        Debug.Assert(parts[0].Last() == ':', $"Invalid change id description in '{inputChangesFilePath}'.");
                        currentChangeId = parts[0].Substring(0, parts[0].Length - 1);
                        break;
                    case 2:
                        // This is a range update, part of the current InputChange
                        Parse(parts[0], out int lineStart, out int columnStart, out int lineEnd, out int columnEnd);
                        string text = parts[1].Replace("\\n", Environment.NewLine);
                        var rangeUpdate = new RangeUpdate(lineStart, columnStart, lineEnd, columnEnd, text);
                        rangeUpdates.Add(rangeUpdate);
                        break;
                    default:
                        // This is not a valid line
                        Debug.Fail($"Invalid format for input change file '{inputChangesFilePath}' !");
                        break;
                }
            }

            // Flush remaining input change, if any
            FlushCurrentInputChange();

            return result;

            void FlushCurrentInputChange()
            {
                if (currentChangeId != null)
                {
                    var updates = rangeUpdates.ToArray();
                    result.Add(new InputChange(currentChangeId, updates));
                    rangeUpdates.Clear();
                }
            }

            void Parse(string rangeDescription, out int lineStart, out int columnStart, out int lineEnd, out int columnEnd)
            {
                var match = Regex.Match(rangeDescription, @"\(([0-9]+),\s*([0-9]+)\)\s*->\s*\(([0-9]+),\s*([0-9]+)\)");
                Debug.Assert(match.Success, $"Invalid range description in '{inputChangesFilePath}'.");
                lineStart = int.Parse(match.Groups[1].Value);
                columnStart = int.Parse(match.Groups[2].Value);
                lineEnd = int.Parse(match.Groups[3].Value);
                columnEnd = int.Parse(match.Groups[4].Value);
            }
        }

        public string Id { get; }

        public RangeUpdate[] Updates { get; }

        public InputChange(string id, RangeUpdate[] updates)
        {
            Id = id;
            Updates = updates;
        }
    }
}
