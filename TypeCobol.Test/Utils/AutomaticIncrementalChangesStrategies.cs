using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Utils
{
    /// <summary>
    /// Interface for any incremental change generator. The generated change sequence must be neutral for source code
    /// meaning after the whole sequence has been applied, the source is identical to what it was before applying the
    /// sequence.
    /// </summary>
    internal interface IIncrementalChangesGenerator
    {
        IEnumerable<RangeUpdate[]> GetUpdatesSequence(CompilationUnit afterInitialParsing);
    }

    internal class AddEmptyLineAtBeginningThenRemove : IIncrementalChangesGenerator
    {
        public IEnumerable<RangeUpdate[]> GetUpdatesSequence(CompilationUnit afterInitialParsing)
        {
            yield return new[] { new RangeUpdate(0, 0, 0, 0, Environment.NewLine) };
            yield return new[] { new RangeUpdate(0, 0, 1, 0, string.Empty) };
        }
    }

    internal class AddEmptyLineInTheMiddleThenRemove : IIncrementalChangesGenerator
    {
        public IEnumerable<RangeUpdate[]> GetUpdatesSequence(CompilationUnit afterInitialParsing)
        {
            int lineIndex = afterInitialParsing.CobolTextLines.Count / 2;
            yield return new[] { new RangeUpdate(lineIndex, 0, lineIndex, 0, Environment.NewLine) };
            yield return new[] { new RangeUpdate(lineIndex, 0, lineIndex + 1, 0, string.Empty) };
        }
    }

    internal class AddEmptyLineAtEndThenRemove : IIncrementalChangesGenerator
    {
        public IEnumerable<RangeUpdate[]> GetUpdatesSequence(CompilationUnit afterInitialParsing)
        {
            if (afterInitialParsing.CobolTextLines.Count < 1) yield break;

            int lineIndex = afterInitialParsing.CobolTextLines.Count - 1;
            int column = afterInitialParsing.CobolTextLines[afterInitialParsing.CobolTextLines.Count - 1].Length;
            yield return new[] { new RangeUpdate(lineIndex, column, lineIndex, column, Environment.NewLine) };
            yield return new[] { new RangeUpdate(lineIndex, column, lineIndex + 1, 0, string.Empty) };
        }
    }

    internal class ClearDocumentThenRewriteLineByLine : IIncrementalChangesGenerator
    {
        public IEnumerable<RangeUpdate[]> GetUpdatesSequence(CompilationUnit afterInitialParsing)
        {
            // We are in a deferred enumerator, so copy lines before they change !
            var initialCobolTextLines = afterInitialParsing.CobolTextLines.ToArray();

            // Clear document. NOTE: with real client, the document is closed then reopened
            var lastLine = initialCobolTextLines.LastOrDefault();
            if (lastLine != null)
            {
                yield return new[] { new RangeUpdate(0, 0, lastLine.LineIndex, lastLine.Length, string.Empty) };
            }

            // Rewrite line by line
            ICobolTextLine previousLine = null;
            for (int i = 0; i < initialCobolTextLines.Length; i++)
            {
                var currentLine = initialCobolTextLines[i];
                if (previousLine == null)
                {
                    // Write first line
                    yield return new[] { new RangeUpdate(0, 0, 0, 0, currentLine.Text) };
                }
                else
                {
                    // Insert lne break at end of previous line followed by new line text
                    yield return new[] { new RangeUpdate(i - 1, previousLine.Length, i - 1, previousLine.Length, Environment.NewLine + currentLine.Text) };
                }

                previousLine = currentLine;
            }
        }
    }

    internal class ClearDocumentThenRewriteLineByLineInReverseOrder : IIncrementalChangesGenerator
    {
        public IEnumerable<RangeUpdate[]> GetUpdatesSequence(CompilationUnit afterInitialParsing)
        {
            // We are in a deferred enumerator, so copy lines before they change !
            var initialCobolTextLines = afterInitialParsing.CobolTextLines.ToArray();

            // Clear document. NOTE: with real client, the document is closed then reopened
            var lastLine = initialCobolTextLines.LastOrDefault();
            if (lastLine != null)
            {
                yield return new[] { new RangeUpdate(0, 0, lastLine.LineIndex, lastLine.Length, string.Empty) };
            }

            // Rewrite line by line in reverse order
            for (int i = initialCobolTextLines.Length - 1; i >= 0; i--)
            {
                var currentLine = initialCobolTextLines[i];
                if (i == initialCobolTextLines.Length - 1)
                {
                    // Write last line at the bottom of the document
                    yield return new[] { new RangeUpdate(0, 0, 0, 0, currentLine.Text) };
                }
                else
                {
                    // Write following line above previous, so just end it with a line break
                    yield return new[] { new RangeUpdate(0, 0, 0, 0, currentLine.Text + Environment.NewLine) };
                }
            }
        }
    }
}
