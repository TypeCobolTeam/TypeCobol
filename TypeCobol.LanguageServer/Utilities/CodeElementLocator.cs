using System.Diagnostics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Utilities
{
    // TODO Factorize with TypeCobolServer.CodeElementFinder / CodeElementMatcher / other classes ?
    internal static class CodeElementLocator
    {
        /// <summary>
        /// Search for CodeElement and corresponding Node at the given position.
        /// </summary>
        /// <param name="compilationUnit">Compilation unit to search into.</param>
        /// <param name="position">Targeted position in the document.</param>
        /// <returns>A non-null pair of CodeElement and Node, both can be null when the method fails to
        /// find anything. The method may also find a CodeElement without the corresponding Node.</returns>
        public static (CodeElement CodeElement, Node Node) FindCodeElementAt(CompilationUnit compilationUnit, Position position)
        {
            var programClassDocument = compilationUnit.ProgramClassDocumentSnapshot;

            var lines = programClassDocument.PreviousStepSnapshot.Lines;
            var lineWithCodeElements = FindLast(lines, position.line, HasCodeElements);
            if (lineWithCodeElements.Line == null)
            {
                // Nothing before given position
                return (null, null);
            }

            var previousLineWithCodeElements = FindLast(lines, lineWithCodeElements.Index - 1, HasCodeElements);
            var precedingCodeElement = previousLineWithCodeElements.Line?.CodeElements[^1]; // Maybe null if the position is on the first line with CEs

            // Check code elements on the line
            foreach (var codeElement in lineWithCodeElements.Line.CodeElements)
            {
                if (position.character < codeElement.StartIndex)
                {
                    // Cursor is located before this code element, return the preceding one if not null
                    return precedingCodeElement != null ? WithCorrespondingNode(precedingCodeElement) : (null, null);
                }

                if (position.character <= codeElement.StopIndex)
                {
                    // Cursor is within this code element, return it
                    return WithCorrespondingNode(codeElement);
                }

                precedingCodeElement = codeElement;
            }

            // Cursor is located within the last code element, but it ends on the next line, return the last code element
            Debug.Assert(precedingCodeElement != null);
            return WithCorrespondingNode(precedingCodeElement);

            static bool HasCodeElements(ICodeElementsLine line) => line.HasCodeElements;

            (CodeElement, Node) WithCorrespondingNode(CodeElement codeElement) =>
                programClassDocument.NodeCodeElementLinkers.TryGetValue(codeElement, out var node)
                    ? (codeElement, node)
                    : (codeElement, null); // The dictionary may be incomplete (for example when CUP fails to parse whole document)
        }

        private static (ICodeElementsLine Line, int Index) FindLast(IReadOnlyList<ICodeElementsLine> list, int start, Predicate<ICodeElementsLine> predicate)
        {
            int index = Math.Min(start, list.Count - 1);
            while (index >= 0)
            {
                var line = list[index];
                if (predicate(line))
                {
                    return (line, index);
                }

                index--;
            }

            return (null, -1);
        }
    }
}