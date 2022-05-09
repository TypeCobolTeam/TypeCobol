using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler
{
    public class CompilationDocumentCache
    {
        private static string GetFullName(string libraryName, string textName)
        {
            return (libraryName ?? SourceFileProvider.DefaultLibraryName) + "." + textName;
        }

        private static string GetKey(string fullName, MultilineScanState scanState)
        {
            string key = fullName;

            // Add string flags based on scan state
            key += (scanState.SpecialNames.DecimalPointIsComma ? "D1" : "__") +
                   (scanState.WithDebuggingMode ? "D2" : "__") +
                   (scanState.InsideDataDivision ? "D3" : "__") +
                   (scanState.InsideProcedureDivision ? "D4" : "__");
            // NB : the hypothesis here is that we don't need to include more properties of scanState in the cache key, 
            // because a COPY is always cleanly delimited at CodeElement boundaries.

            return key;
        }

        private readonly Dictionary<string, CompilationDocument> _documents;

        public CompilationDocumentCache()
        {
            _documents = new Dictionary<string, CompilationDocument>(StringComparer.OrdinalIgnoreCase);
        }

        public CompilationDocument GetOrAddDocument([CanBeNull] string libraryName, [NotNull] string textName, [NotNull] MultilineScanState scanState, [NotNull] Func<CompilationDocument> compileNewDocument)
        {
            string key = GetKey(GetFullName(libraryName, textName), scanState);
            if (!_documents.TryGetValue(key, out var document))
            {
                document = compileNewDocument();
                System.Diagnostics.Debug.Assert(document != null);
                _documents.Add(key, document);
            }

            return document;
        }

        public void Evict([CanBeNull] string libraryName, [NotNull] string textName)
        {
            string fullName = GetFullName(libraryName, textName);
            var keysToRemove = _documents.Keys.Where(key => key.StartsWith(fullName, StringComparison.OrdinalIgnoreCase)).ToArray();
            foreach (var keyToRemove in keysToRemove)
            {
                _documents.Remove(keyToRemove);
            }
        }

        public void Clear() => _documents.Clear();
    }
}
