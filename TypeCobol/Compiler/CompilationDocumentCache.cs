using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
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
                   (scanState.InsideDataDivision ? "D3" : "__");
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

        /// <summary>
        /// Remove the given copies from the cache recursively: the dependent copies of the
        /// given copies are removed too.
        /// </summary>
        /// <param name="textNames">List of copies to remove, each identified by its textName.</param>
        /// <param name="evicted">Non-null set of evicted copies, to be populated by the method.</param>
        public void Evict([NotNull] List<string> textNames, [NotNull] HashSet<string> evicted)
        {
            /*
             * TODO This method does not support qualified names for copies !
             */

            // Create a map of dependent copies: each entry gives the list of copies that include it (directly and indirectly)
            var importedBy = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
            foreach (var document in _documents)
            {
                var copy = document.Value.TextSourceInfo.Name;
                var importedCopies = document.Value.CopyTextNamesVariations;
                foreach (var importedCopy in importedCopies)
                {
                    string fileName = importedCopy.GetFileName(document.Value.CompilerOptions);
                    if (!importedBy.TryGetValue(fileName, out var dependentCopies))
                    {
                        dependentCopies = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                        importedBy.Add(fileName, dependentCopies);
                    }

                    dependentCopies.Add(copy);
                }
            }

            // Evict from this cache
            foreach (var textName in textNames)
            {
                Evict(textName);

                // Evict dependent copies if any (no need to recurse as the list of dependent copies is already flattened)
                if (importedBy.TryGetValue(textName, out var dependentCopies))
                {
                    foreach (var dependentCopy in dependentCopies)
                    {
                        Evict(dependentCopy);
                    }
                }
            }

            void Evict(string textName)
            {
                // Remove all variants of this document
                string fullName = GetFullName(null, textName);
                var keysToRemove = _documents.Keys.Where(key => key.StartsWith(fullName, StringComparison.OrdinalIgnoreCase)).ToArray();
                if (keysToRemove.Length > 0)
                {
                    foreach (var keyToRemove in keysToRemove)
                    {
                        _documents.Remove(keyToRemove);
                    }
                }

                evicted.Add(textName);
            }
        }

        public void Clear() => _documents.Clear();
    }
}
