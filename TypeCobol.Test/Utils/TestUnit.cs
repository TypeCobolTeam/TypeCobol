using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Test.UtilsNew
{
    /// <summary>
    /// Test one source file against multiple comparators. Supports incremental testing, each incremental
    /// change can itself be tested against multiple comparators.
    /// </summary>
    internal class TestUnit
    {
        private static readonly string[] _DefaultCopyExtensions = { ".cpy" };

        private readonly FileCompiler _fileCompiler;
        private readonly List<InputChange> _inputChanges;
        private readonly List<Comparison> _initialResultComparisons;
        private readonly Dictionary<string, List<Comparison>> _intermediateResultsComparisons;

        public TestUnit(string sourceFilePath, DocumentFormat documentFormat = null, TypeCobolOptions compilationOptions = null, bool antlrProfiling = false)
            : this(sourceFilePath, _DefaultCopyExtensions, documentFormat ?? DocumentFormat.RDZReferenceFormat, compilationOptions ?? new TypeCobolOptions(), Array.Empty<string>(), antlrProfiling)
        {

        }

        public TestUnit(string sourceFilePath, string[] copyExtensions, DocumentFormat documentFormat, TypeCobolOptions compilationOptions, string[] copyDirectories, bool antlrProfiling)
        {
            string fileNameWithoutExtension = Path.GetFileNameWithoutExtension(sourceFilePath);
            string extension = Path.GetExtension(sourceFilePath);
            if (string.IsNullOrEmpty(fileNameWithoutExtension) || string.IsNullOrEmpty(extension))
            {
                throw new ArgumentException($"Invalid test: source file must have a name and an extension, source file path '{sourceFilePath}' is not supported !");
            }

            string rootDirectory = Path.GetDirectoryName(sourceFilePath);
            var fileExtensions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
                                 {
                                     extension
                                 };
            foreach (var copyExtension in copyExtensions)
            {
                fileExtensions.Add(copyExtension);
            }

            var compilationProject = new CompilationProject(fileNameWithoutExtension, rootDirectory, fileExtensions.ToArray(), documentFormat, compilationOptions, null);
            foreach (var copyDirectory in copyDirectories)
            {
                compilationProject.SourceFileProvider.AddLocalDirectoryLibrary(copyDirectory, false, copyExtensions, documentFormat);
            }

            bool isCopyFile = copyExtensions.Contains(extension, StringComparer.OrdinalIgnoreCase);
            _fileCompiler = new FileCompiler(compilationProject, fileNameWithoutExtension, isCopyFile);

            if (antlrProfiling)
            {
                _fileCompiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.ActivateDetailedAntlrPofiling = true;
                _fileCompiler.CompilationResultsForProgram.PerfStatsForTemporarySemantic.ActivateDetailedAntlrPofiling = true;
            }

            _inputChanges = new List<InputChange>();
            _initialResultComparisons = new List<Comparison>();
            _intermediateResultsComparisons = new Dictionary<string, List<Comparison>>(StringComparer.OrdinalIgnoreCase);
        }

        public void AddInputChange(InputChange inputChange)
        {
            _inputChanges.Add(inputChange);
        }

        public void AddComparison(Comparison comparison)
        {
            if (comparison.ChangeId == null)
            {
                // Initial comparison
                _initialResultComparisons.Add(comparison);
            }
            else
            {
                // Intermediate comparison
                if (!_intermediateResultsComparisons.TryGetValue(comparison.ChangeId, out var comparisons))
                {
                    comparisons = new List<Comparison>();
                    _intermediateResultsComparisons.Add(comparison.ChangeId, comparisons);
                }

                comparisons.Add(comparison);
            }
        }

        public void Run()
        {
            // Pre-conditions
#if EUROINFO_RULES
            RemoveNonEIComparisons(_initialResultComparisons);
            foreach (var intermediateComparisons in _intermediateResultsComparisons.Values)
            {
                RemoveNonEIComparisons(intermediateComparisons);
            }

            void RemoveNonEIComparisons(List<Comparison> comparisons)
            {
                if (comparisons.Any(c => c.IsEI))
                {
                    // If any -EI result file exists => Remove all comparators without isEI flag to true. 
                    // We only want to check EI results files. 
                    comparisons.RemoveAll(c => !c.IsEI);
                }
            }
#endif
#if DEBUG
            // Incremental changes must be ordered in .inc file
            var changes = _inputChanges.Select(change => change.Id).ToList();
            var sortedChanges = _inputChanges.Select(change => change.Id).OrderBy(id => id).ToList();
            if (!changes.SequenceEqual(sortedChanges))
            {
                throw new Exception("Incremental changes must be sorted.");
            }

            // Each change must have at least one corresponding result file
            var results = _intermediateResultsComparisons.Keys.ToHashSet(StringComparer.OrdinalIgnoreCase);
            results.SymmetricExceptWith(changes);
            if (results.Count > 0)
            {
                throw new Exception("Each incremental change must have at least one comparison.");
            }
#endif

            // Activate incremental changes tracking
            var history = _fileCompiler.CompilationResultsForProgram.TrackChanges();

            // Full parsing
            _fileCompiler.CompileOnce();

            // Compare initial result
            foreach (var comparison in _initialResultComparisons)
            {
                comparison.Compare(_fileCompiler.CompilationResultsForProgram, history);
            }

            // Apply change, perform incremental parsing and compare results
            foreach (var inputChange in _inputChanges)
            {
                // Apply change
                _fileCompiler.CompilationResultsForProgram.UpdateTextLines(inputChange.TextChangedEvent);

                // Incremental parsing
                _fileCompiler.CompileOnce();

                // Compare
                foreach (var comparison in _intermediateResultsComparisons[inputChange.Id])
                {
                    comparison.Compare(_fileCompiler.CompilationResultsForProgram, history);
                }
            }
        }
    }
}
