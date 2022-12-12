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
        private readonly List<Comparison> _finalResultComparisons;
        private readonly Dictionary<string, List<Comparison>> _intermediateResultsComparisons;

        public TestUnit(string sourceFilePath, DocumentFormat documentFormat, TypeCobolOptions compilationOptions, bool antlrProfiling = false)
            : this(sourceFilePath, _DefaultCopyExtensions, documentFormat, compilationOptions, Array.Empty<string>(), antlrProfiling)
        {

        }

        public TestUnit(string sourceFilePath, string[] copyExtensions, DocumentFormat documentFormat, TypeCobolOptions compilationOptions, string[] copyDirectories, bool antlrProfiling)
        {
            string fileNameWithExtension = Path.GetFileName(sourceFilePath);
            string[] parts = fileNameWithExtension.Split('.');
            if (parts.Length < 2)
            {
                throw new ArgumentException($"Invalid test: source file must have an extension, source file path '{sourceFilePath}' is not supported !");
            }

            string testName = parts[0];
            string sampleExtension = $".{parts[1]}";
            string rootDirectory = Path.GetDirectoryName(sourceFilePath);
            var fileExtensions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
                                 {
                                     sampleExtension
                                 };
            foreach (var copyExtension in copyExtensions)
            {
                fileExtensions.Add(copyExtension);
            }

            var compilationProject = new CompilationProject(testName, rootDirectory, fileExtensions.ToArray(), documentFormat, compilationOptions, null);
            foreach (var copyDirectory in copyDirectories)
            {
                compilationProject.SourceFileProvider.AddLocalDirectoryLibrary(copyDirectory, false, copyExtensions, documentFormat);
            }

            bool isCopyFile = copyExtensions.Contains(sampleExtension, StringComparer.OrdinalIgnoreCase);
            _fileCompiler = new FileCompiler(compilationProject, testName, isCopyFile);

            if (antlrProfiling)
            {
                _fileCompiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.ActivateDetailedAntlrPofiling = true;
                _fileCompiler.CompilationResultsForProgram.PerfStatsForTemporarySemantic.ActivateDetailedAntlrPofiling = true;
            }

            _inputChanges = new List<InputChange>();
            _finalResultComparisons = new List<Comparison>();
            _intermediateResultsComparisons = new Dictionary<string, List<Comparison>>();
        }

        public void AddInputChange(InputChange inputChange)
        {
            _inputChanges.Add(inputChange);
        }

        public void AddFinalComparison(Comparison comparison)
        {
            _finalResultComparisons.Add(comparison);
        }

        public void AddIntermediateComparison(string changeId, Comparison comparison)
        {
            if (!_intermediateResultsComparisons.TryGetValue(changeId, out var comparisons))
            {
                comparisons = new List<Comparison>();
                _intermediateResultsComparisons.Add(changeId, comparisons);
            }

            comparisons.Add(comparison);
        }

        public void Run()
        {
#if EUROINFO_RULES
            RemoveNonEIComparisonsWhenNeeded(_finalResultComparisons);
            foreach (var intermediateComparisons in _intermediateResultsComparisons.Values)
            {
                RemoveNonEIComparisonsWhenNeeded(intermediateComparisons);
            }

            void RemoveNonEIComparisonsWhenNeeded(List<Comparison> comparisons)
            {
                if (comparisons.Any(c => c.IsEI))
                {
                    // If any -EI result file exists => Remove all comparators without isEI flag to true. 
                    // We only want to check EI results files. 
                    comparisons.RemoveAll(c => !c.IsEI);
                }
            }
#endif

            // Activate incremental changes tracking
            var history = _fileCompiler.CompilationResultsForProgram.TrackChanges();

            // Full parsing
            _fileCompiler.CompileOnce();

            // Apply change, perform incremental parsing and compare results
            foreach (var inputChange in _inputChanges)
            {
                _fileCompiler.CompilationResultsForProgram.UpdateTextLines(inputChange.TextChangedEvent);
                if (_intermediateResultsComparisons.TryGetValue(inputChange.Id, out var comparisons))
                {
                    foreach (var comparison in comparisons)
                    {
                        comparison.Compare(_fileCompiler.CompilationResultsForProgram, history);
                    }
                }
                else
                {
                    Console.WriteLine($"/!\\ No result file for incremental change '{inputChange.Id}' /!\\");
                }
            }

            // Compare final result
            foreach (var comparison in _finalResultComparisons)
            {
                comparison.Compare(_fileCompiler.CompilationResultsForProgram, history);
            }
        }
    }
}
