using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor;

namespace TypeCobol.Test.UtilsNew
{
    /// <summary>
    /// Allow to test an entire folder automatically. Source files and result files are discovered
    /// using their file extension and name patterns.
    /// </summary>
    internal class FolderTester
    {
        private class TestUnitData
        {
            private static DocumentFormat AutoDetectDocumentFormat(string sourceFilePath)
            {
                return sourceFilePath.Contains(".rdz") ? DocumentFormat.RDZReferenceFormat : DocumentFormat.FreeUTF8Format;
            }

            private readonly string _testName;
            public List<string> SourceFilePaths { get; }
            public List<string> ChangesFilePaths { get; }
            public List<string> ResultFilePaths { get; }

            public TestUnitData(string testName)
            {
                _testName = testName;
                SourceFilePaths = new List<string>();
                ChangesFilePaths = new List<string>();
                ResultFilePaths = new List<string>();
            }

            public TestUnit Create(TypeCobolOptions options)
            {
                if (SourceFilePaths.Count == 0)
                {
                    throw new Exception($"Invalid test '{_testName}': found result file(s) but no source.");
                }
                
                if (SourceFilePaths.Count > 1)
                {
                    string message = $"Invalid test '{_testName}': found conflicting source files:" + Environment.NewLine;
                    message += string.Join(Environment.NewLine, SourceFilePaths);
                    throw new Exception(message);
                }

                string sourceFilePath = SourceFilePaths[0];
                var documentFormat = AutoDetectDocumentFormat(sourceFilePath);
                var testUnit = new TestUnit(sourceFilePath, documentFormat, options);

                if (ChangesFilePaths.Count > 1)
                {
                    string message = $"Invalid test '{_testName}': found multiple input changes files:" + Environment.NewLine;
                    message += string.Join(Environment.NewLine, ChangesFilePaths);
                    throw new Exception(message);
                }
                
                if (ChangesFilePaths.Count == 1)
                {
                    var inputChanges = InputChange.Load(ChangesFilePaths[0]);
                    foreach (var inputChange in inputChanges)
                    {
                        testUnit.AddInputChange(inputChange);
                    }
                }

                foreach (var resultFilePath in ResultFilePaths)
                {
                    var comparison = Comparisons.GetComparison(resultFilePath, out var changeId);
                    if (changeId != null)
                    {
                        // This is an intermediate comparison
                        testUnit.AddIntermediateComparison(changeId, comparison);
                    }
                    else
                    {
                        // This is a final comparison
                        testUnit.AddFinalComparison(comparison);
                    }
                }

                return testUnit;
            }
        }

        private static readonly string[] _DefaultChangeExtensions = { ".inc" };
        private static readonly string[] _DefaultResultExtensions = { ".txt" };

        private static TypeCobolOptions AutoConfigure(string folder, bool isCobolLanguage)
        {
            var options = new TypeCobolOptions();

#if EUROINFO_RULES
            // Auto-detect CPY list in folder
            string copyNameMapFilePath = Path.Combine(folder, "CpyCopies.lst");
            if (File.Exists(copyNameMapFilePath))
            {
                options.CpyCopyNameMap = new CopyNameMapFile(copyNameMapFilePath);
            }
#endif

            // 'Dynamic' option, specifically set for the current test by caller
            options.IsCobolLanguage = isCobolLanguage;

            // Auto-enable SQL parsing based on folder path
            bool enableSqlParsing = folder.IndexOf("SQL", StringComparison.InvariantCultureIgnoreCase) >= 0;
            options.EnableSqlParsing = enableSqlParsing;

            return options;
        }

        private readonly string _rootFolder;
        private readonly string[] _sourceExtensions;
        private readonly string[] _changeExtensions;
        private readonly string[] _resultExtensions;
        private readonly bool _recursive;

        public FolderTester(string rootFolder, string[] sourceExtensions, string[] changeExtensions = null, string[] resultExtensions = null, bool recursive = true)
        {
            if (string.IsNullOrEmpty(rootFolder))
            {
                throw new ArgumentException("Folder to test is required !", nameof(rootFolder));
            }

            if (IsNullOrEmpty(sourceExtensions))
            {
                throw new ArgumentException("Source files extensions are required !", nameof(sourceExtensions));
            }

            _rootFolder = rootFolder;
            _sourceExtensions = sourceExtensions;
            _changeExtensions = IsNullOrEmpty(changeExtensions) ? _DefaultChangeExtensions : changeExtensions;
            _resultExtensions = IsNullOrEmpty(resultExtensions) ? _DefaultResultExtensions : resultExtensions;
            _recursive = recursive;

            bool IsNullOrEmpty(string[] array) => array == null || array.Length == 0;
        }

        public int Test(bool isCobolLanguage = false)
        {
            var exceptions = new List<Exception>();
            int count = Test(_rootFolder, exceptions, isCobolLanguage);

            if (exceptions.Count > 0)
            {
                throw new AggregateException("Folder test failed !", exceptions);
            }

            return count;
        }

        private int Test(string folder, List<Exception> exceptions, bool isCobolLanguage)
        {
            // First pass: analyze folder and gather data to create test units
            var testUnits = new Dictionary<string, TestUnitData>(StringComparer.OrdinalIgnoreCase);
            foreach (var file in Directory.EnumerateFiles(folder, "*.*", SearchOption.TopDirectoryOnly)) //Ignore files without extension, consider only current folder
            {
                string fileName = Path.GetFileName(file);
                int cut = fileName.IndexOf('.');

                string testName = fileName.Substring(0, cut);
                if (!testUnits.TryGetValue(testName, out var testUnitData))
                {
                    testUnitData = new TestUnitData(testName);
                    testUnits.Add(testName, testUnitData);
                }

                cut = fileName.LastIndexOf('.');
                string extension = fileName.Substring(cut, fileName.Length - cut); //Includes dot
                if (_sourceExtensions.Contains(extension))
                {
                    testUnitData.SourceFilePaths.Add(file);
                }
                else if (_changeExtensions.Contains(extension))
                {
                    testUnitData.ChangesFilePaths.Add(file);
                }
                else if (_resultExtensions.Contains(extension))
                {
                    testUnitData.ResultFilePaths.Add(file);
                }
            }

            // Auto configure compilation options. These options are valid for current folder only !
            var options = AutoConfigure(folder, isCobolLanguage);

            // Second pass: create and run each test unit
            int count = 0;
            foreach (var testUnit in testUnits.Values)
            {
                try
                {
                    testUnit.Create(options).Run();
                }
                catch (Exception exception)
                {
                    exceptions.Add(exception);
                }

                count++;
            }

            if (_recursive)
            {
                foreach (var childFolder in Directory.GetDirectories(folder, "*", SearchOption.TopDirectoryOnly))
                {
                    count += Test(childFolder, exceptions, isCobolLanguage);
                }
            }

            return count;
        }
    }
}
