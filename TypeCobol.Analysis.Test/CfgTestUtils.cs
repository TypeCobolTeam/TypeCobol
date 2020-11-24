using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Text;
using TypeCobol.Test;
using TypeCobol.Test.Utils;

namespace TypeCobol.Analysis.Test
{
    internal static class CfgTestUtils
    {
        private const string CFG_ANALYZER_IDENTIFIER = "cfg-test-utils";

        private static readonly Dictionary<CfgBuildingMode, AnalyzerProvider> _AnalyzerProviders;

        //From TypeCobol.Test
        public static readonly string ThirdPartyDir;
        //Project dirs
        public static readonly string BasicCfgInstrs;
        public static readonly string BasicCfgPrograms;
        public static readonly string BasicDfaSamples;
        public static readonly string CfgDfaBuildTests;
        public static readonly string Report;

        static CfgTestUtils()
        {
            _AnalyzerProviders = new Dictionary<CfgBuildingMode, AnalyzerProvider>();
            AddAnalyzerProvider(CfgBuildingMode.Standard);
            AddAnalyzerProvider(CfgBuildingMode.Extended);
            AddAnalyzerProvider(CfgBuildingMode.WithDfa);

            string currentDir = Directory.GetCurrentDirectory();
            string solutionDir = Path.GetDirectoryName(Path.GetDirectoryName(currentDir));
            Assert.IsNotNull(solutionDir);
            ThirdPartyDir = Path.Combine(solutionDir, "TypeCobol.Test", "ThirdParty");
            BasicCfgInstrs = Path.Combine(solutionDir, "TypeCobol.Analysis.Test", "BasicCfgInstrs");
            BasicCfgPrograms = Path.Combine(solutionDir, "TypeCobol.Analysis.Test", "BasicCfgPrograms");
            BasicDfaSamples = Path.Combine(solutionDir, "TypeCobol.Analysis.Test", "BasicDfaSamples");
            CfgDfaBuildTests = Path.Combine(solutionDir, "TypeCobol.Analysis.Test", "CfgDfaBuildTests");
            Report = Path.Combine(solutionDir, "TypeCobol.Analysis.Test", "Report");

            void AddAnalyzerProvider(CfgBuildingMode mode)
            {
                var analyzerProvider = new AnalyzerProvider();
                analyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(CFG_ANALYZER_IDENTIFIER, mode));
                _AnalyzerProviders.Add(mode, analyzerProvider);
            }
        }

        /// <summary>
        /// Perform parsing of the supplied Cobol or TypeCobol source file and build CFGs and DFA results.
        /// </summary>
        /// <param name="sourceFilePath">Full path to the source file.</param>
        /// <param name="expectedDiagnosticsFilePath">Full path to the diagnostic file, pass null to skip
        /// diagnostic comparison</param>
        /// <param name="expectedLivenessFilePath">Full path to a variable liveness CSV result file, pass null to skip comparison</param>
        /// <param name="expectedUseDefsFilePath">Full path to a use-def CSV result file, pass null to skip comparison</param>
        /// <returns>An instance of <see cref="DfaTestResults" /> containing all graphs and DFA results.</returns>
        public static DfaTestResults ParseCompareDiagnosticsWithDfa(string sourceFilePath,
            string expectedDiagnosticsFilePath = null,
            string expectedLivenessFilePath = null,
            string expectedUseDefsFilePath = null)
        {
            var graphs = ParseCompareDiagnostics<DfaBasicBlockInfo<VariableSymbol>>(sourceFilePath, CfgBuildingMode.WithDfa, expectedDiagnosticsFilePath);
            var results = new DfaTestResults(graphs);

            if (expectedLivenessFilePath !=  null)
            {
                //TODO Perform liveness sets comparison
            }

            if (expectedUseDefsFilePath != null)
            {
                //TODO Perform use-def sets comparison
            }

            return results;
        }

        /// <summary>
        /// Perform parsing of the supplied Cobol or TypeCobol source file
        /// and compare actual diagnostics to expected diagnostics.
        /// </summary>
        /// <param name="sourceFilePath">Full path to the source file.</param>
        /// <param name="expectedDiagnosticsFilePath">Full path to the diagnostic file, pass null to skip
        /// diagnostic comparison</param>
        /// <returns>List of CFG built for the source file.</returns>
        public static CfgTestResults<object> ParseCompareDiagnosticsCfgOnly(string sourceFilePath, string expectedDiagnosticsFilePath = null)
        {
            var graphs = ParseCompareDiagnostics<object>(sourceFilePath, CfgBuildingMode.Standard, expectedDiagnosticsFilePath);
            return new CfgTestResults<object>(graphs);
        }

        /// <summary>
        /// Perform parsing of the supplied Cobol or TypeCobol source file
        /// and compare actual diagnostics to expected diagnostics.
        /// </summary>
        /// <typeparam name="D">Type of basic block data used.</typeparam>
        /// <param name="sourceFilePath">Full path to the source file.</param>
        /// <param name="mode">CFG building mode, default is Standard mode.</param>
        /// <param name="expectedDiagnosticsFilePath">Full path to the diagnostic file, pass null to skip
        /// diagnostic comparison.</param>
        /// <param name="customActivators">Additional activators to add custom analyzers to be used while parsing.</param>
        /// <returns>List of CFG built for the source file.</returns>
        public static IList<ControlFlowGraph<Node, D>> ParseCompareDiagnostics<D>(string sourceFilePath,
            CfgBuildingMode mode,
            string expectedDiagnosticsFilePath = null,
            params Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer>[] customActivators)
        {
            if (!_AnalyzerProviders.TryGetValue(mode, out var analyzerProvider))
            {
                throw new NotSupportedException($"Unsupported CFG building mode: '{mode}' !");
            }

            if (customActivators != null)
            {
                foreach (var activator in customActivators)
                {
                    analyzerProvider.AddActivator(activator);
                }
            }

            var parser = Parser.Parse(sourceFilePath, DocumentFormat.RDZReferenceFormat, analyzerProvider: analyzerProvider);
            var results = parser.Results;

            if (expectedDiagnosticsFilePath != null)
            {
                var diagnostics = results.AllDiagnostics();
                if (diagnostics.Count > 0)
                {
                    string diagnosticsText = ParserUtils.DiagnosticsToString(diagnostics);
                    if (File.Exists(expectedDiagnosticsFilePath))
                    {
                        string expectedResult = File.ReadAllText(expectedDiagnosticsFilePath);
                        TestUtils.compareLines(sourceFilePath, diagnosticsText, expectedResult, expectedDiagnosticsFilePath);
                    }
                    else
                    {
                        //Found diagnostics while parsing but no diagnostic file.
                        StringBuilder errorMessage = new StringBuilder();
                        errorMessage.AppendLine($"Found diagnostics while parsing '{sourceFilePath}' but diagnostics file is missing.");
                        errorMessage.AppendLine($"Expected diagnostic file path was '{expectedDiagnosticsFilePath}'.");
                        errorMessage.Append(diagnosticsText);
                        throw new Exception(errorMessage.ToString());
                    }
                }
                else
                {
                    if (File.Exists(expectedDiagnosticsFilePath))
                    {
                        //Expected diagnostics but none found during parsing.
                        StringBuilder errorMessage = new StringBuilder();
                        errorMessage.AppendLine($"Expected diagnostics but none were found during parsing of '{sourceFilePath}'.");
                        errorMessage.AppendLine($"Expected diagnostic file path was '{expectedDiagnosticsFilePath}'.");
                        throw new Exception(errorMessage.ToString());
                    }

                    //file doesn't exist and no diagnostics : OK
                }
            }

            if (results.TryGetAnalyzerResult(CFG_ANALYZER_IDENTIFIER, out IList<ControlFlowGraph<Node, D>> graphs))
            {
                return graphs;
            }

            throw new Exception($"No Control Flow Graph generated for file '{sourceFilePath}'.");
        }

        private static void GenDotCfg<D>(ControlFlowGraph<Node, D> cfg, TextWriter writer, bool fullInstruction)
        {
            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<D> dotGen = new CfgDotFileForNodeGenerator<D>(cfg)
                                                   {
                                                       FullInstruction = fullInstruction,
                                                   };
            dotGen.Report(writer);
        }

        /// <summary>
        /// Generates a Dot CFG file
        /// </summary>
        /// <param name="cfg">The CFG to generate the dot file</param>
        /// <param name="dotFilePath">The Dot File to be generated.</param>
        /// <param name="bFullInstructions">true if full instruction source code must be generated, false if only instruction names.</param>
        public static void GenDotCfgFile<D>(ControlFlowGraph<Node, D> cfg, string dotFilePath, bool bFullInstructions)
        {
            using (var writer = File.CreateText(dotFilePath))
            {
                GenDotCfg(cfg, writer, bFullInstructions);
            }
        }

        /// <summary>
        /// Generate the dot corresponding to Cfg and compare it with the expected file.
        /// </summary>
        /// <param name="cfg">The actual Control Flow Graph instance.</param>
        /// <param name="testPath">Path of the original Cobol/TypeCobol source file.</param>
        /// <param name="expectedDotFile">The expected dot file.</param>
        /// <param name="bFullInstruction">true if full instruction must be displayed, false otherwise</param>
        public static void GenDotCfgAndCompare<D>(ControlFlowGraph<Node, D> cfg, string testPath, string expectedDotFile, bool bFullInstruction)
        {
            StringWriter writer = new StringWriter();
            GenDotCfg(cfg, writer, bFullInstruction);

            // compare with expected result
            string result = writer.ToString();
            string expected = File.ReadAllText(expectedDotFile);
            TestUtils.compareLines(testPath, result, expected, expectedDotFile);
        }
    }
}
