using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Test;
using TypeCobol.Test.Utils;

namespace TypeCobol.Analysis.Test
{
    internal static class CfgTestUtils
    {
        private const string CFG_ANALYZER_IDENTIFIER = "cfg-basic-tests";

        private static readonly AnalyzerProvider _AnalyzerProvider;

        static CfgTestUtils()
        {
            _AnalyzerProvider = new AnalyzerProvider();
            _AnalyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgDfaAnalyzer(CFG_ANALYZER_IDENTIFIER, CfgBuildingMode.Standard));
        }

        /// <summary>
        /// Perform parsing of the supplied Cobol or TypeCobol source file
        /// and compare actual diagnostics to expected diagnostics.
        /// </summary>
        /// <param name="sourceFilePath">Full path to the source file.</param>
        /// <param name="expectedDiagnosticsFilePath">Full path to the diagnostic file, pass null to skip
        /// diagnostic comparison</param>
        /// <returns>List of CFG built for the source file.</returns>
        public static IList<ControlFlowGraph<Node, object>> ParseCompareDiagnostics(string sourceFilePath, string expectedDiagnosticsFilePath = null)
        {
            var parser = Parser.Parse(sourceFilePath, DocumentFormat.RDZReferenceFormat, analyzerProvider: _AnalyzerProvider);
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

            if (results.TryGetAnalyzerResult(CFG_ANALYZER_IDENTIFIER, out IList<ControlFlowGraph<Node, object>> graphs))
            {
                return graphs;
            }

            throw new Exception($"No Control Flow Graph generated for file '{sourceFilePath}'.");
        }

        private static void GenDotCfg(ControlFlowGraph<Node, object> cfg, TextWriter writer, bool fullInstruction)
        {
            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<object> dotGen = new CfgDotFileForNodeGenerator<object>(cfg)
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
        public static void GenDotCfgFile(ControlFlowGraph<Node, object> cfg, string dotFilePath, bool bFullInstructions)
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
        public static void GenDotCfgAndCompare(ControlFlowGraph<Node, object> cfg, string testPath, string expectedDotFile, bool bFullInstruction)
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
