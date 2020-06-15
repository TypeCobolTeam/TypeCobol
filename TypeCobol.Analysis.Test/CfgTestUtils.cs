using System;
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
        /// <summary>
        /// Perform parsing of the supplied Cobol or TypeCobol source file
        /// and compare actual diagnostics to expected diagnostics.
        /// </summary>
        /// <param name="sourceFilePath">Full path to the source file.</param>
        /// <param name="expectedDiagnosticsFilePath">Full path to the diagnostic file.</param>
        /// <returns>An instance of <see cref="CompilationUnit"/> containing all parsing results</returns>
        public static CompilationUnit ParseCompareDiagnostics(string sourceFilePath, string expectedDiagnosticsFilePath)
        {
            var parser = Parser.Parse(sourceFilePath, DocumentFormat.RDZReferenceFormat);
            var results = parser.Results;
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
                    errorMessage.AppendLine($"Expected diagnostic file path was '{expectedDiagnosticsFilePath ?? "NULL"}'.");
                    errorMessage.Append(diagnosticsText);
                    throw new Exception(errorMessage.ToString());
                }
            }

            return results;
        }

        /// <summary>
        /// Generates a Dot CFG file
        /// </summary>
        /// <param name="cfg">The CFG to generate the dot file</param>
        /// <param name="dotFilePath">The Dot File to be generated.</param>
        /// <param name="bFullInstructions">true if full instruction source code must be generated, false if only instruction names.</param>
        public static void GenDotCfgFile(ControlFlowGraph<Node, object> cfg, string dotFilePath, bool bFullInstructions)
        {
            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<object> dotGen = new CfgDotFileForNodeGenerator<object>(cfg)
                                                        {
                                                            FullInstruction = bFullInstructions,
                                                            Filepath = dotFilePath
                                                        };
            dotGen.Report();
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
            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<object> dotGen = new CfgDotFileForNodeGenerator<object>(cfg)
                                                        {
                                                            FullInstruction = bFullInstruction
                                                        };
            StringWriter writer = new StringWriter();
            dotGen.Report(writer);

            // compare with expected result
            string result = writer.ToString();
            string expected = File.ReadAllText(expectedDotFile);
            TestUtils.compareLines(testPath, result, expected, expectedDotFile);
        }
    }
}
