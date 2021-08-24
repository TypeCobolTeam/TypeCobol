using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis.Util;
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
                var analyzerProvider = new AnalyzerProvider(str => throw new Exception(str));
                analyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(mode, o));
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
        /// <returns>An instance of <see cref="DfaTestResults" /> containing all graphs and DFA results.</returns>
        public static DfaTestResults ParseCompareDiagnosticsWithDfa(string sourceFilePath,
            string expectedDiagnosticsFilePath = null,
            string expectedLivenessFilePath = null)
        {
            var graphs = ParseCompareDiagnostics<DfaBasicBlockInfo<VariableSymbol>>(sourceFilePath, CfgBuildingMode.WithDfa, expectedDiagnosticsFilePath);
            var dfaResults = new DfaTestResults(graphs);

            if (expectedLivenessFilePath !=  null)
            {
                //Perform variable liveness (and UseDefs) data comparison

                if (!File.Exists(expectedLivenessFilePath))
                    throw new ArgumentException($"Expected DFA result file not found, invalid path is '{expectedLivenessFilePath}'.");

                StringWriter writer = new StringWriter();
                GenVariableLivenessResults(dfaResults, writer);

                string result = writer.ToString();
                string expected = File.ReadAllText(expectedLivenessFilePath);
                TestUtils.compareLines(sourceFilePath, result, expected, expectedLivenessFilePath);
            }

            return dfaResults;
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
            
            var parser = Parser.Parse(sourceFilePath, false, new TypeCobolOptions(), DocumentFormat.RDZReferenceFormat, analyzerProvider: analyzerProvider);
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

            string analyzerIdentifier = CfgDfaAnalyzerFactory.GetIdForMode(mode);
            if (results.TemporaryProgramClassDocumentSnapshot.AnalyzerResults.TryGetResult(analyzerIdentifier, out IList<ControlFlowGraph<Node, D>> graphs))
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

        /// <summary>
        /// Write DFA results into the supplied TextWriter.
        /// The results are written in CSV format, for each graph the file has 3 stacked parts:
        /// first part is a table of all instructions of the program,
        /// second part describes In, Out, Gen and Kill sets for each block,
        /// last part describes DefPoints for each UsePoint.
        /// </summary>
        /// <param name="dfaResults">Previously computed DFA results.</param>
        /// <param name="output">Target TextWriter.</param>
        public static void GenVariableLivenessResults(DfaTestResults dfaResults, TextWriter output)
        {
            for (int i = 0; i < dfaResults.Graphs.Count; i++)
            {
                //Each graph is identified by its index in the results
                var cfg = dfaResults.Graphs[i];
                output.WriteLine($"Graph {i};;;;");

                WriteInstructionTable();

                var defList = dfaResults.GetDefList(cfg);
                WriteLivenessSets();

                var useList = dfaResults.GetUseList(cfg);
                WriteUseDefsSets();

                //First part of the file: a table of all instructions of the program.
                //Each instruction is identified by its block and its index within all the instructions of the block.
                void WriteInstructionTable()
                {
                    output.WriteLine("Block;InstructionIndex;Instruction;;");
                    foreach (var block in cfg.AllBlocks)
                    {
                        int instructionIndex = 0;
                        foreach (var instruction in block.Instructions)
                        {
                            output.WriteLine($"Block{block.Index};{instructionIndex};{InstructionToString(instruction)};;");
                            instructionIndex++;
                        }
                    }

                    string InstructionToString(Node instruction)
                    {
                        string sourceText = instruction.CodeElement?.SourceText;
                        if (sourceText != null)
                        {
                            sourceText = sourceText.Trim();
                            sourceText = sourceText.Replace('\n', ' ')
                                .Replace('\r', ' '); //instruction written on a single line
                            sourceText = sourceText.Replace("\"", "\"\""); //escape double-quote
                            return '"' + sourceText + '"';
                        }

                        return "\"<empty>\"";
                    }

                    //instruction.CodeElement?.SourceText ?? "<empty>"
                }

                //Second part of the file: In, Out, Gen and Kill sets of each block
                //These sets contain DefPoints, each DefPoint is represented as "VarName:Block:InstructionIndex"
                void WriteLivenessSets()
                {
                    output.WriteLine("Block;In;Out;Gen;Kill");
                    foreach (var block in cfg.AllBlocks)
                    {
                        const string emptyData = "{};{};{};{}";
                        string @in = BitSetToString(block.Data.In, defList, DefPointToString);
                        string @out = BitSetToString(block.Data.Out, defList, DefPointToString);
                        string gen = BitSetToString(block.Data.Gen, defList, DefPointToString);
                        string kill = BitSetToString(block.Data.Kill, defList, DefPointToString);
                        string data = $"{@in};{@out};{gen};{kill}";
                        if (data != emptyData)
                        {
                            output.WriteLine($"Block{block.Index};{data}");
                        }
                    }
                }

                //Third part of the file: UseDef set of each UsePoint in the graph
                //UsePoint and DefPoint have the same representation as "VarName:Block:InstructionIndex"
                void WriteUseDefsSets()
                {
                    output.WriteLine("UsePoint;Definitions;;;");
                    foreach (var usePoint in useList)
                    {
                        string definitions = BitSetToString(usePoint.UseDef, defList, DefPointToString);
                        output.WriteLine($"{UsePointToString(usePoint)};{definitions};;;");
                    }
                }

                //Helper methods
                string DefPointToString(DfaDefPoint<Node, VariableSymbol> defPoint) => $"{defPoint.Variable.Name}:Block{defPoint.BlockIndex}:{defPoint.InstructionIndex}";

                string UsePointToString(DfaUsePoint<Node, VariableSymbol> usePoint) => $"{usePoint.Variable.Name}:Block{usePoint.BlockIndex}:{usePoint.InstructionIndex}";

                string BitSetToString<T>(BitSet set, List<T> list, Func<T, string> toString)
                {
                    return "{" + string.Join(", ", EnumerateSet().Select(toString)) + "}";

                    IEnumerable<T> EnumerateSet()
                    {
                        if (set == null) yield break;

                        int nextSetBit = -1;
                        while ((nextSetBit = set.NextSetBit(nextSetBit + 1)) >= 0)
                        {
                            yield return list[nextSetBit];
                        }
                    }
                }
            }
        }
    }
}
