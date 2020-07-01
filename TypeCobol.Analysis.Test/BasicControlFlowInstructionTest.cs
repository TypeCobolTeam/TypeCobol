using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using System.IO;
using System.Runtime.CompilerServices;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Analysis.Graph;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for control from instructions.
    /// </summary>
    [TestClass]
    public class BasicControlFlowInstructionTest
    {
        private const string BASIC_TESTS_DIR = "BasicCfgInstrs";

        private static string GetThirdPartyDirectoryPath()
        {
            string currentDir = Directory.GetCurrentDirectory();
            string solutionDir = Path.GetDirectoryName(Path.GetDirectoryName(currentDir));
            Assert.IsNotNull(solutionDir);
            return Path.Combine(solutionDir, "TypeCobol.Test", "ThirdParty");
        }

        #region Some helper methods

        private static void AssertIsStacked(ControlFlowGraph<Node, object> graph, string expectedName)
        {
            Assert.IsNull(graph.ParentGraph);
            Assert.IsTrue(graph.ProgramOrFunctionNode is Program);
            Assert.AreEqual(expectedName, graph.ProgramOrFunctionNode.Name);
        }

        private static void AssertIsNested(ControlFlowGraph<Node, object> graph, ControlFlowGraph<Node, object> expectedParentGraph, string expectedName)
        {
            Assert.IsNotNull(graph.ParentGraph);
            Assert.AreEqual(expectedParentGraph, graph.ParentGraph);
            Assert.IsTrue(graph.ProgramOrFunctionNode is Program);
            Assert.AreEqual(expectedName, graph.ProgramOrFunctionNode.Name);
        }

        private static void AssertIsProcedure(ControlFlowGraph<Node, object> graph, ControlFlowGraph<Node, object> expectedParentGraph, string expectedName)
        {
            Assert.IsNotNull(graph.ParentGraph);
            Assert.AreEqual(expectedParentGraph, graph.ParentGraph);
            Assert.IsTrue(graph.ProgramOrFunctionNode is FunctionDeclaration);
            Assert.AreEqual(expectedName, graph.ProgramOrFunctionNode.Name);
            AssertHasNoNested(graph);
        }

        private static void AssertHasNoNested(ControlFlowGraph<Node, object> graph)
        {
            Assert.IsNull(graph.NestedGraphs);
        }

        private static IList<ControlFlowGraph<Node, object>> AssertHasNested(ControlFlowGraph<Node, object> graph, int expectedNestedCount)
        {
            Assert.IsNotNull(graph.NestedGraphs);
            Assert.AreEqual(expectedNestedCount, graph.NestedGraphs.Count);
            return graph.NestedGraphs;
        }

        private static Dictionary<string, ControlFlowGraph<Node, object>> CheckSimpleStructure(ControlFlowGraphBuilder<object> cfgBuilder)
        {
            var allGraphs = new Dictionary<string, ControlFlowGraph<Node, object>>();

            /*
             * StackedNestedPgms
             *   Nested0
             *   Nested1
             *   Nested2
             * Stacked0
             * Stacked1
             */

            var graphs = cfgBuilder.Graphs;
            Assert.IsNotNull(graphs);
            Assert.IsTrue(graphs.Count == 3);

            string name = "StackedNestedPgms";
            var stackedNestedPgms = graphs[0];
            allGraphs.Add(name, stackedNestedPgms);
            AssertIsStacked(stackedNestedPgms, name);
            var nestedInMain = AssertHasNested(stackedNestedPgms, 3);

            name = "Nested0";
            var nested0 = nestedInMain[0];
            allGraphs.Add(name, nested0);
            AssertIsNested(nested0, stackedNestedPgms, name);
            AssertHasNoNested(nested0);

            name = "Nested1";
            var nested1 = nestedInMain[1];
            allGraphs.Add(name, nested1);
            AssertIsNested(nested1, stackedNestedPgms, name);
            AssertHasNoNested(nested1);

            name = "Nested2";
            var nested2 = nestedInMain[2];
            allGraphs.Add(name, nested2);
            AssertIsNested(nested2, stackedNestedPgms, name);
            AssertHasNoNested(nested2);

            name = "Stacked0";
            var stacked0 = graphs[1];
            allGraphs.Add(name, stacked0);
            AssertIsStacked(stacked0, name);
            AssertHasNoNested(stacked0);

            name = "Stacked1";
            var stacked1 = graphs[2];
            allGraphs.Add(name, stacked1);
            AssertIsStacked(stacked1, name);
            AssertHasNoNested(stacked1);

            return allGraphs;
        }

        private static Dictionary<string, ControlFlowGraph<Node, object>> CheckComplexStructure(ControlFlowGraphBuilder<object> cfgBuilder)
        {
            var allGraphs = new Dictionary<string, ControlFlowGraph<Node, object>>();

            /*
             * StackedNestedPgms
             *   Proc0
             *   Proc1
             *   Nested0
             *     NestedProc0
             *   Nested1
             *     NestedProc1
             *   Nested2
             *     NestedProc2
             * Stacked0
             *   StackedNestedProc0
             * Stacked1
             *   StackedNestedProc1
             */

            var graphs = cfgBuilder.Graphs;
            Assert.IsNotNull(graphs);
            Assert.IsTrue(graphs.Count == 3);

            string name = "StackedNestedPgms";
            var stackedNestedPgms = graphs[0];
            allGraphs.Add(name, stackedNestedPgms);
            AssertIsStacked(stackedNestedPgms, name);
            var nestedInMain = AssertHasNested(stackedNestedPgms, 5);

            name = "Proc0";
            var proc0 = nestedInMain[0];
            allGraphs.Add(name, proc0);
            AssertIsProcedure(proc0, stackedNestedPgms, name);

            name = "Proc1";
            var proc1 = nestedInMain[1];
            allGraphs.Add(name, proc1);
            AssertIsProcedure(proc1, stackedNestedPgms, name);

            name = "Nested0";
            var nested0 = nestedInMain[2];
            allGraphs.Add(name, nested0);
            AssertIsNested(nested0, stackedNestedPgms, name);
            var nestedInNested0 = AssertHasNested(nested0, 1);

            name = "NestedProc0";
            var nestedProc0 = nestedInNested0[0];
            allGraphs.Add(name, nestedProc0);
            AssertIsProcedure(nestedProc0, nested0, name);

            name = "Nested1";
            var nested1 = nestedInMain[3];
            allGraphs.Add(name, nested1);
            AssertIsNested(nested1, stackedNestedPgms, name);
            var nestedInNested1 = AssertHasNested(nested1, 1);

            name = "NestedProc1";
            var nestedProc1 = nestedInNested1[0];
            allGraphs.Add(name, nestedProc1);
            AssertIsProcedure(nestedProc1, nested1, name);

            name = "Nested2";
            var nested2 = nestedInMain[4];
            allGraphs.Add(name, nested2);
            AssertIsNested(nested2, stackedNestedPgms, name);
            var nestedInNested2 = AssertHasNested(nested2, 1);

            name = "NestedProc2";
            var nestedProc2 = nestedInNested2[0];
            allGraphs.Add(name, nestedProc2);
            AssertIsProcedure(nestedProc2, nested2, name);

            name = "Stacked0";
            var stacked0 = graphs[1];
            allGraphs.Add(name, stacked0);
            AssertIsStacked(stacked0, name);
            var nestedInStacked0 = AssertHasNested(stacked0, 1);

            name = "StackedNestedProc0";
            var stackedNestedProc0 = nestedInStacked0[0];
            allGraphs.Add(name, stackedNestedProc0);
            AssertIsProcedure(stackedNestedProc0, stacked0, name);

            name = "Stacked1";
            var stacked1 = graphs[2];
            allGraphs.Add(name, stacked1);
            AssertIsStacked(stacked1, name);
            var nestedInStacked1 = AssertHasNested(stacked1, 1);

            name = "StackedNestedProc1";
            var stackedNestedProc1 = nestedInStacked1[0];
            allGraphs.Add(name, stackedNestedProc1);
            AssertIsProcedure(stackedNestedProc1, stacked1, name);

            return allGraphs;
        }

        #endregion

        private NodeListenerFactory _cfgBuilderNodeListenerFactory;
        private DefaultControlFlowGraphBuilder _cfgBuilder;

        [TestInitialize]
        public void TestInitialize()
        {
            //Allocate a static Default Control Flow Graph Builder
            _cfgBuilderNodeListenerFactory = () =>
            {
                _cfgBuilder = new DefaultControlFlowGraphBuilder();
                return _cfgBuilder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(_cfgBuilderNodeListenerFactory);
        }

        [TestCleanup]
        public void TestCleanup()
        {
            if (_cfgBuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(_cfgBuilderNodeListenerFactory);
            }
        }

        /// <summary>
        /// This is a simple test to ensure that given a Cobol program,
        /// all stacked and nested program are captured.
        /// </summary>
        [TestMethod]
        public void MixedStackedNestedPgms()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "MixedStackedNestedPgms");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            CheckSimpleStructure(_cfgBuilder);
        }

        /// <summary>
        /// This is a simple test to ensure that given a Cobol program,
        /// all stacked and nested program are captured.
        /// </summary>
        [TestMethod]
        public void MixedStackedNestedProcsPgms()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "MixedStackedNestedProcsPgms");
            string path = test + ".tcbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            CheckComplexStructure(_cfgBuilder);
        }

        /// <summary>
        /// This is a template method for a simple test on a source file
        /// containing a single program.
        /// Performs parsing, diagnostics comparison and CFG comparison.
        /// </summary>
        /// <remarks>
        /// All parameters are optional, by default :
        /// - input is BasicCfgInstrs\[CallerMethod].cbl
        /// - expected diagnostics is DotOutput\[CallerMethod].diag
        /// - expected result is DotOutput\[CallerMethod].dot
        /// - fullInstruction is True
        /// </remarks>
        /// <param name="inputDirectoryPath">Full path to the directory containing the input source file.</param>
        /// <param name="inputFileName">Name of the input source file without extension.</param>
        /// <param name="inputExtension">Extension of the input source file including the '.'.</param>
        /// <param name="expectedDiagnosticsDirectoryPath">Full path to the directory containing the expected diagnostics file.</param>
        /// <param name="expectedDiagnosticsFileName">Name of the expected diagnostics file without extension.</param>
        /// <param name="expectedDiagnosticsExtension">Extension of the expected diagnostics file including the '.'.</param>
        /// <param name="expectedResultDirectoryPath">Full path to the directory containing the expected result file.</param>
        /// <param name="expectedResultFileName">Name of the expected result file without extension.</param>
        /// <param name="expectedResultExtension">Extension of the expected result file including the '.'.</param>
        /// <param name="fullInstruction">True to use full instruction during dot generation.</param>
        private void TestTemplate(
            string inputDirectoryPath = null,
            [CallerMemberName] string inputFileName = null,
            string inputExtension = ".cbl",
            string expectedDiagnosticsDirectoryPath = null,
            [CallerMemberName] string expectedDiagnosticsFileName = null,
            string expectedDiagnosticsExtension = ".diag",
            string expectedResultDirectoryPath = null,
            [CallerMemberName] string expectedResultFileName = null,
            string expectedResultExtension = ".dot",
            bool fullInstruction = true)
        {
            string baseDir = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR);

            string inputFilePath = Path.Combine(inputDirectoryPath ?? baseDir, inputFileName + inputExtension);
            Assert.IsTrue(File.Exists(inputFilePath), $"Input file '{inputFilePath}' does not exist.");

            string expectedDiagnosticsFilePath = Path.Combine(expectedDiagnosticsDirectoryPath ?? baseDir, expectedDiagnosticsFileName + expectedDiagnosticsExtension);
            //No diag file when there is no diagnostics during parsing, so we don't check file existence here.

            string expectedResultFilePath = Path.Combine(expectedResultDirectoryPath ?? baseDir, expectedResultFileName + expectedResultExtension);
            Assert.IsTrue(File.Exists(expectedResultFilePath), $"Expected results file '{expectedResultFilePath}' does not exist.");

            CfgTestUtils.ParseCompareDiagnostics(inputFilePath, expectedDiagnosticsFilePath);
            var results = _cfgBuilder.Graphs;
            Assert.IsNotNull(results);
            Assert.IsTrue(results.Count == 1); //single program
            var cfg = results[0];
            Assert.IsNull(cfg.ParentGraph);
            Assert.IsTrue(cfg.ProgramOrFunctionNode is Program);
            Assert.IsNull(cfg.NestedGraphs);
            CfgTestUtils.GenDotCfgAndCompare(cfg, inputFilePath, expectedResultFilePath, fullInstruction);
        }

        [TestMethod]
        public void IfThen0() => TestTemplate();

        [TestMethod]
        public void IfThenAfter0() => TestTemplate();

        [TestMethod]
        public void IfThenElse0() => TestTemplate();

        [TestMethod]
        public void IfThenNextSentence0() => TestTemplate();

        [TestMethod]
        public void IfThenElseNextSentence1() => TestTemplate();

        [TestMethod]
        public void IfThenElseNextSentence2() => TestTemplate();

        [TestMethod]
        public void IfThenElseNextSentence0() => TestTemplate();

        [TestMethod]
        public void IfThenNested0() => TestTemplate();

        [TestMethod]
        public void IfThenNested1() => TestTemplate();

        [TestMethod]
        public void IfThenNested2() => TestTemplate();

        [TestMethod]
        public void IfThenElseNested0() => TestTemplate();

        [TestMethod]
        public void IfThenElseNested1() => TestTemplate();

        [TestMethod]
        public void IfThenElseCascade0() => TestTemplate();

        [TestMethod]
        public void SimpleGotoPara0() => TestTemplate();

        [TestMethod]
        public void ComplexGotoPara0() => TestTemplate();

        [TestMethod]
        public void Evaluate0() => TestTemplate();

        [TestMethod]
        public void EvaluateNoOther0() => TestTemplate();

        [TestMethod]
        public void EvaluateNoOther1() => TestTemplate();

        [TestMethod]
        public void EvaluateMultiWhen0() => TestTemplate();

        [TestMethod]
        public void Alter0() => TestTemplate();

        [TestMethod]
        public void Alter1() => TestTemplate();

        /// <summary>
        /// This ALTER test include qualified paragraph names.
        /// </summary>
        [TestMethod]
        public void Alter2() => TestTemplate();

        [TestMethod]
        public void GoBack0() => TestTemplate();

        [TestMethod]
        public void GoBack1() => TestTemplate();

        [TestMethod]
        public void Perform0() => TestTemplate();

        [TestMethod]
        public void PerformUntil0() => TestTemplate();

        [TestMethod]
        public void PerformUntil1() => TestTemplate();

        [TestMethod]
        public void PerformTime0() => TestTemplate();

        [TestMethod]
        public void PerformVarying0() => TestTemplate();

        [TestMethod]
        public void PerformProcedure0() => TestTemplate();

        [TestMethod]
        public void PerformProcedure1() => TestTemplate();

        [TestMethod]
        public void PerformNested0() => TestTemplate();

        [TestMethod]
        public void Search0() => TestTemplate();

        [TestMethod]
        public void SearchNextSentence0() => TestTemplate();

        [TestMethod]
        public void SearchCond0() => TestTemplate();

        [TestMethod]
        public void Declaratives0() => TestTemplate();

        [TestMethod]
        public void Declaratives1() => TestTemplate();

        [TestMethod]
        public void PerformThru0() => TestTemplate();

        [TestMethod]
        public void PerformThru1() => TestTemplate();

        [TestMethod]
        public void MixPerformEvaluateIf0() => TestTemplate();

        [TestMethod]
        public void PerformProcRecursive0() => TestTemplate();

        [TestMethod]
        public void CfgInNestedPrg0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedPrg0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckSimpleStructure(_cfgBuilder);

            //We have taken the same CFG than for IfThenElseCascade0
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Nested1"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInNestedPrg1()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedPrg1");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckSimpleStructure(_cfgBuilder);

            //We have taken the same CFG than for PerformProcedure0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "PerformProcedure0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Nested2"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInNestedPrg2()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedPrg2");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckSimpleStructure(_cfgBuilder);

            //We have taken the same CFG than for MixPerformEvaluateIf0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "MixPerformEvaluateIf0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Nested0"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInStackedPrg0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInStackedPrg0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckSimpleStructure(_cfgBuilder);

            //We have taken the same CFG than for PerformProcedure0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "PerformProcedure0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Stacked0"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInStackedPrg1()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInStackedPrg1");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckSimpleStructure(_cfgBuilder);

            //We have taken the same CFG than for MixPerformEvaluateIf0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "MixPerformEvaluateIf0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Stacked1"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInProcedure0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInProcedure0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckComplexStructure(_cfgBuilder);

            //We have taken the same CFG than for IfThenElseCascade0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Proc0"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInProcedure1()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInProcedure1");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckComplexStructure(_cfgBuilder);

            //We have taken the same CFG than for ComplexGotoPara0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "ComplexGotoPara0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["Proc1"], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInNestedProcedure0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedProcedure0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            var results = CheckComplexStructure(_cfgBuilder);

            //We have taken the same CFG than for ComplexGotoPara0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "ComplexGotoPara0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["NestedProc0"], path, expectedPath, true);

            //We have taken the same CFG than for IfThenElseCascade0  
            expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["NestedProc1"], path, expectedPath, true);

            //We have taken the same CFG than for PerformThru1  
            expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "PerformThru1.dot");
            CfgTestUtils.GenDotCfgAndCompare(results["NestedProc2"], path, expectedPath, true);
        }

        /// <summary>
        /// "dot.exe" -Tpng CGM110.dot -o CGM110.png
        /// "dot.exe" -Tsvg CGM110.dot -o CGM110.svg
        /// </summary>
        [TestMethod]
        public void OneThirdPartyCGM110()
        {
            string cnafBatch = Path.Combine(GetThirdPartyDirectoryPath(), "CNAF", "Batch");
            TestTemplate(cnafBatch,
                inputFileName: "CGM110",               //file name is different from test method here
                expectedDiagnosticsFileName: "CGM110", //file name is different from test method here
                expectedResultFileName: "CGM110",      //file name is different from test method here
                fullInstruction: false);
        }

        /// <summary>
        /// This test contains a PERFORM instruction to a PARAGRAPH that contains a GOTO to another
        /// Paragraph, there is an Diagnostic which is Raised.
        /// </summary>
        [TestMethod]
        public void OneThirdPartyIX105A()
        {
            string nist = Path.Combine(GetThirdPartyDirectoryPath(), "Nist");
            TestTemplate(nist,
                inputFileName: "IX105A",               //file name is different from test method here
                expectedDiagnosticsFileName: "IX105A", //file name is different from test method here
                expectedResultFileName: "IX105A");     //file name is different from test method here
        }

        /// <summary>
        /// This test contains a PERFORM instruction to a PARAGRAPH that contains a GOTO to another
        /// Paragraph, with Block recursion detection, there is an Diagnostic which is Raised.
        /// </summary>
        [TestMethod]
        public void OneThirdPartySG102A()
        {
            string nist = Path.Combine(GetThirdPartyDirectoryPath(), "Nist");
            TestTemplate(nist,
                inputFileName: "SG102A",               //file name is different from test method here
                expectedDiagnosticsFileName: "SG102A", //file name is different from test method here
                expectedResultFileName: "SG102A");     //file name is different from test method here
        }

        private void GenAllNistDots(bool fullInstruction)
        {
            string path = Path.Combine(GetThirdPartyDirectoryPath(), "Nist");
            string[] files = Directory.GetFiles(path, "*.cbl", SearchOption.AllDirectories);
            foreach (string f in files)
            {
                string dotFile = Path.GetFileNameWithoutExtension(f) + ".dot";
                string dotName = (fullInstruction ? string.Empty : "_") + dotFile;

                var nistOutput = Directory.CreateDirectory(Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "Nist"));
                string dotFilePath = Path.Combine(nistOutput.FullName, dotName);

                Parser.Parse(f, DocumentFormat.RDZReferenceFormat);
                var results = _cfgBuilder.Graphs;

                Assert.IsNotNull(results);
                Assert.IsTrue(results.Count > 0);
                CfgTestUtils.GenDotCfgFile(results[0], dotFilePath, fullInstruction);//Generate only main

                TestCleanup();
                TestInitialize();
            }
        }

        /// <summary>
        /// This Test is only used to generate all .dot files corresponding to the Nist source samples.
        /// This dot files contains instructions names only.
        /// </summary>
        [TestMethod]
        [Ignore] //Long execution time
        public void GenAllNistDots() => GenAllNistDots(false);

        /// <summary>
        /// This Test is only used to generate all .dot files corresponding to the Nist source samples.
        /// This dot files contains full instructions source code
        /// </summary>
        [TestMethod]
        [Ignore] //Long execution time
        public void GenAllNistSrcDots() => GenAllNistDots(true);
    }
}
