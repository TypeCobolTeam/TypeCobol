using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using System.IO;
using System.Linq;
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

        private NodeListenerFactory _cfgBuilderNodeListenerFactory;
        private DefaultControlFlowGraphBuilder _cfgBuilder;

        private ControlFlowGraph<Node, object>[] AllGraphs()
        {
            return _cfgBuilder?.AllCfgBuilder?.Select(b => b.Cfg).ToArray();
        }

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

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 6);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsNull(results[4].ParentGraph);
            Assert.IsNull(results[5].ParentGraph);
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

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 8);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);

            //Proc0
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[1].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[1].ProgramOrFunctionNode.Name.Equals("Proc0"));

            //Proc1
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[2].ProgramOrFunctionNode.Name.Equals("Proc1"));

            //Nested0
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[3].ProgramOrFunctionNode.Name.Equals("Nested0"));
            Assert.IsNotNull(results[3].NestedGraphs);
            Assert.IsTrue(results[3].NestedGraphs.Count == 1);
            Assert.IsTrue(results[3].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[3].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("NestedProc0"));

            //Nested1
            Assert.IsTrue(results[4].ParentGraph == results[0]);
            Assert.IsTrue(results[4].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[4].ProgramOrFunctionNode.Name.Equals("Nested1"));
            Assert.IsNotNull(results[4].NestedGraphs);
            Assert.IsTrue(results[4].NestedGraphs.Count == 1);
            Assert.IsTrue(results[4].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[4].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("NestedProc1"));

            //Nested2
            Assert.IsTrue(results[5].ParentGraph == results[0]);
            Assert.IsTrue(results[5].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[5].ProgramOrFunctionNode.Name.Equals("Nested2"));
            Assert.IsNotNull(results[5].NestedGraphs);
            Assert.IsTrue(results[5].NestedGraphs.Count == 1);
            Assert.IsTrue(results[5].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[5].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("NestedProc2"));

            //Stacked0
            Assert.IsNull(results[6].ParentGraph);
            Assert.IsTrue(results[6].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[6].ProgramOrFunctionNode.Name.Equals("Stacked0"));
            Assert.IsNotNull(results[6].NestedGraphs);
            Assert.IsTrue(results[6].NestedGraphs.Count == 1);
            //StackedNestedProc0
            Assert.IsNotNull(results[6].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[6].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("StackedNestedProc0"));

            //Stacked1
            Assert.IsNull(results[7].ParentGraph);
            Assert.IsTrue(results[7].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[7].ProgramOrFunctionNode.Name.Equals("Stacked1"));
            Assert.IsNotNull(results[7].NestedGraphs);
            Assert.IsTrue(results[7].NestedGraphs.Count == 1);
            //StackedNestedProc1
            Assert.IsNotNull(results[7].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[7].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("StackedNestedProc1"));
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
            var results = AllGraphs();
            Assert.IsNotNull(results);
            Assert.IsTrue(results.Length == 1); //single program
            var cfg = results[0];
            Assert.IsNull(cfg.NestedGraphs); //no nested pgms
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

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 6);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsNull(results[4].ParentGraph);
            Assert.IsNull(results[5].ParentGraph);

            //We have taken the same CFG than for IfThenElseCascade0
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[2], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInNestedPrg1()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedPrg1");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 6);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsNull(results[4].ParentGraph);
            Assert.IsNull(results[5].ParentGraph);

            //We have taken the same CFG than for PerformProcedure0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "PerformProcedure0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[3], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInNestedPrg2()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedPrg2");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 6);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsNull(results[4].ParentGraph);
            Assert.IsNull(results[5].ParentGraph);

            //We have taken the same CFG than for MixPerformEvaluateIf0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "MixPerformEvaluateIf0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[1], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInStackedPrg0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInStackedPrg0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 6);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsNull(results[4].ParentGraph);
            Assert.IsNull(results[5].ParentGraph);

            //We have taken the same CFG than for PerformProcedure0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "PerformProcedure0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[4], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInStackedPrg1()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInStackedPrg1");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 6);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsNull(results[4].ParentGraph);
            Assert.IsNull(results[5].ParentGraph);

            //We have taken the same CFG than for MixPerformEvaluateIf0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "MixPerformEvaluateIf0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[5], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInProcedure0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInProcedure0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 8);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);

            //Proc0
            Assert.IsTrue(results[1].ParentGraph == results[0]);
            Assert.IsTrue(results[1].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[1].ProgramOrFunctionNode.Name.Equals("Proc0"));

            //We have taken the same CFG than for IfThenElseCascade0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[1], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInProcedure1()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInProcedure1");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            var results = AllGraphs();
            Assert.IsNotNull(results);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(results.Length == 8);
            Assert.IsTrue(results[0] == _cfgBuilder.Cfg);
            Assert.IsNull(results[0].ParentGraph);

            //Proc1
            Assert.IsTrue(results[2].ParentGraph == results[0]);
            Assert.IsTrue(results[2].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[2].ProgramOrFunctionNode.Name.Equals("Proc1"));

            //We have taken the same CFG than for ComplexGotoPara0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "ComplexGotoPara0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[2], path, expectedPath, true);
        }

        [TestMethod]
        public void CfgInNestedProcedure0()
        {
            string test = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "CfgInNestedProcedure0");
            string path = test + ".cbl";
            string expectedDiagnosticsFilePath = test + ".diag";
            CfgTestUtils.ParseCompareDiagnostics(path, expectedDiagnosticsFilePath);
            var results = AllGraphs();

            //Nested0
            Assert.IsTrue(results[3].ParentGraph == results[0]);
            Assert.IsTrue(results[3].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[3].ProgramOrFunctionNode.Name.Equals("Nested0"));
            Assert.IsNotNull(results[3].NestedGraphs);
            Assert.IsTrue(results[3].NestedGraphs.Count == 1);
            Assert.IsTrue(results[3].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[3].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("NestedProc0"));

            //Nested1
            Assert.IsTrue(results[4].ParentGraph == results[0]);
            Assert.IsTrue(results[4].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[4].ProgramOrFunctionNode.Name.Equals("Nested1"));
            Assert.IsNotNull(results[4].NestedGraphs);
            Assert.IsTrue(results[4].NestedGraphs.Count == 1);
            Assert.IsTrue(results[4].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[4].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("NestedProc1"));

            //Nested2
            Assert.IsTrue(results[5].ParentGraph == results[0]);
            Assert.IsTrue(results[5].ProgramOrFunctionNode is Program);
            Assert.IsTrue(results[5].ProgramOrFunctionNode.Name.Equals("Nested2"));
            Assert.IsNotNull(results[5].NestedGraphs);
            Assert.IsTrue(results[5].NestedGraphs.Count == 1);
            Assert.IsTrue(results[5].NestedGraphs[0].ProgramOrFunctionNode is FunctionDeclaration);
            Assert.IsTrue(results[5].NestedGraphs[0].ProgramOrFunctionNode.Name.Equals("NestedProc2"));

            //We have taken the same CFG than for ComplexGotoPara0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "ComplexGotoPara0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[3].NestedGraphs[0], path, expectedPath, true);

            //We have taken the same CFG than for IfThenElseCascade0  
            expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[4].NestedGraphs[0], path, expectedPath, true);

            //We have taken the same CFG than for PerformThru1  
            expectedPath = Path.Combine(Directory.GetCurrentDirectory(), BASIC_TESTS_DIR, "PerformThru1.dot");
            CfgTestUtils.GenDotCfgAndCompare(results[5].NestedGraphs[0], path, expectedPath, true);
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
                var results = AllGraphs();

                Assert.IsNotNull(results);
                Assert.IsTrue(results.Length > 0);
                CfgTestUtils.GenDotCfgFile(results[0], dotFilePath, fullInstruction);

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
