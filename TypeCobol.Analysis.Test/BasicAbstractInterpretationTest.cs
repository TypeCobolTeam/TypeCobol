using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using System.IO;
using System.Runtime.CompilerServices;
using TypeCobol.Analysis.Graph;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for control from instructions.
    /// </summary>
    [TestClass]
    public class BasicAbstractInterpretationTest
    {
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
        /// - CFG building mode is Standard
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
        /// <param name="mode">CFG Building mode.</param>
        /// <param name="fullInstruction">True to use full instruction during dot generation.</param>
        private void TestTemplate(
            string inputDirectoryPath = null,
            [CallerMemberName] string inputFileName = null,
            string inputExtension = ".cbl",
            string expectedResultDirectoryPath = null,
            [CallerMemberName] string expectedResultFileName = null,
            string expectedResultExtension = ".int",
            CfgBuildingMode mode = CfgBuildingMode.Standard,
            bool fullInstruction = true)
        {
            string inputFilePath = Path.Combine(inputDirectoryPath ?? BasicCfgInstrs, inputFileName + inputExtension);
            Assert.IsTrue(File.Exists(inputFilePath), $"Input file '{inputFilePath}' does not exist.");

            string expectedResultFilePath = Path.Combine(expectedResultDirectoryPath ?? BasicCfgInstrs, expectedResultFileName + expectedResultExtension);
            Assert.IsTrue(File.Exists(expectedResultFilePath), $"Expected results file '{expectedResultFilePath}' does not exist.");

            var graphs = ParseCompareDiagnostics<object>(inputFilePath, mode);
            Assert.IsTrue(graphs.Count == 1); //single program
            var cfg = graphs[0];
            Assert.IsNull(cfg.ParentGraph);
            Assert.IsTrue(cfg.ProgramOrFunctionNode is Program);
            Assert.IsNull(cfg.NestedGraphs);
            GenAbstractInterpretCfgAndCompare(cfg, inputFilePath, expectedResultFilePath, fullInstruction);
        }

        [TestMethod]
        public void IfThen0() => TestTemplate();

        [TestMethod]
        public void IfThenElse0() => TestTemplate();

        [TestMethod]
        public void IfThenNested0() => TestTemplate();

        [TestMethod]
        public void IfThenNested1() => TestTemplate();

        [TestMethod]
        public void IfThenNested2() => TestTemplate();

        [TestMethod]
        public void IfThenElseCascade0() => TestTemplate();

        [TestMethod]
        public void IfThenElseNested1() => TestTemplate();

        [TestMethod]
        public void IfThenNextSentence0() => TestTemplate();

        [TestMethod]
        public void IfThenElseNextSentence0() => TestTemplate();

        [TestMethod]
        public void IfThenElseNextSentence1() => TestTemplate();

        [TestMethod]
        public void IfThenElseNextSentence2() => TestTemplate();

        [TestMethod]
        public void Evaluate0() => TestTemplate();

        [TestMethod]
        public void EvaluateMultiWhen0() => TestTemplate();

        [TestMethod]
        public void EvaluateNoOther0() => TestTemplate();
        [TestMethod]
        public void MixPerformEvaluateIf0() => TestTemplate();
        [TestMethod]
        public void ComplexGotoPara0() => TestTemplate();

        [TestMethod]
        public void Declaratives0() => TestTemplate();

        [TestMethod]
        public void Declaratives1() => TestTemplate();

    }
}
