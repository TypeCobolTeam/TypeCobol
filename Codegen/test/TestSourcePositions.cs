using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Codegen.Test
{
    [TestClass]
    /// <summary>
    /// Test related to Source Position during codegen
    /// </summary>
    public class TestSourcePositions
    {
        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void BlankLine()
        {
            string file = Path.Combine("SourcePositions", "BlankLine");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void BlankMultiLines()
        {
            string file = Path.Combine("SourcePositions", "BlankMultiLines");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void CommentLine()
        {
            string file = Path.Combine("SourcePositions", "CommentLine");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void CommentMultiLines()
        {
            string file = Path.Combine("SourcePositions", "CommentMultiLines");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void DebugLine()
        {
            string file = Path.Combine("SourcePositions", "DebugLine");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void DebugMultiLines()
        {
            string file = Path.Combine("SourcePositions", "DebugMultiLines");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void SingleLineDirective()
        {
            string file = Path.Combine("SourcePositions", "SingleLineDirective");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

        [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void SplitDirective()
        {
            string file = Path.Combine("SourcePositions", "SplitDirective");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }

                [TestMethod]
        [TestCategory("Codegen.SourcePositions")]
        [TestProperty("Time", "fast")]
        public void MixedLinesWithDirectives()
        {
            string file = Path.Combine("SourcePositions", "MixedLinesWithDirectives");
            CodegenTestUtils.ParseGenerateCompare(file + ".cbl");
        }
    }
}
