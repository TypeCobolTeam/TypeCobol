using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class LSRTest
    {
        [TestMethod]
        public void Incremental_RemoveDataDivisionCtrlZ()
        {
            LSRTestHelper.Test("Incremental_RemoveDataDivisionCtrlZ", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        public void AddCharBeginLine()
        {
            LSRTestHelper.Test("AddCharBeginLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void AddCharEndLine()
        {
            LSRTestHelper.Test("AddCharEndLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void AddCharMiddleLine()
        {
            LSRTestHelper.Test("AddCharMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void AddLineJumpMiddleLine()
        {
            LSRTestHelper.Test("AddLineJumpMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void AddMultipleLines()
        {
            LSRTestHelper.Test("AddMultipleLines", LsrTestingOptions.LsrParsingPhaseTesting);
        }
        
        [TestMethod]
        public void AddMultipleLinesWithEmptyLineInside()
        {
            LSRTestHelper.Test("AddMultipleLinesWithEmptyLineInside", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteCharBeginLine()
        {
            LSRTestHelper.Test("DeleteCharBeginLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteCharEndLine()
        {
            LSRTestHelper.Test("DeleteCharEndLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteCharMiddleLine()
        {
            LSRTestHelper.Test("DeleteCharMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteMultipleLineButStopInMiddleLine()
        {
            LSRTestHelper.Test("DeleteMultipleLineButStopInMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteMultipleLines()
        {
            LSRTestHelper.Test("DeleteMultipleLines", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteMultipleLinesButStopAtEndLine()
        {
            LSRTestHelper.Test("DeleteMultipleLinesButStopAtEndLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void DeleteMultipleLineWithEmptyLineInside()
        {
            LSRTestHelper.Test("DeleteMultipleLineWithEmptyLineInside", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void SupprEndLineEmptyLine()
        {
            LSRTestHelper.Test("SupprEndLineEmptyLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void SupprEndLineTextedLine()
        {
            LSRTestHelper.Test("SupprEndLineTextedLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        public void SupprMultipleLinesFromMiddlePreviousLine()
        {
            LSRTestHelper.Test("SupprMultipleLinesFromMiddlePreviousLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }


    }
}
