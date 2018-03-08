using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.LanguageServer.Test
{
    [TestClass]
    public class LSRTest
    {
        #region Incremental_Tests
        [TestMethod]
        [TestCategory("Incremental")]
        public void Incremental_RemoveDataDivisionCtrlZ()
        {
            LSRTestHelper.Test("Incremental_RemoveDataDivisionCtrlZ", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddCharBeginLine()
        {
            LSRTestHelper.Test("AddCharBeginLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddCharEndLine()
        {
            LSRTestHelper.Test("AddCharEndLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddCharMiddleLine()
        {
            LSRTestHelper.Test("AddCharMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddLineJumpMiddleLine()
        {
            LSRTestHelper.Test("AddLineJumpMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void AddMultipleLines()
        {
            LSRTestHelper.Test("AddMultipleLines", LsrTestingOptions.LsrParsingPhaseTesting);
        }
        
        [TestMethod]
        [TestCategory("Incremental")]
        public void AddMultipleLinesWithEmptyLineInside()
        {
            LSRTestHelper.Test("AddMultipleLinesWithEmptyLineInside", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteCharBeginLine()
        {
            LSRTestHelper.Test("DeleteCharBeginLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteCharEndLine()
        {
            LSRTestHelper.Test("DeleteCharEndLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteCharMiddleLine()
        {
            LSRTestHelper.Test("DeleteCharMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteMultipleLineButStopInMiddleLine()
        {
            LSRTestHelper.Test("DeleteMultipleLineButStopInMiddleLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteMultipleLines()
        {
            LSRTestHelper.Test("DeleteMultipleLines", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteMultipleLinesButStopAtEndLine()
        {
            LSRTestHelper.Test("DeleteMultipleLinesButStopAtEndLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void DeleteMultipleLineWithEmptyLineInside()
        {
            LSRTestHelper.Test("DeleteMultipleLineWithEmptyLineInside", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void SupprEndLineEmptyLine()
        {
            LSRTestHelper.Test("SupprEndLineEmptyLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void SupprEndLineTextedLine()
        {
            LSRTestHelper.Test("SupprEndLineTextedLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void SupprMultipleLinesFromMiddlePreviousLine()
        {
            LSRTestHelper.Test("SupprMultipleLinesFromMiddlePreviousLine", LsrTestingOptions.LsrParsingPhaseTesting);
        }
        #endregion

        #region Sementic_Tests
        [TestMethod]
        [TestCategory("Semantic")]
        public void RemoveDataDivisionCtrlZWithDiag()
        {
            LSRTestHelper.Test("RemoveDataDivisionCtrlZWithDiag", LsrTestingOptions.LsrSemanticPhaseTesting);
        }
        #endregion


        #region Completion_Tests
        [TestMethod]
        [TestCategory("Completion")]
        public void TypeCompletion()
        {
            LSRTestHelper.Test("TypeCompletion", LsrTestingOptions.LsrSemanticPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void SimpleMoveToCompletion()
        {
            LSRTestHelper.Test("SimpleMoveToCompletion", LsrTestingOptions.LsrSemanticPhaseTesting);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void VariableQualifiedNameCompletion()
        {
            LSRTestHelper.Test("VariableQualifiedNameCompletion", LsrTestingOptions.LsrSemanticPhaseTesting, true);
        }
        #endregion


    }
}
