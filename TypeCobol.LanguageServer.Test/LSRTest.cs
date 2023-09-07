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

        [TestMethod]
        [TestCategory("Incremental")]
        public void MultiLineCommentIncScan()
        {
            LSRTestHelper.Test("MultiLineCommentIncScan", LsrTestingOptions.NoLsrTesting, false);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void FormalizedCommentIncScan()
        {
            LSRTestHelper.Test("FormalizedCommentIncScan", LsrTestingOptions.NoLsrTesting, false, true);
        }

        [TestMethod]
        [TestCategory("Incremental")]
        public void IncrementalErrorHandling()
        {
            LSRTestHelper.Test("IncrementalErrorHandling", LsrTestingOptions.NoLsrTesting, false, true);
        }

        /// <summary>
        /// This test is releated to issue:https://github.com/TypeCobolTeam/TypeCobol/issues/1350
        /// The following steps were raising an exception, before the fix.
        /// 
        /// Inside TypeCobol Editor: 
        /// - Go to the beginning of a line for instance in the middle of a document
        /// - Ctrl+Shift+End
        /// - Suppr
        /// - Ctrl+S
        /// - Ctrl+Z
        /// </summary>
        [TestMethod]
        [TestCategory("Incremental")]
        public void SelectEndSuppressSaveUndo()
        {
            LSRTestHelper.Test("SelectEndSuppressSaveUndo", LsrTestingOptions.NoLsrTesting, false);
        }

        /// <summary>
        /// This test is releated to issue:https://github.com/TypeCobolTeam/TypeCobol/issues/1350
        /// The following steps were raising an exception, before the fix.
        /// 
        /// Inside TypeCobol Editor: 
        /// - Goto the beginning of the first line of a document.
        /// - Enter
        /// - Enter
        /// </summary>
        [TestMethod]
        [TestCategory("Incremental")]
        public void BeginTopLineEnterEnter()
        {
            LSRTestHelper.Test("BeginTopLineEnterEnter", LsrTestingOptions.NoLsrTesting, false);
        }

        /// <summary>
        /// Test for the fix of:https://github.com/TypeCobolTeam/TypeCobol/issues/1351 
        /// </summary>
        [TestMethod]
        [TestCategory("Incremental")]
        public void FirstLineEditCommentBeforeIdentDiv()
        {
            LSRTestHelper.Test("FirstLineEditCommentBeforeIdentDiv", LsrTestingOptions.NoLsrTesting, false, true);
        }

        /// <summary>
        /// Test for the fix of:https://github.com/TypeCobolTeam/TypeCobol/issues/1370 
        /// </summary>
        [TestMethod]
        [TestCategory("Incremental")]
        public void IncrementalThruTokenInsideCopies()
        {
            LSRTestHelper.Test("IncrementalThruTokenInsideCopies", LsrTestingOptions.NoLsrTesting, false, true);
        }

        /// <summary>
        /// Test the error reporting when modifying a continued line
        /// </summary>
        [TestMethod]
        [TestCategory("Incremental")]
        [Ignore] //Not fixed yet ! No error should be reported when adding a blank line after a continued line
        public void InsertWithinContinuations()
        {
            LSRTestHelper.Test("InsertWithinContinuations", LsrTestingOptions.NoLsrTesting, false);
        }

        #endregion

        #region SyntaxColoring
        [TestMethod]
        [TestCategory("SyntaxColoring")]
        public void MultiCommentSyntaxColoring()
        {
            LSRTestHelper.Test("MultiCommentSyntaxColoring", LsrTestingOptions.NoLsrTesting, false, true);
        }
        #endregion

        #region Sementic_Tests
        [TestMethod]
        [TestCategory("Semantic")]
        public void RemoveDataDivisionCtrlZWithDiag()
        {
            LSRTestHelper.Test("RemoveDataDivisionCtrlZWithDiag", LsrTestingOptions.NoLsrTesting, true);
        }
        #endregion

        #region Sementic_Tests
        [TestMethod]
        [TestCategory("Semantic")]
        public void MultiLineBlockDeleteUndo()
        {
            LSRTestHelper.Test("MultiLineBlockDeleteUndo", LsrTestingOptions.NoLsrTesting, false);
        }
        #endregion

        #region Completion_Tests
        [TestMethod]
        [TestCategory("Completion")]
        public void TypeCompletion()
        {
            LSRTestHelper.Test("TypeCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void SimpleMoveToCompletion()
        {
            LSRTestHelper.Test("SimpleMoveToCompletion", LsrTestingOptions.NoLsrTesting, true);
            LSRTestHelper.Test("SimpleMoveToCompletionNoTC", LsrTestingOptions.NoLsrTesting, true, pureCobol: true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void VariableQualifiedNameCompletion()
        {
            LSRTestHelper.Test("VariableQualifiedNameCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void VariableQualifiedNameCompletion2()
        {
            LSRTestHelper.Test("VariableQualifiedNameCompletion2", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionAfterBetweenColons()
        {
            LSRTestHelper.Test("CompletionAfterBetweenColons", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionAfterDotFromTypedDataDef()
        {
            LSRTestHelper.Test("CompletionAfterDotFromTypedDataDef", LsrTestingOptions.NoLsrTesting, true, false, false, null, null, "CustomDependencies");
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionAfterWhen()
        {
            LSRTestHelper.Test("CompletionAfterWhen", LsrTestingOptions.NoLsrTesting, true, false, true);
            LSRTestHelper.Test("CompletionAfterWhen2", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionAfterAddTo()
        {
            LSRTestHelper.Test("CompletionAfterAddTo", LsrTestingOptions.NoLsrTesting, true, false, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletion()
        {
            LSRTestHelper.Test("ProcedureCompletion", LsrTestingOptions.NoLsrTesting, true, false, false, null, null, "CustomDependencies");
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void PerformCompletion()
        {
            LSRTestHelper.Test("PerformCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void SetToAddressOfCompletion()
        {
            LSRTestHelper.Test("SetToAddressOfCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void IfCompletion()
        {
            LSRTestHelper.Test("IfCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void DisplayCompletion()
        {
            LSRTestHelper.Test("DisplayCompletion", LsrTestingOptions.NoLsrTesting, true);
            LSRTestHelper.Test("DisplayCompletionNoTC", LsrTestingOptions.NoLsrTesting, true, pureCobol: true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void NestedProgramCompletion()
        {
            LSRTestHelper.Test("CompletionWithNestedProgram", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void MoveZeroSpaceCompletion()
        {
            LSRTestHelper.Test("MoveZeroSpaceCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void OfCompletion()
        {
            LSRTestHelper.Test("OfCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void GlobalWithNestedProgramCompletion()
        {
            LSRTestHelper.Test("GlobalWithNestedProgramCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void QualifiedNameCompletionWithFiller()
        {
            LSRTestHelper.Test("QualifiedNameCompletionWithFiller", LsrTestingOptions.NoLsrTesting, true);
        }


        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletionCaseInsensitive()
        {
            LSRTestHelper.Test("ProcedureCompletionCaseInsensitive", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletionInputInoutOutput()
        {
            LSRTestHelper.Test("ProcedureCompletionInputInoutOutput", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletionInputInoutOutput_Case()
        {
            LSRTestHelper.Test("ProcedureCompletionInputInoutOutput-Case", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletionInputInoutOutput_NoContext()
        {
            LSRTestHelper.Test("ProcedureCompletionInputInoutOutput-NoContext", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletionNoParam()
        {
            LSRTestHelper.Test("ProcedureCompletionNoParam", LsrTestingOptions.NoLsrTesting);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ProcedureCompletionPotentialChildrenWithNoName()
        {
            LSRTestHelper.Test("ProcedureCompletionPotentialChildrenWithNoName", LsrTestingOptions.NoLsrTesting, true);
        }

#if EUROINFO_RULES
        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionUsingCopy()
        {
            LSRTestHelper.Test("CompletionUsingCopy", LsrTestingOptions.NoLsrTesting, true, false, false, "CopyFolder");
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void ReplacingSyntaxOff()
        {
            LSRTestHelper.Test("replacingSyntaxOff", LsrTestingOptions.NoLsrTesting, true, false, false, "CopyFolder");
        }

        [TestMethod]
        [TestCategory("EI-Specific")]
        public void EI_ExtractRemarksData()
        {
            LSRTestHelper.Test("EI_ExtractRemarksData", LsrTestingOptions.NoLsrTesting, true, false, false, "CopyFolder");
            LSRTestHelper.Test("EI_ExtractRemarksData_CPYInsideCPX", LsrTestingOptions.NoLsrTesting, true, false, false, "CopyFolder");
            LSRTestHelper.Test("EI_ExtractRemarksData_DebugLines", LsrTestingOptions.NoLsrTesting, true);
        }
#endif

        [TestMethod]
        [TestCategory("GetDataLayout")]
        public void GetDataLayoutRequest()
        {
            LSRTestHelper.Test("GetDataLayoutRequest", LsrTestingOptions.NoLsrTesting);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void GlobalStorageCompletion()
        {
            LSRTestHelper.Test("GlobalStorageCompletion", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionOnPartialLibraryName()
        {
            LSRTestHelper.Test("CompletionOnPartialLibraryName", LsrTestingOptions.NoLsrTesting, true, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionOnPartialTypeName()
        {
            LSRTestHelper.Test("CompletionOnPartialTypeName", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionOutputParamEmptyType()
        {
            LSRTestHelper.Test("CompletionOutputParamEmptyType", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void CompletionCallDuplicateProcedure()
        {
            LSRTestHelper.Test("CompletionCallDuplicateProcedure", LsrTestingOptions.NoLsrTesting, true);
        }

        [TestMethod]
        [TestCategory("Completion")]
        public void AmbiguousVariablesCompletion()
        {
            LSRTestHelper.Test("AmbiguousVariablesCompletion", LsrTestingOptions.NoLsrTesting, true);
            LSRTestHelper.Test("AmbiguousVariablesCompletionNoTC", LsrTestingOptions.NoLsrTesting, true, pureCobol: true);
        }
        #endregion

        [TestMethod]
        public void GoToDefinition()
        {
            LSRTestHelper.Test("GoToDefinition", LsrTestingOptions.NoLsrTesting, true, false, false, "CopyFolder");
        }

        [TestMethod]
        public void OnOutlineRefresh()
        {
            LSRTestHelper.Test("OutlineRefresh", LsrTestingOptions.NoLsrTesting, true, useOutline: true);
        }

        [TestMethod]
        public void OnHover()
        {
            LSRTestHelper.Test("OnHover", LsrTestingOptions.NoLsrTesting, true, true);
        }

        [TestMethod]
        public void OnHoverDisplayVariableDefinition()
        {
            LSRTestHelper.Test("OnHoverDisplayVariableDefinition", LsrTestingOptions.NoLsrTesting, true, true);
            LSRTestHelper.Test("OnHoverDisplayVariableDefinitionFromCopy", LsrTestingOptions.NoLsrTesting, true, false, false, "CopyFolder");
        }

        [TestMethod]
        public void RefreshCopiesNotif()
        {
            LSRTestHelper.Test("RefreshCopiesNotif", LsrTestingOptions.NoLsrTesting, true, true);
        }

        [TestMethod]
        public void LSRAutoReplaceSecurityTest()
        {
            if (global::CLI.Test.UnitTestHelper.CompareDirectory(null, null, "TypeCobol.LanguageServer.Test\\LSRTests"))
            {
                Assert.Fail("Set AutoReplace to false in UnitTestHelper.CompareDirectory()\n\n");
            }
        }

        [TestMethod]
        [TestCategory("CfgDfa")]
        public void CfgDataInformation()
        {
            LSRTestHelper.Test("CfgDataInformation", LsrTestingOptions.NoLsrTesting, true, useCfg:true);
        }

        [TestMethod]
        public void EmptyExecStatement()
        {
            //Issue #1900, initially a NullReferenceException was thrown instead of a proper diagnostic
            LSRTestHelper.Test("EmptyExecStatement", LsrTestingOptions.NoLsrTesting);
        }

        [TestMethod]
        [TestCategory("CopyEdit")]
        public void CopyEditAddDataItem()
        {
            LSRTestHelper.Test("CopyEditAddDataItem", LsrTestingOptions.NoLsrTesting, true);
        }
    }
}
