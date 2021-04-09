using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser.Incremental {


    [TestClass]
    public class TestChangeEvent
    {
        static readonly string Root = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Programs" + Path.DirectorySeparatorChar + "Cobol85";

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "long")]
        public void Check_BeforeAfterInsertionBatched()
        {
            Paths paths = new Paths(Root, Root, Root + Path.DirectorySeparatorChar + "Simple.pgm", new Multipass.IndexNames());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Parse();

            var e = UpdateLine(TextChangeType.LineInserted, 2, "END PROGRAM Simple.");
            e = UpdateLine(TextChangeType.LineUpdated, 1, "PROGRAM-ID. Simpler.", e);

            // clear document
            unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
            unit.Parse();

            var names = unit.Comparator.paths.Resultnames as Multipass.IndexNames;
            if (names != null) names.index = 2;
            Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
            unit.Compare();//with Simple.2.txt
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "long")]
        public void Check_BeforeAfterInsertion()
        {
            Paths paths = new Paths(Root, Root, Root + Path.DirectorySeparatorChar + "Simple.pgm", new Multipass.IndexNames());
            TestUnit unit = new TestUnit(new Multipass(paths));
            unit.Parse();
            var names = unit.Comparator.paths.Resultnames as Multipass.IndexNames;
            if (names != null)
            {
                names.index = 0;
                unit.Compare();//with Simple.0.txt

                // explicitely close program by adding END PROGRAM line
                var e2 = UpdateLine(TextChangeType.LineInserted, 2, "END PROGRAM Simple.");
                var e = UpdateLine(TextChangeType.LineUpdated, 1, "  PROGRAM-ID. Simple.");
                e.TextChanges.Add(e2.TextChanges[0]);
                unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
                unit.Parse();
                names.index++;
                Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
                unit.Compare();//with Simple.1.txt

                // change program name ; now first and last line have differing program id
                e = UpdateLine(TextChangeType.LineUpdated, 1, "PROGRAM-ID. Simpler.");
                unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
                unit.Parse();
                names.index++;
                Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
                unit.Compare();//with Simple.2.txt

                // clear document
                e = UpdateLine(TextChangeType.DocumentCleared, /* the following parameters are not used when DocumentCleared*/ 0, null);
                unit.Compiler.CompilationResultsForProgram.UpdateTextLines(e);
                unit.Parse();
                names.index++;
            }
            Console.WriteLine("Compare with result file: " + unit.Comparator.paths.Result);
            unit.Compare();//with Simple.3.txt
        }


        public static TextChangedEvent UpdateLine(TextChangeType type, int line, string text, TextChangedEvent e = null)
        {
            if (e == null) e = new TextChangedEvent();
            ITextLine snapshot = new TextLineSnapshot(line, text, null);
            e.TextChanges.Add(new TextChange(type, line, snapshot));
            return e;
        }
    }

    /// <summary>
    /// These tests are going to check if the document's line update works fine. 
    /// </summary>
    [TestClass]
    public class IncrementalTextLineChanges
    {
        static readonly string Root = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar +
                                      "Incremental" + Path.DirectorySeparatorChar + "TextLineChanges";

        private Paths UnitTestPaths = null;
        private TestUnit UnitTest = null;

        /// <summary>
        /// This method will initialize the paths and the unit. It will also parse the tcbl program once. 
        /// </summary>
        /// <param name="inputFileName">File name of the test</param>
        private void InitUnitTest(string inputFileName)
        {
            UnitTestPaths = new Paths(Root, Root, Root + Path.DirectorySeparatorChar + inputFileName, new Multipass.IndexNames());
            UnitTest = new TestUnit(new Multipass(UnitTestPaths));
            //Parser file for the first time
            UnitTest.Parse();
        }

        private void CompareTextDocuments()
        {
            //Get document's lines as string
            var stringDocument = GetTextDocument(UnitTest.Compiler.CompilationResultsForProgram.CobolTextLines);
            //Compare with result file
            UnitTest.Compare(stringDocument);
        }
        private string GetTextDocument(IReadOnlyList<ICobolTextLine> cobolTextLines)
        {
            var sb = new StringBuilder();
            foreach (var cobolTextLine in cobolTextLines)
            {
                sb.AppendLine(cobolTextLine.SourceText);
            }
            return sb.ToString();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_AddCharBeginLine()
        {
            InitUnitTest("AddCharBeginLine.tcbl");

            var updateLine = TestChangeEvent.UpdateLine(TextChangeType.LineUpdated, 3, "       CDATA DIVISION.");
            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(updateLine);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_AddCharMiddleLine()
        {
            InitUnitTest("AddCharMiddleLine.tcbl");

            var updateLine = TestChangeEvent.UpdateLine(TextChangeType.LineUpdated, 3, "       DATA DIVCISION.");
            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(updateLine);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_AddCharEndLine()
        {
            InitUnitTest("AddCharEndLine.tcbl");

            var updateLine = TestChangeEvent.UpdateLine(TextChangeType.LineUpdated, 3, "       DATA DIVISION.C");
            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(updateLine);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_AddLineJumpMiddleLine()
        {
            InitUnitTest("AddLineJumpMiddleLine.tcbl");

            var lineJumpChanges = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
            lineJumpChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "       DATA        ").TextChanges[0]);
            lineJumpChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 4, "       DIVISION.").TextChanges[0]);
            
            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(lineJumpChanges);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_DeleteCharBeginLine()
        {
            InitUnitTest("DeleteCharBeginLine.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "       ATA DIVISION.").TextChanges[0]);

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_DeleteCharMiddleLine()
        {
            InitUnitTest("DeleteCharMiddleLine.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "       DATA DIISION.").TextChanges[0]);

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_DeleteCharEndLine()
        {
            InitUnitTest("DeleteCharEndLine.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "       DATA DIVISION").TextChanges[0]);

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_SupprEndLineEmptyLine()
        {
            InitUnitTest("SupprEndLineEmptyLine.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 1, "");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 1, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 1, "       PROGRAM-ID. ProcCall.").TextChanges[0]);

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_SupprEndLineTextedLine()
        {
            InitUnitTest("SupprEndLineTextedLine.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "       DATA DIVISION.       WORKING-STORAGE SECTION.").TextChanges[0]);

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }


        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_AddMultipleLines()
        {
            InitUnitTest("AddMultipleLines.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 6, "       01 mytest     TYPE BOOL.");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 7, "       01 myzef TYPE DATE.").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 8, "       01 zfzef TYPE BOOL.").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 9, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 10, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 11, "       01 fzef TYPE CountryISO2.").TextChanges[0]);
            

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Incremental_DeleteMultipleLines()
        {
            InitUnitTest("DeleteMultipleLines.tcbl");

            var changes = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "");
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 5, "").TextChanges[0]);
            changes.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 5, "       01 W-TechCTX TYPE TechnicalContext.").TextChanges[0]);

            //Update text document
            UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(changes);

            CompareTextDocuments();
        }

        [TestMethod]
        [TestCategory("Incremental")]
        [TestProperty("Time", "fast")]
        public void Check_TextLineChange()
        {
            InitUnitTest("TextLineIncremental.tcbl");

            //Get existing result files
            var names = UnitTest.Comparator.paths.Resultnames as Multipass.IndexNames;
            if (names != null)
            {
                names.index = 0;
                //Delete multiple lines 
                var firstChanges = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
                firstChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "").TextChanges[0]);
                firstChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "").TextChanges[0]);
                firstChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "").TextChanges[0]);
                firstChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "").TextChanges[0]);
                firstChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "").TextChanges[0]);

                //Update text document
                UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(firstChanges);
                //Compare
                CompareTextDocuments();

                //Simulate a Ctrl+Z
                var secondChanges = TestChangeEvent.UpdateLine(TextChangeType.LineRemoved, 3, "");
                secondChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 3, "       DATA DIVISION.").TextChanges[0]);
                secondChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 4, "       WORKING-STORAGE SECTION.").TextChanges[0]);
                secondChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 5, "       01 W-TechCTX TYPE TechnicalContext.").TextChanges[0]);
                secondChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 6, "       01 mytest     TYPE BOOL.").TextChanges[0]);
                secondChanges.TextChanges.Add(TestChangeEvent.UpdateLine(TextChangeType.LineInserted, 7, "       01 myzef TYPE DATE.").TextChanges[0]);

                //Update text document
                UnitTest.Compiler.CompilationResultsForProgram.UpdateTextLines(secondChanges);
                //Compare
                names.index++;
                CompareTextDocuments();
            }
        }
    }
}
