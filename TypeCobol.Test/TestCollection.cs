using System;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.File;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Compiler.Preprocessor;
using TypeCobol.Test.Compiler.Scanner;
using TypeCobol.Test.Compiler.Text;

namespace TypeCobol.Test {

    [TestClass]
    public class TestCollection
    {
        static readonly string root = PlatformUtils.GetPathForProjectFile("Compiler" + Path.DirectorySeparatorChar + "Parser");
        static readonly string sampleRoot = root + Path.DirectorySeparatorChar + "Samples";
        static readonly string resultRoot = root + Path.DirectorySeparatorChar + "ResultFiles";

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckFile()
        {
            TestIBMCodePages.Check_GetDotNetEncoding();
            TestIBMCodePages.Check_IsEBCDICCodePage();
            TestIBMCodePages.Check_DBCSCodePageNotSupported();

            TestCobolFile.Check_EBCDICCobolFile();
            TestCobolFile.Check_EBCDICCobolFileWithUnsupportedChar();
            TestCobolFile.Check_ASCIICobolFile_ReferenceFormat();
            TestCobolFile.Check_ASCIICobolFile_LinuxReferenceFormat();
            TestCobolFile.Check_ASCIICobolFile_FreeTextFormat();

            TestCobolFile.Check_UTF8File();
        }

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckText()
        {
            TestReadOnlyTextDocument.Check_DocumentFormatExceptions();

            TestReadOnlyTextDocument.Check_EmptyDocument();
            TestReadOnlyTextDocument.Check_ReferenceFormatDocument();
            TestReadOnlyTextDocument.Check_FreeFormatDocument();
        }

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckScanner()
        {
            //System.Threading.Thread.CurrentThread.CurrentCulture = System.Globalization.CultureInfo.InvariantCulture;

            TestTokenTypes.CheckSeparators();
            TestTokenTypes.CheckComments();
            TestTokenTypes.CheckOperators();
            TestTokenTypes.CheckAlphanumericLiterals();
            TestTokenTypes.CheckPseudoText();
            TestTokenTypes.CheckNumericLiterals();
            TestTokenTypes.CheckKeywordsAndUserDefinedWords();
            TestTokenTypes.CheckPartialCobolWords();
            TestTokenTypes.CheckPictureCharacterString();
            TestTokenTypes.CheckCommentEntry();
            TestTokenTypes.CheckExecStatement();
            TestTokenTypes.CheckContextSensitiveKeywords();
            TestTokenTypes.CheckSymbolicCharacters();
            TestTokenTypes.CheckCblProcessCompilerDirective();

            TestContinuations.CheckSeparators();
            TestContinuations.CheckComments();
            TestContinuations.CheckOperators();
            TestContinuations.CheckAlphanumericLiterals();
            TestContinuations.CheckNumericLiterals();
            TestContinuations.CheckKeywordsAndUserDefinedWords();
            TestContinuations.CheckPictureCharacterString();
            TestContinuations.CheckCommentEntry();

            TestRealPrograms.CheckAllFilesForExceptions();
        }

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckPreprocessor()
        {
            TestCompilerDirectiveBuilder.CheckBASIS();
            TestCompilerDirectiveBuilder.CheckCBL_PROCESS();
            TestCompilerDirectiveBuilder.CheckASTERISK_CONTROL_CBL();
            TestCompilerDirectiveBuilder.CheckCOPY();
            TestCompilerDirectiveBuilder.CheckDELETE();
            TestCompilerDirectiveBuilder.CheckEJECT();
            TestCompilerDirectiveBuilder.CheckENTER();
            TestCompilerDirectiveBuilder.CheckEXEC_SQL_INCLUDE();
            TestCompilerDirectiveBuilder.CheckINSERT();
            TestCompilerDirectiveBuilder.CheckREADY_RESET_TRACE();
            TestCompilerDirectiveBuilder.CheckREPLACE();
            TestCompilerDirectiveBuilder.CheckSERVICE_LABEL();
            TestCompilerDirectiveBuilder.CheckSERVICE_RELOAD();
            TestCompilerDirectiveBuilder.CheckSKIP1_2_3();
            TestCompilerDirectiveBuilder.CheckTITLE();
            TestCompilerDirectiveBuilder.CheckRealFiles();

            TestCopyDirective.CheckCopy();
            TestCopyDirective.CheckCopyReplacing();

            TestReplaceDirective.CheckReplaceSingle();
            TestReplaceDirective.CheckReplacePartial();
            TestReplaceDirective.CheckReplaceSingleToMultiple();
            TestReplaceDirective.CheckReplaceMultiple();
            TestReplaceDirective.CheckReplaceNested();
            TestReplaceDirective.CheckReplaceFunction();
            TestReplaceDirective.CheckEmptyPartialWordReplace();
        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckParserCobol85()
        {
            TestTokenSource.Check_CobolCharStream();
            TestTokenSource.Check_CobolTokenSource();
            TestTokenSource.Check_CobolTokenSource_WithStartToken();

            // Test the recognition of potentially ambiguous CodeElements which begin with the same first Token
            TestCodeElements.Check("AT");
            TestCodeElements.Check("END");
            TestCodeElements.Check("NOT");
            TestCodeElements.Check("ON");
            // TO DO -> fix new identifier parsing :
            TestCodeElements.Check_DISPLAYCodeElements();
            TestCodeElements.Check_EXITCodeElements();
            TestCodeElements.Check_IDCodeElements();
            TestCodeElements.Check_UDWCodeElements();
            TestCodeElements.Check_WHENCodeElements();

            TestCodeElements.Check_HeaderCodeElements();
            TestCodeElements.Check_IdentificationCodeElements();
            TestCodeElements.Check_ParagraphCodeElements();
            TestCodeElements.Check_EntryCodeElements();
            TestParser.Check_BeforeAfterInsertion();
            TestParser.Check_BeforeAfterInsertionBatched();

			var errors = new System.Collections.Generic.List<Exception>();
			int nbOfTests = 0;
			foreach (string directory in Directory.GetDirectories(sampleRoot)) {
				var dirname = Path.GetFileName(directory);
				string[] extensions = {"*.cbl"};
				if (dirname.Equals("Programs")) extensions = new[] {"*.pgm", "*.cbl", "*.cpy" };
				Console.WriteLine("Entering directory \"" + dirname + "\" [" + string.Join(", ", extensions) + "]:");
				var folderTester = new FolderTester(sampleRoot, resultRoot, directory, extensions);
				try { folderTester.Test(); }
				catch (Exception ex) { errors.Add(ex); }
				nbOfTests += folderTester.GetTestCount();
				Console.WriteLine();
			}

            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");

			if (errors.Count > 0) {
				var str = new System.Text.StringBuilder();
				foreach(var ex in errors) str.Append(ex.Message);
				throw new Exception(str.ToString());
			}

            //This test use TypeChecker which is specific to TypeCobol
            //As specifications of TypeCobol are not final yet this test can't be used
            //            TestParser.Check_ParserIntegration();
        }

        /// <summary>
        /// Check only files with *.tcbl extensions
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckParserTcblPrograms()
        {
            int nbOfTests = 0;

            foreach (string directory in Directory.GetDirectories(sampleRoot))
            {
                var dirname = Path.GetFileName(directory);
                string[] extensions = { "*.tcbl" };
                if (dirname == "Programs") extensions = new[] { "*.tcbl", "*.cpy" };
                Console.WriteLine("Entering directory \"" + dirname + "\" [" + string.Join(", ", extensions) + "]:");
                var folderTester = new FolderTester(sampleRoot, resultRoot, directory, extensions);
                folderTester.Test();
                nbOfTests += folderTester.GetTestCount();
                Console.Write("\n");
            }
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }
    }
}
