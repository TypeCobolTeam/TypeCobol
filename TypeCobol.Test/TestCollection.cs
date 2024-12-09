using System;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Parser;
using TypeCobol.Test.Parser.FileFormat;
using TypeCobol.Test.Parser.Preprocessor;
using TypeCobol.Test.Parser.Scanner;
using TypeCobol.Test.Parser.Text;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test {

    [TestClass]
    public class TestCollection
    {
        static readonly string root = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "Programs");

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckFileFormat()
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
            TestTokenTypes.CheckPseudoText2();
            TestTokenTypes.CheckNumericLiterals();
            TestTokenTypes.CheckKeywordsAndUserDefinedWords();
            TestTokenTypes.CheckPartialCobolWords(true);//For Cobol
            TestTokenTypes.CheckPartialCobolWords(false);//For TypeCobol
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
            TestContinuations.CheckContinuationsMixedWithDebug();

            TestRealPrograms.CheckAllFilesForExceptions();
        }

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckSqlScanner()
        {
            TestSqlScanner.CheckSqlConstants();
            TestSqlScanner.CheckSqlConstants2();
        }

        [TestMethod]
        [TestProperty("Time", "fast")]
        public void CheckPreprocessor()
        {
            TestCompilerDirectiveBuilder.CheckBASIS();
            TestCompilerDirectiveBuilder.CheckCBL_PROCESS();
            TestCompilerDirectiveBuilder.CheckASTERISK_CONTROL_CBL();
#if EUROINFO_RULES
            TestCompilerDirectiveBuilder.CheckCOPY(); //Because of the presence of remarks directive and non use of comprarator
#endif
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
            TestReplaceDirective.CheckReplacePartial(true);
            TestReplaceDirective.CheckReplacePartial(false);
            TestReplaceDirective.CheckReplaceSingleToMultiple();
            TestReplaceDirective.CheckReplaceMultiple();
            TestReplaceDirective.CheckReplaceFunction();
            TestReplaceDirective.CheckEmptyPartialWordReplace();
            TestReplaceDirective.CheckEmptyPartialWordReplace2();
        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckParserRobustness()
        {
            TestParserRobustness.CheckProgramCodeElements();
        }


        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckCodeElements() {
            // Test the recognition of potentially ambiguous CodeElements which begin with the same first Token
            TestCodeElements.Check_DISPLAYCodeElements();


            TestCodeElements.CheckCodeElements();
        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckToken() {
            TestTokenSource.Check_CobolCharStream();
            TestTokenSource.Check_CobolTokenSource();
            TestTokenSource.Check_CobolTokenSource_WithStartToken();
        }

        /// <summary>
        /// Check Parser on Cobol85 source code
        /// </summary>
        /// <param name="cobol">true if it is really for pure Cobol85 language, rather than TypeCobol, false otherwise</param>
        private void CheckParserCobol85(bool cobol)
        {
            string[] extensions = { ".cbl", ".pgm" };
            Console.WriteLine("Entering directory \"" + root + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(root, extensions) { IsCobolLanguage = cobol };
            int nbOfTests = folderTester.Test();
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckParserCobol85()
        {
            CheckParserCobol85(true);
            CheckParserCobol85(false);
        }

        /// <summary>
        /// Check only files with *.tcbl extensions
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckParserTcblPrograms()
        {
            string[] extensions = { ".tcbl" };
            Console.WriteLine("Entering directory \"" + root + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(root, extensions);
            int nbOfTests = folderTester.Test();
            Console.Write("\n");
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }

        /// <summary>
        /// Fails when auto-replace is left enabled to avoid committing auto-validating tests.
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void TCBLAutoReplaceSecurityTest()
        {
            TestUtils.CompareLines(string.Empty, string.Empty, string.Empty, string.Empty);
        }


#if EUROINFO_RULES
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void EILegacyCheck()
        {
            string tempRoot = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "EILegacy");

            string[] extensions = {".tcbl", ".cbl"};

            //Do not parse unsupported remarks, they are covered in a separate test
            var folderTester = new FolderTester(tempRoot, extensions, recursive: false);
            int nbOfTests = folderTester.Test();
            Console.Write("\n");

            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void EILegacyUnsupportedRemarksCheck()
        {
            string tempRoot = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "EILegacy" + Path.DirectorySeparatorChar + "UnsupportedRemarks");

            string[] extensions = { ".cbl" };

            //In pure cobol mode, REMARKS directive is considered as regular comment so unsupported REMARKS formats won't break the parsing
            var folderTester = new FolderTester(tempRoot, extensions, recursive: false) { IsCobolLanguage = true };
            int nbOfTests = folderTester.Test();
            Console.Write("\n");

            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }
#endif

        /// <summary>
        /// Check for ExecToStep
        /// </summary>
        [TestMethod]
        //[Ignore]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckExecToStep()
        {
            string fileName = "ExecToStepFile.rdz.cbl";

            var folder = "Parser" + Path.DirectorySeparatorChar + "ExecToStep";

            var compileResult = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.Scanner);
            //Verify that the option hasn't change during processing and that compilation process didn't go further than defined step
            if (compileResult.CompilerOptions.ExecToStep != ExecutionStep.Scanner 
                && compileResult.TokensLines.Count == 0 
                && compileResult.ProcessedTokensDocumentSnapshot != null 
                && compileResult.CodeElementsDocumentSnapshot != null 
                && compileResult.ProgramClassDocumentSnapshot.Root.Programs.Any())
                throw new Exception("Scanner Step failed");

            compileResult = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.Preprocessor);
            if (compileResult.CompilerOptions.ExecToStep != ExecutionStep.Preprocessor 
                && compileResult.ProcessedTokensDocumentSnapshot == null 
                && compileResult.CodeElementsDocumentSnapshot != null 
                && compileResult.ProgramClassDocumentSnapshot.Root.Programs.Any())
                throw new Exception("Preprocessor Step failed");

            compileResult = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.CodeElement);
            if (compileResult.CompilerOptions.ExecToStep != ExecutionStep.CodeElement
                && compileResult.CodeElementsDocumentSnapshot == null 
                && compileResult.ProgramClassDocumentSnapshot.Root.Programs.Any())
                throw new Exception("CodeElement Step failed");

            compileResult = ParserUtils.ParseCobolFile(fileName, folder, execToStep: ExecutionStep.AST);
            if (compileResult.CompilerOptions.ExecToStep != ExecutionStep.AST
                && !compileResult.ProgramClassDocumentSnapshot.Root.Programs.Any())
                throw new Exception("AST Step failed");
        }


        /// <summary>
        /// Check for Documentation generation
        /// </summary>
        [TestMethod]
        //[Ignore]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckDocumentation()
        {
            string[] extensions = { ".tcbl" };
            var directory = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "Documentation");
            var folderTester = new FolderTester(directory, extensions);
            int nbOfTests = folderTester.Test();
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }


        /// <summary>
        /// Direct copy parsing tests. Checks all ".cpy" files from "Parser\Copies".
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckCopies()
        {
            string[] extensions = { ".cpy" };
            var directory = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "Copies");
            Console.WriteLine("Entering directory \"" + directory + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(directory, extensions);
            int nbOfTests = folderTester.Test();
            Console.Write("\n");
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }

        /// <summary>
        /// Specific tests for parser limitations. Tests can be separated between TC and pure cobol using 'NoTC' suffix in directory name.
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckParserLimitations()
        {
            int nbOfTests = 0;

            string[] extensions = { ".cbl", ".tcbl" };
            var directory = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "Limitations");

            //Tests results may differ between TypeCobol compatibility mode and Cobol strict mode,
            //so use a dedicated non-recursive FolderTester for each folder instead of a global one.
            foreach (var testDirectory in Directory.GetDirectories(directory))
            {
                var dirname = Path.GetFileName(testDirectory);
                Console.WriteLine("Entering directory \"" + dirname + "\" [" + string.Join(", ", extensions) + "]:");
                var folderTester = new FolderTester(testDirectory, extensions, recursive: false) { IsCobolLanguage = dirname.EndsWith("NoTC") };
                nbOfTests += folderTester.Test();
            }

            Console.Write("\n");
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
        }
    }
}
