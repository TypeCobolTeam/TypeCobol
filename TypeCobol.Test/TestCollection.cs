﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using TypeCobol.Test.Compiler.File;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Compiler.Pipeline;
using TypeCobol.Test.Compiler.Preprocessor;
using TypeCobol.Test.Compiler.Scanner;
using TypeCobol.Test.Compiler.Text;

namespace TypeCobol.Test {

	[TestClass]
	public class TestCollection {

		[TestMethod]
		[TestProperty("Time","fast")]
		public void CheckFile() {
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
		[TestProperty("Time","fast")]
		public void CheckText() {
			TestReadOnlyTextDocument.Check_DocumentFormatExceptions();

			TestReadOnlyTextDocument.Check_EmptyDocument();
			TestReadOnlyTextDocument.Check_ReferenceFormatDocument();
			TestReadOnlyTextDocument.Check_FreeFormatDocument();
		}

		[TestMethod]
		[TestProperty("Time","fast")]
		public void CheckScanner() {
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
		[TestProperty("Time","fast")]
		public void CheckPreprocessor() {
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
		}

		[TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void CheckParser() {
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
//            TestCodeElements.Check_WHENCodeElements(); //TODO: these cannot be parsed alone anymore since today

			TestCodeElements.Check_HeaderCodeElements();
			TestCodeElements.Check_IdentificationCodeElements();
			TestCodeElements.Check_ParagraphCodeElements();
			//TODO  TestCodeElements.Check_EntryCodeElements();// RefreshProgramClassDocumentSnapshot
			TestParser.Check_BeforeAfterInsertion();
			TestParser.Check_BeforeAfterInsertionBatched();

			string root = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar + "Samples";
			foreach (string directory in Directory.GetDirectories(PlatformUtils.GetPathForProjectFile(root)))
			{
				var dirname = Path.GetFileName(directory);
				string[] extensions = new string[] { "*.cbl", };
				if (dirname == "Programs") extensions = new string[] { "*.pgm", "*.cpy", };
				System.Console.WriteLine("Entering directory \""+dirname+"\" ["+string.Join(", ", extensions)+"]:");
				new FolderTester(dirname, extensions).Test();
				System.Console.Write("\n");
			}

			//This test use TypeChecker which is specific to TypeCobol
			//As specifications of TypeCobol are not final yet this test can't be used
			//            TestParser.Check_ParserIntegration();
		}
	}
}
