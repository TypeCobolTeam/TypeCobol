using System;
using System.IO;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.CodeElements;
using System.Collections.Generic;

namespace TypeCobol.Test.Compiler.Parser
{
    internal static class TestCodeElements
    {
        private static void Check(string testName, bool debug = false)
        {
            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName, null);
            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            if (debug)
            {
                Console.WriteLine(compilationUnit.SyntaxDocument.CodeElements.Count+" CodeElements found:");
                foreach (var e in compilationUnit.SyntaxDocument.CodeElements) Console.WriteLine("> "+e);
                Console.WriteLine("result:\n" + result);
            }
            ParserUtils.CheckWithResultFile(result, testName);
        }

        // --- Tests the recognition of potentially ambiguous CodeElements which begin with the same first Token --- 

        public static void Check_ATCodeElements()
        {
            Check("ATCodeElements");
        }

        public static void Check_ENDCodeElements()
        {
            Check("ENDCodeElements");
        }


        /// <summary>
        /// 
        /// </summary>
        public static void Check_DISPLAYCodeElements()
        {
            CompilationUnit compilationUnit = ParserUtils.CreateCompilationUnitForVirtualFile();
            
            Tuple<SyntaxDocument, DisplayStatement> tuple;

            //Test using the generic method which parse a single CodeElement
            tuple = ParseOneCodeElement<DisplayStatement>(compilationUnit, "display 'toto'");
            Assert.IsTrue(tuple.Item2.VarsToDisplay.Count == 1);
            Assert.IsTrue(tuple.Item2.UponMnemonicOrEnvironmentName == null);
            Assert.IsTrue(tuple.Item2.IsWithNoAdvancing.Value == false);

            tuple = ParseOneCodeElement<DisplayStatement>(compilationUnit, "display toto no advancing no advancing", false);
            Assert.IsTrue(tuple.Item2.VarsToDisplay.Count == 1);
            Assert.IsTrue(tuple.Item2.UponMnemonicOrEnvironmentName == null);
            Assert.IsTrue(tuple.Item2.IsWithNoAdvancing.Value);


            ParseDisplayStatement(compilationUnit, "display toto", 1);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata", 3);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon mnemo", 3, SymbolType.MnemonicForEnvironmentName);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon zeiruzrzioeruzoieruziosdfsdfsdfe ", 3, SymbolType.MnemonicForEnvironmentName);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon SYSIN", 3, SymbolType.EnvironmentName);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon C06", 3, SymbolType.EnvironmentName);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon SYSIN with no advancing", 3, SymbolType.EnvironmentName, true);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon C06  no advancing", 3, SymbolType.EnvironmentName, true);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon mnemo no advancing", 3, SymbolType.MnemonicForEnvironmentName, true);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata upon toto with no advancing", 3, SymbolType.MnemonicForEnvironmentName, true);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata no advancing", 3, null, true);
            ParseDisplayStatement(compilationUnit, "display toto 'titi' tata with no advancing", 3, null, true);

            ParseDisplayStatement(compilationUnit, "display ", 0, false);
            ParseDisplayStatement(compilationUnit, "display", 0, false);
            ParseDisplayStatement(compilationUnit, "display 'fsdf  \\ sdf'", 1);
            ParseDisplayStatement(compilationUnit, "display \"treortiertertert  zerzerzerze\" ", 1);
            ParseDisplayStatement(compilationUnit, "display 'treortiertertert  '' zerzerzerze' ", 1);
            ParseDisplayStatement(compilationUnit, "display 'treortiertertert  \" zerzerzerze' ", 1);
            ParseDisplayStatement(compilationUnit, "display 'treortiertertert  \"\" zerzerzerze' ", 1);

            Check("Statements" + Path.DirectorySeparatorChar + "DISPLAYCodeElements");
        }


        public static Tuple<SyntaxDocument, DisplayStatement> ParseDisplayStatement(CompilationUnit compilationUnit, string textToParse,
            int nbrOfVarToDisplay)
        {
            return ParseDisplayStatement(compilationUnit, textToParse, nbrOfVarToDisplay, null, false, true);
        }
        public static Tuple<SyntaxDocument, DisplayStatement> ParseDisplayStatement(CompilationUnit compilationUnit, string textToParse,
            int nbrOfVarToDisplay, bool correctSyntax)
        {
            return ParseDisplayStatement(compilationUnit, textToParse, nbrOfVarToDisplay, null, false, correctSyntax);
        }


        /// <summary>
        /// Parse a display statement and test if:
        /// - the number of identifier or literals parse is equals to the number specified in #nbrOfVarToDisplay
        /// - upon mnemonic or environment name is
        /// - with no advancing
        /// </summary>
        /// <param name="compilationUnit"></param>
        /// <param name="textToParse"></param>
        /// <param name="nbrOfVarToDisplay"></param>
        /// <param name="uponMnemonicOrEnvName"></param>
        /// <param name="isWithNoAdvancing"></param>
        /// <param name="correctSyntax"></param>
        /// <param name="varsToDisplay"></param>
        /// <returns></returns>
        public static Tuple<SyntaxDocument, DisplayStatement> ParseDisplayStatement(CompilationUnit compilationUnit, string textToParse, int nbrOfVarToDisplay, SymbolType? uponMnemonicOrEnvName, bool isWithNoAdvancing = false, bool correctSyntax = true, params string[] varsToDisplay) 
        {
            Tuple<SyntaxDocument, DisplayStatement> tuple = ParseOneCodeElement<DisplayStatement>(compilationUnit, textToParse, correctSyntax);
            Assert.IsTrue(tuple.Item2.VarsToDisplay.Count == nbrOfVarToDisplay);
            if (uponMnemonicOrEnvName == null)
            {
                Assert.IsTrue(tuple.Item2.UponMnemonicOrEnvironmentName == null);
            }
            else
            {
                Assert.IsTrue(tuple.Item2.UponMnemonicOrEnvironmentName != null);
                if (tuple.Item2.UponMnemonicOrEnvironmentName != null)
                {
                    Assert.IsTrue(tuple.Item2.UponMnemonicOrEnvironmentName.Type == uponMnemonicOrEnvName);
                }
            }
            Assert.IsTrue(!tuple.Item2.IsWithNoAdvancing.Value | isWithNoAdvancing);


            foreach (var varToDisp in varsToDisplay)
            {
                
            }

            return tuple;
        }


        /// <summary>
        /// Parse a text that match exactly one code element.
        /// The type of the code element is compared to the parsed one.
        /// </summary>
        /// <param name="compilationUnit"></param>
        /// <param name="textToParse"></param>
        /// <param name="correctSyntax"></param>
        public static Tuple<SyntaxDocument, T> ParseOneCodeElement<T>(CompilationUnit compilationUnit, string textToParse, bool correctSyntax = true) where T : CodeElement
        {
//            compilationUnit.SyntaxDocument.Diagnostics.Clear();
//            compilationUnit.SyntaxDocument.CodeElements.Clear();
            compilationUnit.TextDocument.LoadChars(textToParse);

            SyntaxDocument syntaxDocument = compilationUnit.SyntaxDocument;
            Assert.IsTrue(syntaxDocument.CodeElements.Count == 1);
            Assert.IsTrue(syntaxDocument.CodeElements[0].GetType() == typeof(T));

            if (correctSyntax)
            {
                Assert.IsTrue(syntaxDocument.Diagnostics.Count == 0);
            }
            else
            {
                Assert.IsFalse(syntaxDocument.Diagnostics.Count == 0);
            }

            return new Tuple<SyntaxDocument, T>(syntaxDocument, (T) syntaxDocument.CodeElements[0]);
        }

        public static void Check_EXITCodeElements()
        {
            Check("EXITCodeElements");
        }

        public static void Check_IDCodeElements()
        {
            Check("IDCodeElements");
        }

        public static void Check_NOTCodeElements()
        {
            Check("NOTCodeElements");
        }

        public static void Check_ONCodeElements()
        {
            Check("ONCodeElements");
        }

        public static void Check_PERFORMCodeElements()
        {
            Check("PERFORMCodeElements");
        }

        public static void Check_UDWCodeElements()
        {
            Check("UDWCodeElements");
        }

        [TestMethod]
        public static void Check_WHENCodeElements()
        {
            Check("WHENCodeElements");
        }

        public static void Check_XMLCodeElements()
        {
            Check("XMLCodeElements");
        }

        // --- Tests the correct parsing of all CodeElements ---

        public static void Check_HeaderCodeElements()
        {
            Check("HeaderCodeElements");
        }

        public static void Check_IdentificationCodeElements()
        {
            Check("IdentificationCodeElements");
        }

        public static void Check_ParagraphCodeElements()
        {
            Check("ParagraphCodeElements");
        }

        public static void Check_EntryCodeElements()
        {
            Check("EntryCodeElements");
        }

        public static void Check_Expressions()
        {
            TestDirectory("Expressions");
        }

        private static void TestDirectory(string subdir = null, bool debug = false)
        {
            string root = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar;
            if (subdir == null) subdir = "";
            if (subdir.Length > 0) subdir = Path.DirectorySeparatorChar + subdir;
            string samples = PlatformUtils.GetPathForProjectFile(root + "Samples" + subdir);
            string[] paths = Directory.GetFiles(samples, "*.cbl", SearchOption.AllDirectories);
            for (int c = 0; c < paths.Length; c++)
            {
                System.Console.WriteLine("sample=" + paths[c]);
                string filename = Path.GetFileNameWithoutExtension(paths[c]);
                string expr = subdir + Path.DirectorySeparatorChar + filename;
                string path = PlatformUtils.GetPathForProjectFile(root + "ResultFiles" + expr + ".txt");
                if (!System.IO.File.Exists(path)) continue;
                TestFile("Samples" + expr + ".cbl", "ResultFiles" + expr + ".txt", root, debug);
            }
        }

        private static void TestFile(string sample, string result, string root, bool debug = false)
        {
            System.Console.WriteLine("Test \""+sample+"\" vs \""+result+"\"");
            CompilationUnit unit = ParseUTF8File(sample, root);
            using (System.IO.StreamReader reader = new System.IO.StreamReader(PlatformUtils.GetStreamForProjectFile(root + result)))
            {
                TestLineByLine(unit.SyntaxDocument.CodeElements, reader);
            }
        }

        private static void TestLineByLine(IList<CodeElement> elements, StreamReader reader)
        {
            foreach (var e in elements)
            {
                if (e.GetType() == typeof(SentenceEnd)) continue;
                string line = reader.ReadLine();
                TestLine(e, line);
            }
        }

        private static void TestLine(CodeElement e, string line)
        {
            Console.WriteLine("TODO TestLine( "+e+" , \""+line+"\")" );
        }

        private static CompilationUnit ParseUTF8File(string path, string root)
        {
            DocumentFormat format = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
            DirectoryInfo localDirectory = new DirectoryInfo(Path.GetDirectoryName(PlatformUtils.GetPathForProjectFile(root + path)));
            TypeCobolOptions options = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
            string filename = Path.GetFileName(path);
            CompilationUnit unit = new CompilationUnit(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, new TypeCobolOptions());
            unit.SetupCodeAnalysisPipeline(null, 0);
            unit.StartDocumentProcessing();
            return unit;
        }



        public static void Check_StatementCodeElements()
        {
            // decision
            CheckStatement("IFCodeElements");
            CheckStatement("EVALUATECodeElements");
            // arithmetic
            CheckArithmeticStatement("ADDCodeElements", "ADDRPN.txt");
            CheckArithmeticStatement("SUBTRACTCodeElements", "SUBTRACTRPN.txt");
            // procedure branching
            CheckStatement("PERFORMCodeElements");
        }

        public static CompilationUnit CheckStatement(string filename, bool debug = false)
        {
            return CheckUTF8("Statements" + Path.DirectorySeparatorChar + filename, debug);
        }

        public static CompilationUnit CheckUTF8(string path, bool debug = false)
        {
            DocumentFormat format = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
            CompilationUnit unit = ParserUtils.ParseCobolFile(path, format);
            string result = ParserUtils.DumpCodeElements(unit);
            if (debug) Console.WriteLine("\"" + path + "\" result:\n" + result);
            ParserUtils.CheckWithResultFile(result, path);
            return unit;
        }

        public static void CheckArithmeticStatement(string filename, string rpnfilename) {
            CompilationUnit unit = CheckStatement(filename);
            string path = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar + "ResultFiles" + Path.DirectorySeparatorChar + "Statements";
            new ArithmeticStatementTester().CompareWithRPNFile(unit.SyntaxDocument, path + Path.DirectorySeparatorChar + rpnfilename);
        }
    }
}
