﻿using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using System.Linq;

namespace TypeCobol.Test.Compiler.Parser
{
    internal static class TestCodeElements
    {
        internal static void Check(string testName, bool debug = false)
        {
            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName, null);
            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit.CodeElementsDocumentSnapshot);
            if (debug)
            {
                //Console.WriteLine(compilationUnit.CodeElementsDocumentSnapshot.CodeElements.Count+" CodeElements found:");
                foreach (var e in compilationUnit.CodeElementsDocumentSnapshot.CodeElements) Console.WriteLine("> "+e);
                Console.WriteLine("result:\n" + result);
            }
            ParserUtils.CheckWithResultFile(result, testName);
        }

        /// <summary>
        /// 
        /// </summary>
        public static void Check_DISPLAYCodeElements()
        {
			
            Tuple<CodeElementsDocument, DisplayStatement> tuple;

            //Test using the generic method which parse a single CodeElement
            tuple = ParseOneCodeElement<DisplayStatement>("display 'toto'");
            Assert.IsTrue(tuple.Item2.Variables.Length == 1);
            Assert.IsTrue(tuple.Item2.OutputDeviceName == null);
            Assert.IsFalse(tuple.Item2.IsWithNoAdvancing);

            tuple = ParseOneCodeElement<DisplayStatement>("display toto no advancing no advancing", false);
            Assert.IsTrue(tuple.Item2.Variables.Length == 1);
            Assert.IsTrue(tuple.Item2.OutputDeviceName == null);
            Assert.IsTrue(tuple.Item2.IsWithNoAdvancing);


            ParseDisplayStatement("display toto", 1);
            ParseDisplayStatement("display toto  'titi' tata", 3);
            ParseDisplayStatement("display toto 'titi' tata upon mnemo", 3, SymbolType.TO_BE_RESOLVED);
            ParseDisplayStatement("display toto 'titi' tata upon zeiruzrzioeruzoieruziosdfsdfsdfe ", 3, SymbolType.TO_BE_RESOLVED);
            ParseDisplayStatement("display toto 'titi' tata upon SYSIN", 3, SymbolType.TO_BE_RESOLVED);
            ParseDisplayStatement("display toto 'titi' tata upon C06", 3, SymbolType.TO_BE_RESOLVED);
            ParseDisplayStatement("display toto 'titi' tata upon SYSIN with no advancing", 3, SymbolType.TO_BE_RESOLVED, true);
            ParseDisplayStatement("display toto 'titi' tata upon C06  no advancing", 3, SymbolType.TO_BE_RESOLVED, true);
            ParseDisplayStatement("display toto 'titi' tata upon mnemo no advancing", 3, SymbolType.TO_BE_RESOLVED, true);
            ParseDisplayStatement("display toto 'titi' tata upon toto with no advancing", 3, SymbolType.TO_BE_RESOLVED, true);
            ParseDisplayStatement("display toto 'titi' tata no advancing", 3, null, true);
            ParseDisplayStatement("display toto 'titi' tata with no advancing", 3, null, true);

            ParseDisplayStatement("display ", 0, false);
            ParseDisplayStatement("display", 0, false);
            ParseDisplayStatement("display 'fsdf  \\ sdf'", 1);
            ParseDisplayStatement("display \"treortiertertert  zerzerzerze\" ", 1);
            ParseDisplayStatement("display 'treortiertertert  '' zerzerzerze' ", 1);
            ParseDisplayStatement("display 'treortiertertert  \" zerzerzerze' ", 1);
            ParseDisplayStatement("display 'treortiertertert  \"\" zerzerzerze' ", 1);
        }


        public static Tuple<CodeElementsDocument, DisplayStatement> ParseDisplayStatement(string textToParse,
            int nbrOfVarToDisplay)
        {
            return ParseDisplayStatement(textToParse, nbrOfVarToDisplay, null, false, true);
        }
        public static Tuple<CodeElementsDocument, DisplayStatement> ParseDisplayStatement(string textToParse,
            int nbrOfVarToDisplay, bool correctSyntax)
        {
            return ParseDisplayStatement(textToParse, nbrOfVarToDisplay, null, false, correctSyntax);
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
        public static Tuple<CodeElementsDocument, DisplayStatement> ParseDisplayStatement(string textToParse, int nbrOfVarToDisplay, SymbolType? uponMnemonicOrEnvName, bool isWithNoAdvancing = false, bool correctSyntax = true, params string[] varsToDisplay) 
        {
            Tuple<CodeElementsDocument, DisplayStatement> tuple = ParseOneCodeElement<DisplayStatement>(textToParse, correctSyntax);
            if (nbrOfVarToDisplay > 0)
            {
                Assert.IsTrue(tuple.Item2.Variables != null && tuple.Item2.Variables.Length == nbrOfVarToDisplay);
            }
            else
            {
                Assert.IsTrue(tuple.Item2.Variables == null || tuple.Item2.Variables.Length == 0);
            }
            if (uponMnemonicOrEnvName == null)
            {
                Assert.IsTrue(tuple.Item2.OutputDeviceName == null);
            }
            else
            {
                Assert.IsTrue(tuple.Item2.OutputDeviceName != null);
                if (tuple.Item2.OutputDeviceName != null)
                {
                    Assert.IsTrue(tuple.Item2.OutputDeviceName.Type == uponMnemonicOrEnvName);
                }
            }
            Assert.IsTrue(!tuple.Item2.IsWithNoAdvancing | isWithNoAdvancing);


            foreach (var varToDisp in varsToDisplay)
            {
             //TODO   
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
        public static Tuple<CodeElementsDocument, T> ParseOneCodeElement<T>(string textToParse, bool correctSyntax = true) where T : CodeElement
        {
            CompilationUnit compilationUnit = ParserUtils.ParseCobolString(textToParse);

            CodeElementsDocument codeElementsDocument = compilationUnit.CodeElementsDocumentSnapshot;
            Assert.IsTrue(codeElementsDocument.CodeElements.Any());
            var firstCodeElement = codeElementsDocument.CodeElements.First();
            Assert.IsTrue(firstCodeElement.GetType() == typeof(T));

            bool codeElementOk = firstCodeElement.Diagnostics == null || !firstCodeElement.Diagnostics.Any();
            bool codeElementDocumentOk = codeElementsDocument.ParserDiagnostics == null || !codeElementsDocument.ParserDiagnostics.Any();

            //Do not test compilationUnit.ProgramClassDocumentSnapshot.Diagnostics here because we are just parsing a single line
            //the semantic phase will produce errors.

            Assert.IsTrue((codeElementOk && codeElementDocumentOk) == correctSyntax);

            return new Tuple<CodeElementsDocument, T>(codeElementsDocument, (T) codeElementsDocument.CodeElements.First());
        }

        public static void Check_EXITCodeElements()
        {
            Check("EXITCodeElements");
        }

        public static void Check_IDCodeElements()
        {
            Check("IDCodeElements");
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
            //Check("EntryCodeElements");
        }
    }
}
