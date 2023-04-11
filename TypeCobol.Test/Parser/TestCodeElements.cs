using System;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser
{
    internal static class TestCodeElements
    {
        static readonly string _Root = PlatformUtils.GetPathForProjectFile("Parser" + Path.DirectorySeparatorChar + "CodeElements");

        public static void CheckCodeElements()
        {
            string[] extensions = { ".cbl", ".pgm" };
            Console.WriteLine("Entering directory \"" + _Root + "\" [" + string.Join(", ", extensions) + "]:");
            var folderTester = new FolderTester(_Root, extensions);
            int nbOfTests = folderTester.Test();
            Console.Write("Number of tests: " + nbOfTests + "\n");
            Assert.IsTrue(nbOfTests > 0, "No tests found");
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

            tuple = ParseOneCodeElement<DisplayStatement>("display toto no advancing no advancing", correctSyntax: false);
            Assert.IsTrue(tuple.Item2.Variables.Length == 1);
            Assert.IsTrue(tuple.Item2.OutputDeviceName == null);
            Assert.IsTrue(tuple.Item2.IsWithNoAdvancing);

            tuple = ParseOneCodeElement<DisplayStatement>("display 'a' all 'b' 'c' upon SYSIN with no advancing", correctSyntax: true);
            Assert.IsTrue(tuple.Item2.Variables.Length == 3);
            Assert.IsTrue(tuple.Item2.OutputDeviceName != null);
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

            ParseDisplayStatement("display all", 1, false);
            ParseDisplayStatement("display all ", 1, false);
            ParseDisplayStatement("display all 9.1", 2, false);
            ParseDisplayStatement("display all Var2", 2, false);
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
            Tuple<CodeElementsDocument, DisplayStatement> tuple = ParseOneCodeElement<DisplayStatement>(textToParse, false, correctSyntax);
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
        /// <param name="textToParse"></param>
        /// <param name="correctSyntax"></param>
        /// <param name="asPartOfACopy"></param>
        public static Tuple<CodeElementsDocument, T> ParseOneCodeElement<T>(string textToParse, bool asPartOfACopy = false, bool correctSyntax = true) where T : CodeElement
        {
            CompilationUnit compilationUnit = ParserUtils.ParseCobolString(textToParse, asPartOfACopy);

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
        
       
    }
}
