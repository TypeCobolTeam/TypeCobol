using System;
using System.Linq;
using TypeCobol.Compiler;

namespace TypeCobol.Test.Compiler.Parser
{
    static class TestParser
    {
        public static void Check_ParserIntegration()
        {
            string textName = "TESTPGM1";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(textName);
            
            // The code below enables rich diagnostic if necessary
            //TestErrorListener errorListener = new TestErrorListener();
            //parser.AddErrorListener(errorListener);
            //errorListener.ErrorLog  

            // Check for parsing errors
            if(compilationUnit.CodeElementsDocumentSnapshot.ParserDiagnostics.Count() > 0)
            {            
                throw new Exception(compilationUnit.CodeElementsDocumentSnapshot.ParserDiagnostics.Count() + " errors found while parsing " + textName);
            }
            // Check for semantic analysis errors
            if (compilationUnit.CodeElementsDocumentSnapshot.ParserDiagnostics.Count() != 1 || compilationUnit.CodeElementsDocumentSnapshot.ParserDiagnostics.First().Message != "Data item W-TAB-LCC-CLE-DAT of type date can not be moved into data item W-TAB-LCC-CLE-RIB of type rib")
            {
                throw new Exception("Parser and type checker with TypeCobol extension KO");
            }
        }
    }
}
