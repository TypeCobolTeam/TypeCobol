using System;
using TypeCobol.Compiler;

namespace TypeCobol.Test.Compiler.Parser
{
    static class TestCodeElements
    {
        public static void Check_ATCodeElements()
        {
            string testName = "ATCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_ENDCodeElements()
        {
            string testName = "ENDCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_EXITCodeElements()
        {
            string testName = "EXITCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }
    }
}
