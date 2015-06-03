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

        public static void Check_IDCodeElements()
        {
            string testName = "IDCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_NOTCodeElements()
        {
            string testName = "NOTCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_ONCodeElements()
        {
            string testName = "ONCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_PERFORMCodeElements()
        {
            string testName = "PERFORMCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_UDWCodeElements()
        {
            string testName = "UDWCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_WHENCodeElements()
        {
            string testName = "WHENCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_XMLCodeElements()
        {
            string testName = "XMLCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }
    }
}
