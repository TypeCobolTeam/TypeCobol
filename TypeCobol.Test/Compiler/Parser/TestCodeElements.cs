using System;
using TypeCobol.Compiler;

namespace TypeCobol.Test.Compiler.Parser
{
    static class TestCodeElements
    {
        // --- Tests the recognition of potentially ambiguous CodeElements which begin with the same first Token --- 

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

        // --- Tests the correct parsing of all CodeElements ---

        public static void Check_HeaderCodeElements()
        {
            string testName = "HeaderCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }

        public static void Check_IdentificationCodeElements()
        {
            string testName = "IdentificationCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
        }
    }
}
