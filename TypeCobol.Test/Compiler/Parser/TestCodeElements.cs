using System;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Parser
{
    static class TestCodeElements
    {
        private static void Check(string testName, DocumentFormat format = null)
        {
            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName, format);
            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
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

        public static void Check_DISPLAYCodeElements()
        {
            string testName = "DISPLAYCodeElements";

            // Compile test file
            CompilationUnit compilationUnit = ParserUtils.ParseCobolFile(testName);

            // Check code elements
            string result = ParserUtils.DumpCodeElements(compilationUnit);
            ParserUtils.CheckWithResultFile(result, testName);
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

        public static void Check_StatementCodeElements()
        {
            Check("StatementCodeElements", new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat));
        }

        public static void Check_EntryCodeElements()
        {
            Check("EntryCodeElements");
        }
    }
}
