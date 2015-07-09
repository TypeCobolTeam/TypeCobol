using System;
using System.IO;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.CodeElements;

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

        public static void Check_EntryCodeElements()
        {
            Check("EntryCodeElements");
        }

        public static void Check_StatementCodeElements()
        {
            Check_ADDCodeElements(); 
        }

        public static void Check_ADDCodeElements() {
            string path = "Statements" + Path.DirectorySeparatorChar + "ADDCodeElements";
            DocumentFormat format = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
            CompilationUnit unit = ParserUtils.ParseCobolFile(path, format);
            ArithmeticStatementTester tester = new ArithmeticStatementTester();
            string[] rpn = { 
                // format 1
                "x = a x +",
                "toto = titi tata + toto +",
                "x = a b + ab + x +, toto = a b + ab + toto +",
                "x = a x +",
                "x = 1 x +",
                "toto = 1 2 + toto +",
                "x = 1 2 + 3 + x +, toto = 1 2 + 3 + toto +",
                "x = 1 0 + 1 + 0 + 1 + 0 + x +",
                "", "", "", "", "", // literals not allowed as 2nd operand
                //format 2
                "x = a m +",
                "toto = titi tata +",
                "x = a b + ab + m +, toto = a b + ab + m +",
                "x = 1 m +",
                "x = 1 m +, y = 1 m +",
                "x = a 1 + 2 +",
                "x = a b + ab + 1 +, toto = a b + ab + 1 +",
                "x = 0 0 +",
                "x = 1 0 + 1 + 0 + 1 + 0 +",
                // format 3
                "x = a x +",
                "x = a x +",
            };
            tester.CompareWithRPN(unit.SyntaxDocument, rpn);
            //Check("StatementCodeElements", new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat));
        }
    }
}
