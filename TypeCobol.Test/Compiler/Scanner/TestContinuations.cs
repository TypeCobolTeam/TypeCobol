using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Scanner
{
    static class TestContinuations
    {
        public static void CheckSeparators()
        {
            string testName = "Separators-continuations";
            TextLine[] textLines = new TestTextLine[] {
                // .     , ; ():.
                new TestTextLine("."),
                new TestTextLine('-',"    "),
                new TestTextLine("."),
                new TestTextLine('-'," a"),
                new TestTextLine("."),
                new TestTextLine('-',"    a"),
                new TestTextLine(","),
                new TestTextLine('-',"    a"),
                new TestTextLine(";"),
                new TestTextLine('-',"    a"),
                new TestTextLine("("),
                new TestTextLine('-',"    a"),
                new TestTextLine(")"),
                new TestTextLine('-',"    a"),
                new TestTextLine(":"),
                new TestTextLine('-',"    a"),
                new TestTextLine("(."),
                new TestTextLine('-',"025 * ."),
                new TestTextLine('-',"44"),
                new TestTextLine('-',") + 1"),
                new TestTextLine('-',".1"),
                new TestTextLine('-',"8.")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckComments()
        {
            string testName = "Comments-continuations";
            TextLine[] textLines = new TestTextLine[] {
                // Comment lines
                // Floating comments
                // Blank lines
                new TestTextLine("1"),
                new TestTextLine('*', "This is a comment line ! ***"),
                new TestTextLine('-',"    .234*> and a floating comment"),
                new TestTextLine('-',"    56"),
                new TestTextLine('-',"      "),
                new TestTextLine('-',"    78*> and another floating comment"),
                new TestTextLine('*', "This is another comment line ! ***"),
                new TestTextLine(".")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckOperators()
        {
            string testName = "Operators-continuations";
            TextLine[] textLines = new TestTextLine[] {
                // + - * / **
                // < > <= >= = 
                new TestTextLine("1 +"),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 + "),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 -"),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 *"),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 /"),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 *"),
                new TestTextLine('-',"    * 2"),
                new TestTextLine("1 <"),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 >"),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 <"),
                new TestTextLine('-',"    = 2"),
                new TestTextLine("1 >"),
                new TestTextLine('-',"    = 2"),
                new TestTextLine("1 ="),
                new TestTextLine('-',"    2"),
                new TestTextLine("1 ="),
                new TestTextLine('-',"    = 2")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckAlphanumericLiterals()
        {
            string testName = "AlphanumericLiterals-continuations";
            TextLine[] textLines = new TestTextLine[] {
                // " N" G" Z" X" NX" 
                new TestTextLine("\"toto  "),
                new TestTextLine('-',"    \"and titi1\"."),    
                new TestTextLine("\"toto  "),
                new TestTextLine('-',"\"and titi2\"."), 
                new TestTextLine("\"toto  \""),
                new TestTextLine('-',"    \"and titi3\"."),  
                new TestTextLine("\"toto  \" "),
                new TestTextLine('-',"    \"and titi3b\"."),    
                new TestTextLine("\"toto  "),
                new TestTextLine('-',"    and titi4\"."), 
                new TestTextLine("\"toto  "),
                new TestTextLine('-',"    'and titi5\"."),
                new TestTextLine("\"toto  \""),
                new TestTextLine('-',"    \"\"and titi6\"."),
                new TestTextLine("\"toto  \""),
                new TestTextLine('-',"    \"\"and titi7  "),
                new TestTextLine('-',"    \"and titi8\"."),
                new TestTextLine("'toto  "),
                new TestTextLine('-',"    'and titi9'"),  
                new TestTextLine("'toto'  "),
                new TestTextLine('-',"    'and titi10'"),
                new TestTextLine("'toto'"),
                new TestTextLine('-',"    'and titi11' "),
                new TestTextLine("'toto'"),
                new TestTextLine('-',"    ''and titi11b' "),
                new TestTextLine("'toto'"),
                new TestTextLine('-',"    \"and titi11c' "),
                new TestTextLine("'toto  "),
                new TestTextLine('-',"    \"and titi11d' "),
                new TestTextLine(" N\"toto  "),
                new TestTextLine('-',"    \"and titi12\""),
                new TestTextLine("G\"toto  "),
                new TestTextLine('-',"    \"and titi13\""),
                new TestTextLine("Z\"toto  "),
                new TestTextLine('-',"    \"and titi14\""),
                new TestTextLine("X\"ABCD12"),
                new TestTextLine('-',"    \"345678\""),
                new TestTextLine("NX\"ABCD12"),
                new TestTextLine('-',"    \"345678\""),
                new TestTextLine("== toto an"),
                new TestTextLine('-',"    d ti"),
                new TestTextLine('-',"    ti15 =="),
                new TestTextLine('-',"== super ="),
                new TestTextLine('-',"= ="),
                new TestTextLine('-',"=cool="),
                new TestTextLine('-',"=."),
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckNumericLiterals()
        {
            string testName = "NumericLiterals-continuations";
            TextLine[] textLines = new TestTextLine[] {
                new TestTextLine("1"),
                new TestTextLine('*', "This is a comment line ! ***"),
                new TestTextLine('-',"    23456"),
                new TestTextLine("12345"),
                new TestTextLine("    "),
                new TestTextLine('-',"    6"),
                new TestTextLine("+"),
                new TestTextLine('-',"123"),
                new TestTextLine(" -1"),
                new TestTextLine('-',"    23"),
                new TestTextLine("."),
                new TestTextLine('-',"    23"),
                new TestTextLine("1"),
                new TestTextLine('-',"    .23"),
                new TestTextLine("1."),
                new TestTextLine('-',"    23"),
                new TestTextLine("1.2"),
                new TestTextLine('-',"    3"),
                new TestTextLine("1. "),
                new TestTextLine('-',"    23"),
                new TestTextLine(" -1.23"),
                new TestTextLine('-',"    1E+"),
                new TestTextLine('-',"    20."),
                new TestTextLine("1.23"),
                new TestTextLine('-',"    1E"),
                new TestTextLine('-',"    +20.")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "NumericLiteralsComma-continuations";
            textLines = new TestTextLine[] {
                new TestTextLine("DECIMAL-POINT IS COMMA"),
                new TestTextLine("1"),
                new TestTextLine('*', "This is a comment line ! ***"),
                new TestTextLine('-',"    23456"),
                new TestTextLine("12345"),
                new TestTextLine("    "),
                new TestTextLine('-',"    6"),
                new TestTextLine("+"),
                new TestTextLine('-',"123"),
                new TestTextLine(" -1"),
                new TestTextLine('-',"    23"),
                new TestTextLine(","),
                new TestTextLine('-',"    23"),
                new TestTextLine("1"),
                new TestTextLine('-',"    ,23"),
                new TestTextLine("1,"),
                new TestTextLine('-',"    23"),
                new TestTextLine("1,2"),
                new TestTextLine('-',"    3"),
                new TestTextLine("1, "),
                new TestTextLine('-',"    23"),
                new TestTextLine(" -1,23"),
                new TestTextLine('-',"    1E+"),
                new TestTextLine('-',"    20,"),
                new TestTextLine("1,23"),
                new TestTextLine('-',"    1E"),
                new TestTextLine('-',"    +20,")
            };
            result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);             
        }

        public static void CheckKeywordsAndUserDefinedWords()
        {
            string testName = "Keywords-continuations";
            TextLine[] textLines = new TestTextLine[] {
                new TestTextLine("REPLACE *"),
                new TestTextLine('-',"    CONTROL BASIS COP"),
                new TestTextLine('-',"    Y ALT"),
                new TestTextLine('-',"    ER"),
                new TestTextLine("end-"),
                new TestTextLine('-',"    EXEC")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "UserDefinedWords-continuations";
            textLines = new TestTextLine[] {
                new TestTextLine("laurent_prud-"),
                new TestTextLine('-',"    on10 "),
                new TestTextLine('-',"    !super-"),
                new TestTextLine('-',"    _@#1254540."),
                new TestTextLine('-',"    10azfdaedf99*8:tshg; prud'hon'="),
                new TestTextLine('-',"    =")
            };
            result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckPictureCharacterString()
        {
            string testName = "PictureCharacterString-continuations";
            TextLine[] textLines = new TestTextLine[] {
                new TestTextLine("PICTURE IS X"),
                new TestTextLine('-',"    .4. PIC IS Y."),
                new TestTextLine('-',"    3. PICTURE"),
                new TestTextLine("X("),
                new TestTextLine('-',"    3)"),
                new TestTextLine("PICTURE abcd,"),
                new TestTextLine('-',"    YKF3.")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckCommentEntry()
        {
            string testName = "CommentEntry-continuations";
            TextLine[] textLines = new TestTextLine[] {
                new TestTextLine("AUTHOR comment on several"),
                new TestTextLine('-',"    lines without delimiter"),
                new TestTextLine("INSTALLATION"),
                new TestTextLine("     comment1"),
                new TestTextLine('-',"comment2")
            };
            string result = ScannerUtils.ScanLines(textLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }
    }
}
