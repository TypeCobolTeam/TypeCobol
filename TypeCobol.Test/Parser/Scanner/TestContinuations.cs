namespace TypeCobol.Test.Parser.Scanner
{
    static class TestContinuations
    {
        public static void CheckSeparators()
        {
            string testName = "Separators-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                // .     , ; ():.
                new TestTokensLine("."),
                new TestTokensLine('-',"    "),
                new TestTokensLine("."),
                new TestTokensLine('-'," a"),
                new TestTokensLine("."),
                new TestTokensLine('-',"    a"),
                new TestTokensLine(","),
                new TestTokensLine('-',"    a"),
                new TestTokensLine(";"),
                new TestTokensLine('-',"    a"),
                new TestTokensLine("("),
                new TestTokensLine('-',"    a"),
                new TestTokensLine(")"),
                new TestTokensLine('-',"    a"),
                new TestTokensLine(":"),
                new TestTokensLine('-',"    a"),
                new TestTokensLine("(."),
                new TestTokensLine('-',"025 * ."),
                new TestTokensLine('-',"44"),
                new TestTokensLine('-',") + 1"),
                new TestTokensLine('-',".1"),
                new TestTokensLine('-',"8.")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckComments()
        {
            string testName = "Comments-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                // Comment lines
                // Floating comments
                // Blank lines
                new TestTokensLine("1"),
                new TestTokensLine('*', "This is a comment line ! ***"),
                new TestTokensLine('-',"    .234*> and a floating comment"),
                new TestTokensLine('-',"    56"),
                new TestTokensLine('-',"      "),
                new TestTokensLine('-',"    78*> and another floating comment"),
                new TestTokensLine('*', "This is another comment line ! ***"),
                new TestTokensLine(".")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckOperators()
        {
            string testName = "Operators-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                // + - * / **
                // < > <= >= = 
                new TestTokensLine("1 +"),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 + "),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 -"),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 *"),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 /"),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 *"),
                new TestTokensLine('-',"    * 2"),
                new TestTokensLine("1 <"),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 >"),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 <"),
                new TestTokensLine('-',"    = 2"),
                new TestTokensLine("1 >"),
                new TestTokensLine('-',"    = 2"),
                new TestTokensLine("1 ="),
                new TestTokensLine('-',"    2"),
                new TestTokensLine("1 ="),
                new TestTokensLine('-',"    = 2")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckAlphanumericLiterals()
        {
            string testName = "AlphanumericLiterals-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                // " N" G" Z" X" NX" 
                new TestTokensLine("\"toto  "),
                new TestTokensLine('-',"    \"and titi1\"."),    
                new TestTokensLine("\"toto  "),
                new TestTokensLine('-',"\"and titi2\"."), 
                new TestTokensLine("\"toto  \""),
                new TestTokensLine('-',"    \"and titi3\"."),  
                new TestTokensLine("\"toto  \" "),
                new TestTokensLine('-',"    \"and titi3b\"."),    
                new TestTokensLine("\"toto  "),
                new TestTokensLine('-',"    and titi4\"."), 
                new TestTokensLine("\"toto  "),
                new TestTokensLine('-',"    'and titi5\"."),
                new TestTokensLine("\"toto  \""),
                new TestTokensLine('-',"    \"\"and titi6\"."),
                new TestTokensLine("\"toto  \""),
                new TestTokensLine('-',"    \"\"and titi7  "),
                new TestTokensLine('-',"    \"and titi8\"."),
                new TestTokensLine("'toto  "),
                new TestTokensLine('-',"    'and titi9'"),  
                new TestTokensLine("'toto'  "),
                new TestTokensLine('-',"    'and titi10'"),
                new TestTokensLine("'toto'"),
                new TestTokensLine('-',"    'and titi11' "),
                new TestTokensLine("'toto'"),
                new TestTokensLine('-',"    ''and titi11b' "),
                new TestTokensLine("'toto'"),
                new TestTokensLine('-',"    \"and titi11c' "),
                new TestTokensLine("'toto  "),
                new TestTokensLine('-',"    \"and titi11d' "),
                new TestTokensLine(" N\"toto  "),
                new TestTokensLine('-',"    \"and titi12\""),
                new TestTokensLine("G\"toto  "),
                new TestTokensLine('-',"    \"and titi13\""),
                new TestTokensLine("Z\"toto  "),
                new TestTokensLine('-',"    \"and titi14\""),
                new TestTokensLine("X\"ABCD12"),
                new TestTokensLine('-',"    \"345678\""),
                new TestTokensLine("NX\"ABCD12"),
                new TestTokensLine('-',"    \"345678\""),
                new TestTokensLine("== toto an"),
                new TestTokensLine('-',"    d ti"),
                new TestTokensLine('-',"    ti15 =="),
                new TestTokensLine('-',"== super ="),
                new TestTokensLine('-',"= ="),
                new TestTokensLine('-',"=cool="),
                new TestTokensLine('-',"=."),
                new TestTokensLine(" "),
                new TestTokensLine('*',"Issue 60"),
                new TestTokensLine(' ',"                MOVE 'Lorem ipsum dolor sit amet, consectetur adi"),
                new TestTokensLine('-',"               'piscing elit, sed do eiusmod tempor incididunt ut"),
                new TestTokensLine('-',"               'labore et dolore magna aliqua                 '  "),
                new TestTokensLine(' ',"                                         TO  SOMEWHERE           ")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckNumericLiterals()
        {
            string testName = "NumericLiterals-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                new TestTokensLine("1"),
                new TestTokensLine('*', "This is a comment line ! ***"),
                new TestTokensLine('-',"    23456"),
                new TestTokensLine("12345"),
                new TestTokensLine("    "),
                new TestTokensLine('-',"    6"),
                new TestTokensLine("+"),
                new TestTokensLine('-',"123"),
                new TestTokensLine(" -1"),
                new TestTokensLine('-',"    23"),
                new TestTokensLine("."),
                new TestTokensLine('-',"    23"),
                new TestTokensLine("1"),
                new TestTokensLine('-',"    .23"),
                new TestTokensLine("1."),
                new TestTokensLine('-',"    23"),
                new TestTokensLine("1.2"),
                new TestTokensLine('-',"    3"),
                new TestTokensLine("1. "),
                new TestTokensLine('-',"    23"),
                new TestTokensLine(" -1.23"),
                new TestTokensLine('-',"    1E+"),
                new TestTokensLine('-',"    20."),
                new TestTokensLine("1.23"),
                new TestTokensLine('-',"    1E"),
                new TestTokensLine('-',"    +20.")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "NumericLiteralsComma-continuations";
            tokensLines = new TestTokensLine[] {
                new TestTokensLine("DECIMAL-POINT IS COMMA"),
                new TestTokensLine("1"),
                new TestTokensLine('*', "This is a comment line ! ***"),
                new TestTokensLine('-',"    23456"),
                new TestTokensLine("12345"),
                new TestTokensLine("    "),
                new TestTokensLine('-',"    6"),
                new TestTokensLine("+"),
                new TestTokensLine('-',"123"),
                new TestTokensLine(" -1"),
                new TestTokensLine('-',"    23"),
                new TestTokensLine(","),
                new TestTokensLine('-',"    23"),
                new TestTokensLine("1"),
                new TestTokensLine('-',"    ,23"),
                new TestTokensLine("1,"),
                new TestTokensLine('-',"    23"),
                new TestTokensLine("1,2"),
                new TestTokensLine('-',"    3"),
                new TestTokensLine("1, "),
                new TestTokensLine('-',"    23"),
                new TestTokensLine(" -1,23"),
                new TestTokensLine('-',"    1E+"),
                new TestTokensLine('-',"    20,"),
                new TestTokensLine("1,23"),
                new TestTokensLine('-',"    1E"),
                new TestTokensLine('-',"    +20,")
            };
            result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);             
        }

        public static void CheckKeywordsAndUserDefinedWords()
        {
            string testName = "Keywords-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                new TestTokensLine("REPLACE *"),
                new TestTokensLine('-',"    CONTROL BASIS COP"),
                new TestTokensLine('-',"    Y ALT"),
                new TestTokensLine('-',"    ER"),
                new TestTokensLine("end-"),
                new TestTokensLine('-',"    EXEC")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "UserDefinedWords-continuations";
            tokensLines = new TestTokensLine[] {
                new TestTokensLine("laurent_prud-"),
                new TestTokensLine('-',"    on10 "),
                new TestTokensLine('-',"    !super-"),
                new TestTokensLine('-',"    _@#1254540."),
                new TestTokensLine('-',"    10azfdaedf99*8:tshg; prud'hon'="),
                new TestTokensLine('-',"    =")
            };
            result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckPictureCharacterString()
        {
            string testName = "PictureCharacterString-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                new TestTokensLine("PICTURE IS X"),
                new TestTokensLine('-',"    .4. PIC IS Y."),
                new TestTokensLine('-',"    3. PICTURE"),
                new TestTokensLine("X("),
                new TestTokensLine('-',"    3)"),
                new TestTokensLine("PICTURE abcd,"),
                new TestTokensLine('-',"    YKF3.")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckCommentEntry()
        {
            string testName = "CommentEntry-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                new TestTokensLine("AUTHOR comment on several"),
                new TestTokensLine('-',"    lines without delimiter"),
                new TestTokensLine("INSTALLATION"),
                new TestTokensLine("     comment1"),
                new TestTokensLine('-',"comment2")
            };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckContinuationsMixedWithDebug()
        {
            string testName = "Debug-continuations";
            TestTokensLine[] tokensLines = new TestTokensLine[] {
                                                                    new TestTokensLine('D', "   05 var1 PIC X(3000) VALUE 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
                                                                    new TestTokensLine('-', "    'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"),
                                                                    new TestTokensLine('D', "    'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"),
                                                                    new TestTokensLine('-', "    'DDDDDDDDDDDDDDDDDD'.")
                                                                };
            string result = ScannerUtils.ScanLines(tokensLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }
    }
}
