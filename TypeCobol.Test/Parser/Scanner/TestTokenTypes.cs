using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Test.Parser.Scanner
{
    static class TestTokenTypes
    {
        public static void CheckSeparators()
        {
            string testName = "Separators1";
            string testLine = " .     , ; ():.";
            string result = ScannerUtils.ScanLine(testLine);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "Separators2";
            string[] testLines = new string[] {
                "",
                "   ",
                ",,,",
                ";;;",
                "...",
                ":::",
                "(((",
                ")))"
            };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "Separators3";
            testLine =  "(.025 * .44) + 1.18.";
            result = ScannerUtils.ScanLine(testLine);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckComments()
        {
            string testName = "Comments1";
            TokensLine testTokensLine =  new TestTokensLine('*',"This is a comment line ! ***");
            string result = ScannerUtils.ScanTextLine(testTokensLine);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "Comments2";
            string testLine = "01 TOTO *> Comment until end of line 1.";
            result = ScannerUtils.ScanLine(testLine);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "CommentsDebugging";
            string[] testLines = new string[] {
                "D CONFIGURATION SECTION.",
                "SOURCE-COMPUTER. IBM-392 WITH DEBUGGING MODE.",
                "D DISPLAY TOTO" };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckOperators()
        {
            string testName = "ArithmeticOperators";
            string[] testLines = new string[] {
                "1 + 2 - 3 * 4 / 5 ** 6",
                " -1 + +2 - -6 - 6 +",
                "1+2 +0 ++",
                "3 - 2 -1 --",
                "4*5 *6 ***",
                "7/8 /9 //",
                "1**3 **5 **",
                "1+2*4/(6 - 2)"
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "RelationalOperators";
            testLines = new string[] {
                "1 < 2 > 3 <= 4 >= 5 = 6",
                "1<2>3<=4>=5=6",
                "1<2 <0 <<",
                "3>2 >1 >>",
                "4<=5 <=6 <==<<=",
                "7>=8 >=9 >==>>=",
                "1=3 =5 ==",
                "1="
            };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckAlphanumericLiterals()
        {
            string testName = "AlphanumericLiterals1";
            string[] testLines = new string[] {
                "\"toto\" 'titi' \"'tata'\" '\"tutu\"' \"\" '' .",
                "\"ok\" \"sans fin",
                "'ok' 'sans fin",
                "\"",
                "'",
                "\"toto\"'titi' \"'tata'\"'\"tutu\"' \"\"''.",
                "\"to\"\"to\" 'ti''ti' \"\"\"to\" '''ti' \"to\"\"\" 'ti'''",
                "\"\"",
                "''"
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "AlphanumericLiterals2";
            testLines = new string[] {
                "N\"toto\" N'titi' N G\"'tata'\" G'\"tutu\"' G Z\"\" Z'' Z.",
                "N\"ok\" N\"sans fin",
                "N'ok' N'sans fin",
                "N\"fin ligne\"",
                "N'fin ligne'",
                "N\"",
                "N'",
                "G\"ok\" G\"sans fin",
                "G'ok' G'sans fin",
                "G\"fin ligne\"",
                "G'fin ligne'",
                "G\"",
                "G'",
                "Z\"ok\" Z\"sans fin",
                "Z'ok' Z'sans fin",
                "Z\"fin ligne\"",
                "Z'fin ligne'",
                "Z\"",
                "Z'"
            };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "AlphanumericLiterals3";
            testLines = new string[] {
                "X\"A396A396\" X'A389A389' X NX\"7400610074006100\" NX'7400750074007500' NX X\"\" NX'' X.",
                "X\"A396A396\" X'A389A3",
                "NX\"7400610074006100\" NX'7400",
                "X\"A396A396\"",
                "NX'7400750074007500'",
                "X\"A396A3960\"",
                "NX'74007500740075000'"
            };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckPseudoText()
        {
            string testName = "PseudoText";
            string[] testLines = new string[] {
                "==un texte. Il \"est\" super = ==",
                "==== = === === ",
                "====. ====a",
                " == pas fini=",
                " == pas fini==a",
                "=="
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckPseudoText2()
        {
            string testName = "PseudoText2";
            string[] testLines = new string[] {
                "====== ==",  //Incorrect
                              //Use == at the end, so the scanner is again in a correct state
                "==== ====.", //Correct
                "==::==== ==.",   //Incorrect
		                  //Use == at the end, so the scanner is again in a correct state
                "==::== ==::==.", //Correct
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckNumericLiterals()
        {
            string testName = "NumericLiterals";
            string[] testLines = new string[] {
                "01 50 00 17870154 +197 -254 -0 +0 - 0 0+ A10",
                "1.23 +0.47 -9.38 .2 +.45 -.4 15. 10.",
                "2.3e8 -.35e-17 -50.7851E+20 +.45486756E+1 10.E2 10e+4 .55E+9847 .25e",
                "1234537",
                "+1234567",
                ".1234567",
                " -.321E18.",
                "+."
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "NumericLiteralsComma";
            testLines = new string[] {
                "DECIMAL-POINT IS COMMA",
                "01 50 00 17870154 +197 -254 -0 +0 - 0 0+ A10",
                "1,23 +0,47 -9,38 ,2 +,45 -,4 15, 10,",
                "2,3e8 -,35e-17 -50,7851E+20 +,45486756E+1 10,E2 10e+4 ,55E+9847 ,25e",
                //FloatingPoint followed by IntegerLiteral
                " ,25e-24-1  ,25e-24-100 ,25e-24-100-123",
                //FloatingPoint followed by UserDefinedWord
                ",25e-24X ,25e-24X-1 ,25e-24X-1X",
                "1234537",
                "+1234567",
                ",1234567",
                " -,321E18,"
            };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckKeywordsAndUserDefinedWords()
        {
            string testName = "Keywords";
            string[] testLines = new string[] {
                "*CBL *CONTROL BASIS COP",
                "Y delete eXeC ADDI ALT",
                "ER SKIp1",
                "TITLE end-EXEC.",
                " CBL ARCH(6)"
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "UserDefinedWords";
            testLines = new string[] {
                "laurent_prud-on10",
                "!super-_@#1254540.10azfdaedf99*8:tshg; prud'hon'==",
                "123ABC-01 123 123a 123-abc",
                " -123ABC-01 _123ABC-01_ 123ABC- 123- 123-456 123E-06 123-4X -123X -123-"
            };
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            testName = "TCKeywords";
            testLines = new string[]
                        {
                            "TYPEDEF STRONG UNSAFE PUBLIC PRIVATE IN-OUT STRICT",
                            "TyPeDeF StRoNg UnSaFe PuBlIc PrIvAtE In-oUt StRiCt",
                            "MOVE public TO private",
                            "MOVE UNSAFE strong TO strict"
                        };

            //Scan as pure cobol
            ScannerUtils.CompilerOptions.IsCobolLanguage = true;
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName + "-AsCobol85");

            //Scan as TypeCobol
            ScannerUtils.CompilerOptions.IsCobolLanguage = false;
            result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName + "-AsTypeCobol");
        }

        /// <summary>
        /// CheckPartialCobolWords
        /// </summary>
        /// <param name="cobol">true for Pure Cobol Language, false otherwise</param>
        public static void CheckPartialCobolWords(bool cobol)
        {
            string testName = cobol ? "PartialCobolWords" : "PartialCobolWordsTC";
            string[] testLines = new string[] {
                "88  :MSVCOUT:-RtnCod-OK",
                "01  TOTO-:MSVCOUT:",
                "COPY REPLACING :MSVCOUT: BY MYCOPY1",
                "WORD1:WORD2 :WORD3(0:1)",
                ":TAG1:",
                " :TAG2:",
                ":TAG3: ",
                "a:TAG4:",
                ":TAG5:b",
                ":TAG6:.",
                "a01-:7::8:_:9:: b02-:10:11:12:ab c03-:13:abc:14:e",
                "REPLACING ==:TAG:== BY ==EXEC-==  ",
                "replace ==:NBJLCC:==        by ==7==",
                "05 W-TAB-LCC-X OCCURS :NBJLCC:",
                "replace ==:ZONE:== by ==NATIONALITE==.",
                "if (W-CCOMDE-UN-CHOIX(CCOMDI-:ZONE:) = 'D' or   ",
                "replace ==:ZONE:== by ==SUPX== ==:SSPRO:== by ==CCTZ023B==."
            };
            ScannerUtils.CompilerOptions.IsCobolLanguage = cobol;
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CompilerOptions.IsCobolLanguage = false;
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckPictureCharacterString()
        {
            string testName = "PictureCharacterString";
            string[] testLines = new string[] {
                "PICTURE SYMBOL sym1 PIC SYMBOL sym2 PICTURE",
                "IS X.4. PIC IS Y.3. PICTURE",
                "YKF3.,;145",
                "PIC X(3)",
                "PICTURE abcd,a PIc cdef;y; picTURE yuv.1.",
                "abcd,a PIC abcd,",
                "cdef;y; PIC cdef;",
                "PIC 9(3) PIC +$$$9.9."
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckCommentEntry()
        {
            string testName = "CommentEntry";
            string[] testLines = new string[] {
                "AUTHOR comment on several",
                "    lines without delimiter",
                "AUTHOR. comment on one line \"with\" : special chars.",
                "AUTHOR. ..........",
                "INSTALLATION",
                "     comment1",
                "comment2",
                "    comment3",
                " DATE-WRITTEN.",
                "       comment1",
                "    comment2",
                "  DATE-COMPILED.",
                "    comment1",
                "       comment2",
                "   SECURITY",
                "      comment1",
                "    comment2",
                " SECURITY.",
                "    comment1",
                "   comment2"

            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckExecStatement()
        {
            string testName = "ExecStatement";
            string[] testLines = new string[] {
                "EXEC SQL DELETE Table END-EXEC.",
                "EXEC SQL INCLUDE YCPP324 REPLACING",
                "EXEC EXEC EXEC SQL SQL SQL END-EXEC END-EXEC END-EXEC",
                "EXEC",
                "   SQL",
                "  tout un tas de blabla",
                "     bliblibli",
                "   c est la fin.END-EXEC ici.",
                " EXEC",
                " SQL",
                "INCLUDE YPPRCGF ",
                "END-EXEC.",
                "EXEC SQLIMS INCLUDE TOTO2 END-EXEC",
                "ExeC Sql",
                "include copytoinclude",
                "eND-Exec "
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckContextSensitiveKeywords()
        {
            string testName = "ContextSensitiveKeywords";
            string[] testLines = new string[] {
                "toto FUNCTION toto toto FUNCTION",
                "toto DELETE toto DELETE",
                "toto DELETE 0000001 DELETE",
                "00000001 DATA RECORD DISPLAY DATA",
                "DIVISION DISPLAY PROCEDURE POINTER",
                "DISPLAY PROCEDURE",
                "DIVISION DISPLAY ENTRY",
                "TO ENTRY ENTRY TO TOTO TO",
                "ENTRY SAME SORT SAME TOTO SORT SAME",
                "SORT TOTO"
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckSymbolicCharacters()
        {
            string testName = "SymbolicCharacters";
            string[] testLines = new string[] {
                "MOVE toto TO titi DISPLAY tata ADD tete TO tutu",
                "SYMBOLIC toto titi 10 20",
                "SYMBOLIC CHARACTERS tete IS 30",
                "tutu ARE 40 IN alphabetName",
                "DISPLAY toto titi",
                "tata tutu tyty tete"
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);
        }

        public static void CheckCblProcessCompilerDirective()
        {
            string testName = "CblProcessCompilerDirective";
            string[] testLines = new string[] {
                "CBL NOADATA AFP(VOLATILE) ARCH(6) CODEPAGE(1140) BUFSIZE(1K)",
                "  CBL CICS(’string2’),CICS(\"string3\"),CURRENCY(X\"0AB2\"),  BLOCK0",
                "    PROCESS EXIT( INEXIT([’str1’,]mod1) ,LIBEXIT([’str2’,]mod2) )",
                "PROCESS FLAG(I,I) FLAGSTD(x[yy][,0]) ",
                "CBL TOTO",
                "CBL ARCH.",
                "CBL ARCH(",
                "CBL (",
                "CBL ARCH(5",
                "CBL ARCH(5 B()",
                "PROCESS CP(1147) NOADV C"
            };
            string result = ScannerUtils.ScanLines(testLines);
            ScannerUtils.CheckWithResultFile(result, testName);

            if (ScannerUtils.CompilerOptions.CODEPAGE.IsActivated == false || ScannerUtils.CompilerOptions.CODEPAGE.Value != "1147" ||
                ScannerUtils.CompilerOptions.ADV.IsActivated == true || ScannerUtils.CompilerOptions.ADV.Value != null ||
                ScannerUtils.CompilerOptions.COMPILE.IsActivated == false || ScannerUtils.CompilerOptions.COMPILE.Value != null)
            {
                throw new Exception("IBMCompilerOptions not set by CBL/PROCESS compiler directives");
            }
        }
    }
}
