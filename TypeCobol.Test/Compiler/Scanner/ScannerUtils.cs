﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Scanner
{
    /// <summary>
    /// Test implementation of TokensLine
    /// </summary>
    internal class TestTokensLine : TokensLine
    {
        public TestTokensLine(string text) : 
            base(new TextLineSnapshot(-1, text, null), ColumnsLayout.FreeTextFormat)
        { }

        public TestTokensLine(char indicator, string text) : 
            base(new TextLineSnapshot(-1, indicator + text, null), ColumnsLayout.FreeTextFormat)
        { }
    }

    internal class TextChangeMap : TextChange
    {
        public CobolTextLine NewLineMap { get; private set; }

        public TextChangeMap(TextChange change, ColumnsLayout columnsLayout) :
            base(change.Type, change.LineIndex, change.NewLine)
        {
            NewLineMap = new CobolTextLine(NewLine, columnsLayout);
        }
    }


    internal static class ScannerUtils
    {
        public static TextSourceInfo TextSourceInfo = new TextSourceInfo("test", IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), ColumnsLayout.FreeTextFormat);
        public static TypeCobolOptions CompilerOptions = new TypeCobolOptions();

        public static string ScanLine(string testLine)
        {
            TokensLine tokensLine = new TestTokensLine(testLine);

            return ScanTextLine(tokensLine);
        }

        public static string ScanLines(string[] testLines)
        {
            TokensLine[] tokensLines = new TokensLine[testLines.Length];
            for(int i = 0; i < testLines.Length; i++)
            {
                TokensLine tokensLine = new TestTokensLine(testLines[i]);
                tokensLines[i] = tokensLine;
            }

            return ScanLines(tokensLines);
        }

        public static string ScanTextLine(TokensLine tokensLine)
        {
            TypeCobol.Compiler.Scanner.Scanner.ScanFirstLine(tokensLine, false, false, false, TextSourceInfo.EncodingForAlphanumericLiterals, CompilerOptions);
            return BuildResultString(tokensLine);
        }
        
        public static string ScanLines(TokensLine[] tokensLines) 
        {
            ImmutableList<TokensLine>.Builder tokensLinesList = ImmutableList<TokensLine>.Empty.ToBuilder();
            tokensLinesList.AddRange(tokensLines);

            ScannerStep.ScanDocument(TextSourceInfo, tokensLinesList, CompilerOptions);

            StringBuilder sbResult = new StringBuilder();
            for (int i = 0; i < tokensLines.Length; i++)
            {
                sbResult.AppendLine("-- Line " + (i + 1) + " --");
                sbResult.AppendLine(BuildResultString(tokensLines[i]));
            }

            return sbResult.ToString();
        }
        
        public static string BuildResultString(ITokensLine tokensLine)
        {
            StringBuilder tokensText = new StringBuilder();
            foreach (Token token in tokensLine.SourceTokens)
            {
                tokensText.AppendLine(token.ToString());
            }

            StringBuilder diagnosticsText = new StringBuilder();
            foreach (Diagnostic diagnostic in tokensLine.ScannerDiagnostics)
            {
                diagnosticsText.AppendLine(diagnostic.ToString());
            }

            return tokensText.ToString() + diagnosticsText.ToString();
        }

        public static void CheckWithResultFile(string result, string testName)
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(@"Compiler\Scanner\ResultFiles\" + testName + ".txt")))
            {
                string expectedResult = reader.ReadToEnd();
                
                TestUtils.compareLines(testName, result, expectedResult); 
            }
        }
    }
}
