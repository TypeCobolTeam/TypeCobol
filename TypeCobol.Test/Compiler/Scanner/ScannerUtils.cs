using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;

namespace TypeCobol.Test.Compiler.Scanner
{
    /// <summary>
    /// Test implementation of the ITextLine interface
    /// </summary>
    internal class TestTextLine : TextLine
    {
        public TestTextLine(string text) : base(-1, 0, text)
        { }

        public TestTextLine(char indicator, string text) : base(-1, 0, indicator + text)
        { }
    }

    internal class TextChangeMap : TextChange
    {
        public TextLineMap NewLineMap { get; private set; }

        public TextChangeMap(TextChange change, ColumnsLayout columnsLayout) :
            base(change.Type, change.LineIndex, change.NewLine)
        {
            NewLineMap = new TextLineMap(NewLine, columnsLayout);
        }
    }


    internal static class ScannerUtils
    {
        public static TextSourceInfo TextSourceInfo = new TextSourceInfo("test", IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), ColumnsLayout.FreeTextFormat);
        public static TypeCobolOptions CompilerOptions = new TypeCobolOptions();

        public static string ScanLine(string testLine)
        {
            ITextLine textLine = new TestTextLine(testLine);
            return ScanTextLine(textLine);
        }

        public static string ScanLines(string[] testLines)
        {
            IList <TokensLine> tokensLines = new List<TokensLine>();

            ITextLine firstTextLine = new TestTextLine(testLines[0]);
            TokensLine tokensLine = TypeCobol.Compiler.Scanner.Scanner.ScanFirstLine(firstTextLine, false, false, false, TextSourceInfo, CompilerOptions);
            tokensLines.Add(tokensLine);

            for (int i = 1; i < testLines.Length; i++)
            {
                TextLine textLine = new TestTextLine(testLines[i]);
                tokensLine = TypeCobol.Compiler.Scanner.Scanner.ScanTextLine(textLine, tokensLine, TextSourceInfo, CompilerOptions);
                tokensLines.Add(tokensLine);
            }

            StringBuilder sbResult = new StringBuilder();

            for(int i=0 ; i<testLines.Length ; i++)
            {         
                sbResult.AppendLine("-- Line "+(i+1)+" --");
                sbResult.AppendLine(BuildResultString(tokensLines[i]));
            }

            return sbResult.ToString();
        }

        public static string ScanTextLine(ITextLine textLine)
        {
            TokensLine tokensLine = TypeCobol.Compiler.Scanner.Scanner.ScanFirstLine(textLine, false, false, false, TextSourceInfo, CompilerOptions);
            return BuildResultString(tokensLine);
        }
        
        public static string ScanLines(ITextLine[] textLines) 
        {
            IList<TokensLine> tokensLines = new List<TokensLine>();

            ITextLine firstTextLine = textLines[0];
            TokensLine tokensLine = TypeCobol.Compiler.Scanner.Scanner.ScanFirstLine(firstTextLine, false, false, false, TextSourceInfo, CompilerOptions);
            tokensLines.Add(tokensLine);

            for (int i = 1; i < textLines.Length; i++)
            {
                ITextLine textLine = textLines[i];
                tokensLine = TypeCobol.Compiler.Scanner.Scanner.ScanTextLine(textLine, tokensLine, TextSourceInfo, CompilerOptions);
                tokensLines.Add(tokensLine);
            }

            StringBuilder sbResult = new StringBuilder();

            for (int i = 0; i < textLines.Length; i++)
            {
                sbResult.AppendLine("-- Line " + (i + 1) + " --");
                sbResult.AppendLine(BuildResultString(tokensLines[i]));
            }

            return sbResult.ToString();
        }
        
        public static string BuildResultString(TokensLine tokensLine)
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
