using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser
{
    static class TestTokenSource {
        public static readonly string SampleFolder = "Parser"+ Path.DirectorySeparatorChar + "Samples";

        public static void Check_CobolCharStream()
        {
            // Test file properties
            string relativePath = SampleFolder;
            string textName = "MSVCOUT";
            DocumentFormat documentFormat = DocumentFormat.RDZReferenceFormat;

            // Compile test file
            CompilationDocument compilationDocument = ParserUtils.ScanCobolFile(relativePath, textName, true, documentFormat);

            // Create a token iterator on top of tokens lines
            TokensLinesIterator tokensIterator = new TokensLinesIterator(
                compilationDocument.TokensDocumentSnapshot.TextSourceInfo.Name,
                compilationDocument.TokensDocumentSnapshot.Lines,
                null,
                Token.CHANNEL_SourceTokens);

            // Crate an Antlr compatible token source on top a the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                compilationDocument.TokensDocumentSnapshot.TextSourceInfo.Name,
                tokensIterator);
            tokenSource.NextToken();

            // Get underlying CharStream
            ICharStream charStream = tokenSource.InputStream;

            if(charStream.Index != 0)
            {
                throw new Exception("Char stream index should start at 0");
            }
            if (charStream.La(0) != 0)
            {
                throw new Exception("La(0) should be 0");
            }
            if (charStream.La(1) != '0')
            {
                throw new Exception("La(1) should be 0");
            }
            if (charStream.La(4) != '1')
            {
                throw new Exception("La(4) should be 1");
            }
            if (charStream.La(5) != '6')
            {
                throw new Exception("La(5) should be 6");
            }

            charStream.Consume();
            if (charStream.Index != 1)
            {
                throw new Exception("Char stream index should be 1 after consume");
            }
            if (charStream.La(4) != '6')
            {
                throw new Exception("La(4) should be 6 after consume");
            }
            if (charStream.La(80) != IntStreamConstants.Eof)
            {
                throw new Exception("La(80) should be Eof");
            }
            
            charStream.Seek(12);
            if(charStream.Index != 12)
            {
                throw new Exception("Char stream index should be 12 after seek");
            }
            if (charStream.La(-1) != ':')
            {
                throw new Exception("La(-1) should be : after seek");
            }
            if (charStream.La(1) != 'M')
            {
                throw new Exception("La(1) should be M after seek");
            }
            // should do nothing
            int marker = charStream.Mark();
            charStream.Release(marker);
            if (charStream.La(2) != 'S')
            {
                throw new Exception("La(2) should be S after release");
            }

            string text = charStream.GetText(new Interval(11,18));
            if (text != ":MSVCOUT")
            {
                throw new Exception("Char stream GetText method KO");
            }

            if (charStream.Size != 80)
            {
                throw new Exception("Char stream size KO");
            }
        }

        public static void Check_CobolTokenSource()
        {
            // Test file properties
            string relativePath = SampleFolder;
            string textName = "MSVCOUT";
            DocumentFormat docFormat = DocumentFormat.RDZReferenceFormat;

            // Compile test file
            CompilationDocument compilationDocument = ParserUtils.ScanCobolFile(relativePath, textName, true, docFormat);

            // Create a token iterator on top of tokens lines
            TokensLinesIterator tokensIterator = new TokensLinesIterator(
                compilationDocument.TokensDocumentSnapshot.TextSourceInfo.Name,
                compilationDocument.TokensDocumentSnapshot.Lines,
                null,
                Token.CHANNEL_SourceTokens);

            // Crate an Antlr compatible token source on top a the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                compilationDocument.TokensDocumentSnapshot.TextSourceInfo.Name,
                tokensIterator);

            if (tokenSource.SourceName != "MSVCOUT")
            {
                throw new Exception("Token source name KO");
            }

            var source = new Tuple<ITokenSource, ICharStream>(tokenSource, tokenSource.InputStream);
            IToken token = tokenSource.TokenFactory.Create(source, (int)TokenType.IntegerLiteral, "314", Token.CHANNEL_CompilerDirectives, 10, 20, 30, 17);
            if (token.Channel != Token.CHANNEL_CompilerDirectives || token.Column != 18 || token.Line != 1 ||
                token.StartIndex != 17 || token.StopIndex != 16 || token.Text != "314" ||
                token.TokenIndex != -1 || token.InputStream == null || token.TokenSource == null ||
                token.Type != (int)TokenType.IntegerLiteral || ((IntegerLiteralTokenValue)((Token)token).LiteralValue).Number != 314)
            {
                throw new Exception("TokenFactory second Create method KO");
            }
            
            if(tokenSource.Column != 0)
            {
                throw new Exception("Token source column should be 0 at start");
            }
            if (tokenSource.Line != 1)
            {
                throw new Exception("Token source line should be 1 at start");
            }

            IList<IToken> tokensList = new List<IToken>();
            for(int i=0 ; token.Type != (int)TokenType.EndOfFile; i++)
            {
                token = tokenSource.NextToken();
                tokensList.Add(token);
            }
            if(tokensList.Count != 293 ||
                tokensList[0].Text != "01" ||
                tokensList[1].Text != ":MSVCOUT:" ||
                tokensList[290].Text != "'/MSVCOUT'" ||
                tokensList[292].Type != (int)TokenType.EndOfFile)
            {
                throw new Exception("Token source nextToken method KO");
            }
        }

        public static void Check_CobolTokenSource_WithStartToken()
        {
            // Test file properties
            string relativePath = SampleFolder;
            string textName = "MSVCOUT";
            Encoding encoding = Encoding.GetEncoding(1252);
            DocumentFormat docFormat = DocumentFormat.RDZReferenceFormat;

            // Compile test file
            CompilationDocument compilationDocument = ParserUtils.ScanCobolFile(relativePath, textName, true, docFormat);

            // Search for first level 88 as a start token
            Token startToken = compilationDocument.TokensDocumentSnapshot.SourceTokens.First(t => (t.TokenType == TokenType.LevelNumber && ((IntegerLiteralTokenValue)t.LiteralValue).Number == 88));

            // Create a token iterator on top of tokens lines
            TokensLinesIterator tokensIterator = new TokensLinesIterator(
                compilationDocument.TokensDocumentSnapshot.TextSourceInfo.Name,
                compilationDocument.TokensDocumentSnapshot.Lines,
                startToken,
                Token.CHANNEL_SourceTokens);

            // Crate an Antlr compatible token source on top a the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                compilationDocument.TokensDocumentSnapshot.TextSourceInfo.Name,
                tokensIterator);

            IToken token = null;
            IList<IToken> tokensList = new List<IToken>();
            for (int i = 0; i < 9 ; i++)
            {
                token = tokenSource.NextToken();
                tokensList.Add(token);
            }
            if (tokensList[0].Text != "88" ||
                tokensList[1].Text != ":MSVCOUT:-RtnCod-OK" ||
                tokensList[7].Text != "VALUE" ||
                tokensList[8].Text != "'STUB'")
            {
                throw new Exception("Token source nextToken method with start token KO");
            }
        }
    }
}
