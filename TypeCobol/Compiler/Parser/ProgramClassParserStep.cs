using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Parse a complete Cobol program or class object from a set of CodeElements changes.
    /// This ultimate step of parsing is not incremental.
    /// </summary>
    static class ProgramClassParserStep
    {
        public static void ParseProgramOrClass(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> codeElementsLines, TypeCobolOptions compilerOptions, out Program newProgram, out Class newClass, out IList<ParserDiagnostic> diagnostics)
        {
            // Create an Antlr compatible token source on top a the token iterator
            CodeElementsLinesTokenSource tokenSource = new CodeElementsLinesTokenSource(
                textSourceInfo.Name,
                codeElementsLines);

            // Init parser
            ITokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            CobolProgramClassParser cobolParser = new CobolProgramClassParser(tokenStream);
            // -> activate full ambiguities detection
            //parser.Interpreter.PredictionMode = PredictionMode.LlExactAmbigDetection; 

            // Register all parse errors in a list in memory
            DiagnosticSyntaxErrorListener errorListener = new DiagnosticSyntaxErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Try to parse a Cobol program or class
            CobolProgramClassParser.CobolCompilationUnitContext codeElementParseTree = cobolParser.cobolCompilationUnit();

            // Visit the parse tree to build a first class object representing a Cobol program or class
            ParseTreeWalker walker = new ParseTreeWalker();
            ProgramClassBuilder programClassBuilder = new ProgramClassBuilder();
            walker.Walk(programClassBuilder, codeElementParseTree);
                        
            // Register compiler results
            newProgram = programClassBuilder.Program;
            newClass = programClassBuilder.Class;
            diagnostics = errorListener.Diagnostics;
        }
    }
}
