using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Nodes;
using System.Linq;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Parse a complete Cobol program or class object from a set of CodeElements changes.
    /// This ultimate step of parsing is not incremental.
    /// </summary>
    static class ProgramClassParserStep
    {
        // When not null, optionnaly used to gather Antlr performance profiling information
        public static AntlrPerformanceProfiler AntlrPerformanceProfiler;

        public static void ParseProgramOrClass(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> codeElementsLines, TypeCobolOptions compilerOptions, SymbolTable customSymbols, PerfStatsForParserInvocation perfStatsForParserInvocation, out SourceFile root, out IList<ParserDiagnostic> diagnostics, out Dictionary<CodeElement, Node> nodeCodeElementLinkers )
        {
            // Create an Antlr compatible token source on top a the token iterator
            CodeElementsLinesTokenSource tokenSource = new CodeElementsLinesTokenSource(
                textSourceInfo.Name,
                codeElementsLines);

            // Init parser
            ITokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            ProgramClassParser cobolParser = new ProgramClassParser(tokenStream);
            // -> activate full ambiguities detection
            //parser.Interpreter.PredictionMode = PredictionMode.LlExactAmbigDetection; 

            // Optionnaly activate Antlr Parser performance profiling
            // WARNING : use this in a single-treaded context only (uses static field)  
            if (AntlrPerformanceProfiler == null && perfStatsForParserInvocation.ActivateDetailedAntlrPofiling) AntlrPerformanceProfiler = new AntlrPerformanceProfiler(cobolParser);
            if (AntlrPerformanceProfiler != null)
            {
                // Replace the generated parser by a subclass which traces all rules invocations
                cobolParser = new ProgramClassTracingParser(tokenStream);
                AntlrPerformanceProfiler.BeginParsingFile(textSourceInfo, null);
            }

            // Register all parse errors in a list in memory
            ParserDiagnosticErrorListener errorListener = new ParserDiagnosticErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Try to parse a Cobol program or class
            perfStatsForParserInvocation.OnStartAntlrParsing();
            if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.BeginParsingSection();
            ProgramClassParser.CobolCompilationUnitContext programClassParseTree = cobolParser.cobolCompilationUnit();
            if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.EndParsingSection(programClassParseTree.ChildCount);
            perfStatsForParserInvocation.OnStopAntlrParsing(
                AntlrPerformanceProfiler != null ? (int)AntlrPerformanceProfiler.CurrentFileInfo.DecisionTimeMs : 0,
                AntlrPerformanceProfiler != null ? AntlrPerformanceProfiler.CurrentFileInfo.RuleInvocations.Sum() : 0);

            if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.EndParsingFile(cobolParser.ParseInfo.DecisionInfo, (int)(cobolParser.ParseInfo.GetTotalTimeInPrediction() / 1000000));


            // Visit the parse tree to build a first class object representing a Cobol program or class
            ParseTreeWalker walker = new ParseTreeWalker();
            CobolNodeBuilder programClassBuilder = new CobolNodeBuilder();
            programClassBuilder.SyntaxTree = new SyntaxTree(); //Initializie SyntaxTree for the current source file
			programClassBuilder.CustomSymbols = customSymbols;
            programClassBuilder.Dispatcher = new NodeDispatcher();
            programClassBuilder.Dispatcher.CreateListeners();

            perfStatsForParserInvocation.OnStartTreeBuilding();

            ParserDiagnostic programClassBuilderError = null;
            try { walker.Walk(programClassBuilder, programClassParseTree); }
            catch (Exception ex)
            {
                var code = Diagnostics.MessageCode.ImplementationError;
                programClassBuilderError = new ParserDiagnostic(ex.ToString(), null, null, code, ex);
            }

            //Create link between datas
            programClassBuilder.SyntaxTree.Root.AcceptASTVisitor(new TypeCobolLinker());

            perfStatsForParserInvocation.OnStopTreeBuilding();

            //Complete some information on Node and run checker that need a full AST
            programClassBuilder.SyntaxTree.Root.AcceptASTVisitor(new Cobol85CompleteASTChecker());
              
           
            // Register compiler results
            root = programClassBuilder.SyntaxTree.Root; //Set output root node
            diagnostics = programClassBuilder.GetDiagnostics(programClassParseTree);
            nodeCodeElementLinkers = programClassBuilder.NodeCodeElementLinkers;

            if (programClassBuilderError != null)
            {
                if (diagnostics == null) diagnostics = new List<ParserDiagnostic>();
                diagnostics.Add(programClassBuilderError);
            }
        }


    }
}
