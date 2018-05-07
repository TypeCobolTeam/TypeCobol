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
using TypeCobol.Compiler.CupParser;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using System.Reflection;
using System.Runtime.CompilerServices;

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

        public static void ParseProgramOrClass(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> codeElementsLines, TypeCobolOptions compilerOptions, SymbolTable customSymbols, PerfStatsForParserInvocation perfStatsForParserInvocation, out SourceFile root, out List<Diagnostic> diagnostics, out Dictionary<CodeElement, Node> nodeCodeElementLinkers )
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
            perfStatsForParserInvocation.OnStartParsing();
            if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.BeginParsingSection();
            ProgramClassParser.CobolCompilationUnitContext programClassParseTree = cobolParser.cobolCompilationUnit();
            if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.EndParsingSection(programClassParseTree.ChildCount);
            perfStatsForParserInvocation.OnStopParsing(
                AntlrPerformanceProfiler != null ? (int)AntlrPerformanceProfiler.CurrentFileInfo.DecisionTimeMs : 0,
                AntlrPerformanceProfiler != null ? AntlrPerformanceProfiler.CurrentFileInfo.RuleInvocations.Sum() : 0);

            if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.EndParsingFile(cobolParser.ParseInfo.DecisionInfo, (int)(cobolParser.ParseInfo.GetTotalTimeInPrediction() / 1000000));


            // Visit the parse tree to build a first class object representing a Cobol program or class
            ParseTreeWalker walker = new ParseTreeWalker();
            CobolNodeBuilder programClassBuilder = new CobolNodeBuilder();
            diagnostics = new List<Diagnostic>();
            programClassBuilder.SyntaxTree = new SyntaxTree<ParserRuleContext>(); //Initializie SyntaxTree for the current source file
            programClassBuilder.CustomSymbols = customSymbols;
            programClassBuilder.Dispatcher = new NodeDispatcher<ParserRuleContext>();
            programClassBuilder.Dispatcher.CreateListeners();

            perfStatsForParserInvocation.OnStartTreeBuilding();

            ParserDiagnostic programClassBuilderError = null;
            try { walker.Walk(programClassBuilder, programClassParseTree); }
            catch (Exception ex)
            {
                var code = Diagnostics.MessageCode.ImplementationError;
                programClassBuilderError = new ParserDiagnostic(ex.ToString(), null, null, code, ex);
            }

            root = programClassBuilder.SyntaxTree.Root; //Set output root node

            //Create link between data definition an Types, will be stored in SymbolTable
            root.AcceptASTVisitor(new TypeCobolLinker());

            //Stop measuring tree building performance
            perfStatsForParserInvocation.OnStopTreeBuilding();

            // Register compiler results
            var syntaxTreeDiag = programClassBuilder.GetDiagnostics(programClassParseTree);
            if (syntaxTreeDiag != null)
                diagnostics.AddRange(syntaxTreeDiag);
            nodeCodeElementLinkers = programClassBuilder.NodeCodeElementLinkers;

            if (programClassBuilderError != null)
            {
                diagnostics.Add(programClassBuilderError);
            }
        }

        private static bool CupPrepared = false;
        /// <summary>
        /// This static prepare the parser generated method CUP_TypeCobolProgramParser_do_action.
        /// Which in debug mode JIT precompilation takes time about 400ms.
        /// </summary>
        public static void PrepareCupParser()
        {
            if (!CupPrepared)
            {
                var method = typeof(CUP_TypeCobolProgramParser_actions).GetMethod("CUP_TypeCobolProgramParser_do_action");
                if (method != null)
                {
                    RuntimeHelpers.PrepareMethod(method.MethodHandle);
                    CupPrepared = true;
                }
            }
        }
        public static void CupParseProgramOrClass(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> codeElementsLines, TypeCobolOptions compilerOptions, SymbolTable customSymbols, PerfStatsForParserInvocation perfStatsForParserInvocation, out SourceFile root, out List<Diagnostic> diagnostics, out Dictionary<CodeElement, Node> nodeCodeElementLinkers)
        {
            PrepareCupParser();
            //var t1 = DateTime.UtcNow;            
            CodeElementTokenizer scanner = new CodeElementTokenizer(codeElementsLines);
            TypeCobolProgramParser parser = new TypeCobolProgramParser(scanner);
            CupParserTypeCobolProgramDiagnosticErrorReporter diagReporter = new CupParserTypeCobolProgramDiagnosticErrorReporter();
            parser.ErrorReporter = diagReporter;
            ProgramClassBuilder builder = new ProgramClassBuilder();
            parser.Builder = builder;
            ParserDiagnostic programClassBuilderError = null;

            builder.SyntaxTree = new SyntaxTree<CodeElement>(); //Initializie SyntaxTree for the current source file
            builder.CustomSymbols = customSymbols;
            builder.Dispatcher = new NodeDispatcher<CodeElement>();
            builder.Dispatcher.CreateListeners();

            // Try to parse a Cobol program or class, with cup w are also building the The Syntax Tree Node
            perfStatsForParserInvocation.OnStartParsing();
            try
            {
                TUVienna.CS_CUP.Runtime.Symbol symbol = parser.parse();
            }
            catch (Exception ex)
            {
                var code = Diagnostics.MessageCode.ImplementationError;
                programClassBuilderError = new ParserDiagnostic(ex.ToString(), null, null, code, ex);
            }
            perfStatsForParserInvocation.OnStopParsing(0, 0);

            //            var t2 = DateTime.UtcNow;
            //            var t = t2 - t1;
            //#if DEBUG
            //            System.Diagnostics.Debug.WriteLine("Time[" + textSourceInfo.Name + "];" + t.Milliseconds);
            //#else
            //            System.Console.WriteLine("Time[" + textSourceInfo.Name + "];" + t.Milliseconds);
            //#endif
            root = builder.SyntaxTree.Root; //Set output root node

            perfStatsForParserInvocation.OnStartTreeBuilding();

            //Create link between data definition an Types, will be stored in SymbolTable
            root.AcceptASTVisitor(new TypeCobolLinker());

            //Stop measuring tree building performance
            perfStatsForParserInvocation.OnStopTreeBuilding();

            // Register compiler results
            diagnostics = diagReporter.Diagnostics ?? new List<Diagnostic>();
            nodeCodeElementLinkers = builder.NodeCodeElementLinkers;

            if (programClassBuilderError != null)
            {
                diagnostics.Add(programClassBuilderError);
            }
        }

        public static void CrossCheckPrograms(SourceFile root)
        {
            //Complete some information on Node and run checker that need a full AST
            root.AcceptASTVisitor(new CrossCompleteChecker());
        }

    }
}
