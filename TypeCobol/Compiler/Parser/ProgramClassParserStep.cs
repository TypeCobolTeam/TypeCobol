//#define DEBUG_ANTRL_CUP_TIME
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
using TypeCobolProgramParser = TypeCobol.Compiler.CupParser.TypeCobolProgramParser;

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
        public static void CupParseProgramOrClass(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> codeElementsLines, TypeCobolOptions compilerOptions, SymbolTable customSymbols, PerfStatsForParserInvocation perfStatsForParserInvocation, out SourceFile root, out List<Diagnostic> diagnostics, 
            out Dictionary<CodeElement, Node> nodeCodeElementLinkers,
            out List<DataDefinition> typedVariablesOutsideTypedef,
            out List<TypeDefinition> typeThatNeedTypeLinking)
        {
            PrepareCupParser();
#if DEBUG_ANTRL_CUP_TIME
            var t1 = DateTime.UtcNow;            
#endif
            CodeElementTokenizer scanner = new CodeElementTokenizer(codeElementsLines);
            CupParser.TypeCobolProgramParser parser = new CupParser.TypeCobolProgramParser(scanner);
            CupParserTypeCobolProgramDiagnosticErrorReporter diagReporter = new CupParserTypeCobolProgramDiagnosticErrorReporter();
            parser.ErrorReporter = diagReporter;
            ProgramClassBuilder builder = new ProgramClassBuilder();
            parser.Builder = builder;
            ParserDiagnostic programClassBuilderError = null;

            builder.SyntaxTree = new SyntaxTree(); //Initializie SyntaxTree for the current source file
            builder.CustomSymbols = customSymbols;
            builder.Dispatcher = new NodeDispatcher();
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

#if DEBUG_ANTRL_CUP_TIME
            var t2 = DateTime.UtcNow;
            var t = t2 - t1;
            System.Diagnostics.Debug.WriteLine("Time[" + textSourceInfo.Name + "];" + t.Milliseconds);
#endif
            root = builder.SyntaxTree.Root; //Set output root node

            perfStatsForParserInvocation.OnStartTreeBuilding();


            //Stop measuring tree building performance
            perfStatsForParserInvocation.OnStopTreeBuilding();

            // Register compiler results
            diagnostics = diagReporter.Diagnostics ?? new List<Diagnostic>();
            nodeCodeElementLinkers = builder.NodeCodeElementLinkers;
            typedVariablesOutsideTypedef = builder.TypedVariablesOutsideTypedef;
            typeThatNeedTypeLinking = builder.TypeThatNeedTypeLinking;

            if (programClassBuilderError != null)
            {
                diagnostics.Add(programClassBuilderError);
            }
        }

        public static void CrossCheckPrograms(SourceFile root, TemporarySemanticDocument temporarySemanticDocument)
        {
            //Create link between data definition an Types, will be stored in SymbolTable
            TypeCobolLinker.LinkedTypedVariables(temporarySemanticDocument.TypedVariablesOutsideTypedef, 
                temporarySemanticDocument.TypeThatNeedTypeLinking);

            //Complete some information on Node and run checker that need a full AST
            root.AcceptASTVisitor(new CrossCompleteChecker());
        }

    }
}
