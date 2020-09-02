using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Report;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Report
{
    /// <summary>
    /// ZCallPgmReport class using DFA.
    /// </summary>
    public class ZCallPgmReport : IReport
    {
        /// <summary>
        /// The list of all ZCALLXXX we need to detect
        /// </summary>
        public List<string> AlternativeCallList = new List<string>()
        {
            "zcallpgm",
            "zcallpgf",
            "zcallpgg",
            "zcallpgr",
            "zcallpgt",
            "zcallpgx",
            "zcallsrv",
        };

        /// <summary>
        /// The list of all Use Point CallStatement Nodes
        /// </summary>
        public List<DfaUsePoint<Node, Symbol>> CallUsePoints { get; private set; }

        /// <summary>
        /// All Control Flow Graphs for DFA Basic Block Information
        /// </summary>
        public IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> AllCfgs
        {
            get;
            private set;
        }

        /// <summary>
        /// Internal Writer.
        /// </summary>
        private TextWriter Writer { get; set; }

        /// <summary>
        /// Visitor to Collect all level 88 symbols.
        /// </summary>
        class Level88SymbolCollector : AbstractSymbolAndTypeVisitor<Dictionary<Symbol, Symbol>, Dictionary<Symbol, Symbol>>
        {
            Symbol ParentSymbol;
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="parentSymbol">The Parent symnol being collected</param>
            public Level88SymbolCollector(Symbol parentSymbol)
            {
                ParentSymbol = parentSymbol;
            }
            public override Dictionary<Symbol, Symbol> VisitSymbol(Symbol s, Dictionary<Symbol, Symbol> symbols)
            {
                s.Type?.Accept(this, symbols);
                return symbols;
            }

            public override Dictionary<Symbol, Symbol> VisitType(Compiler.Types.Type t, Dictionary<Symbol, Symbol> symbols)
            {
                t.TypeComponent?.Accept(this, symbols);
                return symbols;
            }

            public override Dictionary<Symbol, Symbol> VisitVariableSymbol(VariableSymbol s, Dictionary<Symbol, Symbol> symbols)
            {
                if (s.Level == 88)
                {
                    symbols[s] = ParentSymbol;
                    return symbols;
                }
                else return VisitSymbol(s, symbols);
            }

            public override Dictionary<Symbol, Symbol> VisitGroupType(Compiler.Types.GroupType t, Dictionary<Symbol, Symbol> symbols)
            {
                foreach (var field in t.Fields)
                {
                    field.Accept(this, symbols);
                }
                return symbols;
            }
        }


        /// <summary>
        /// Level 88 Symbol to their parent symbol map.
        /// </summary>
        private Dictionary<Symbol, Symbol> Level88SymbolParentSymbolMap;

        /// <summary>
        /// Callback method when a Use Point is encountered
        /// </summary>
        /// <param name="dfaBuilder">The Dfa Builder</param>
        /// <param name="up">The use Point</param>
        void OnCallUsePoint(DataFlowGraphBuilder<Node, Symbol> dfaBuilder, DfaUsePoint<Node, Symbol> up)
        {
            switch (up.Instruction.CodeElement.Type)
            {
                case CodeElementType.CallStatement:
                    {
                        CallStatement callStatement = up.Instruction.CodeElement as CallStatement;
                        string pgmVar = callStatement?.InputParameters.First().StorageAreaOrValue.MainSymbolReference.Name;
                        if (up.Variable.Name.Equals(pgmVar, StringComparison.InvariantCultureIgnoreCase))
                        {
                            var target = up.Instruction.CodeElement.CallSites.First().CallTarget;
                            foreach (var callStyle in AlternativeCallList)
                            {
                                if (target != null && target.ToString().Equals(callStyle, StringComparison.OrdinalIgnoreCase))
                                {
                                    CallUsePoints.Add(up);
                                    if (up.Variable.Type.Tag == Compiler.Types.Type.Tags.Group)
                                    {
                                        //Compute the Map of Level88 variable to their parent..
                                        if (Level88SymbolParentSymbolMap == null)
                                        {
                                            Level88SymbolParentSymbolMap = new Dictionary<Symbol, Symbol>();
                                        }
                                        Level88SymbolCollector l88c = new Level88SymbolCollector(up.Variable);
                                        up.Variable.Accept(l88c, Level88SymbolParentSymbolMap);
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Called when a DefPoint is built. If the Def point is a level 88 variable of the target CALL variable
        /// then we deceive DFA algorithm to think that the target variable of the DefPoint is the CALL variable
        /// thus we change the target variable of the DefPoint.
        /// </summary>
        /// <param name="dfaBuilder">The Dfa Builder</param>
        /// <param name="dp">The Def Point</param>
        private void OnDefPoint(DataFlowGraphBuilder<Node, Symbol> dfaBuilder, DfaDefPoint<Node, Symbol> dp)
        {
            VariableSymbol sym = (VariableSymbol)dp.Variable;
            Symbol parent = null;
            if (sym.Level == 88 && sym.Value != null && Level88SymbolParentSymbolMap != null && Level88SymbolParentSymbolMap.TryGetValue(sym, out parent))
            {//Now say that it is the parent variable which is the target of the definition.
                dp.Variable = parent;
                dp.UserData = sym;
            }
        }

        /// <summary>
        /// Report for a Cfg all Call use point.
        /// </summary>
        /// <param name="cfg">The Control Flow Graph instance</param>
        /// <returns>true</returns>
        private bool ReportCfgCallUsePoint(ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>> cfg)
        {
            //Check that a semantic data has been associated to this node.
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode != null);
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode.SemanticData != null);
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode.SemanticData.SemanticKind == SemanticKinds.Symbol);

            //The semantic data must be a ProgramSymbol or a FunctionSymbol
            System.Diagnostics.Debug.Assert(((Symbol)cfg.ProgramOrFunctionNode.SemanticData).Kind == Symbol.Kinds.Program ||
                                            ((Symbol)cfg.ProgramOrFunctionNode.SemanticData).Kind == Symbol.Kinds.Function);

            ProgramSymbol program = (ProgramSymbol)cfg.ProgramOrFunctionNode.SemanticData;

            CallUsePoints.Clear();
            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfg);
            dfaBuilder.OnUsePointEvent += OnCallUsePoint;
            dfaBuilder.OnDefPointEvent += OnDefPoint;
            dfaBuilder.ComputeUseDefSet();
            //Report Call Use Points.            
            foreach (DfaUsePoint<Node, Symbol> up in CallUsePoints)
            {
                List < Tuple<string, string> > defPaths = ComputeUsePointDefPaths(dfaBuilder, up);
                if (defPaths.Count > 0)
                {
                    foreach (var path in defPaths)
                    {
                        ReportUsePoint(up, path);
                    }
                }
            }
            return true;
        }

        /// <summary>
        /// Compute a List of definition paths for a UsePoint.
        /// </summary>
        /// <param name="dfaBuilder">The current DataFlow Builder</param>
        /// <param name="up">The Use Point instance</param>
        /// <returns></returns>
        private List<Tuple<string, string>> ComputeUsePointDefPaths(DefaultDataFlowGraphBuilder dfaBuilder, DfaUsePoint<Node, Symbol> up)
        {
            var valueOrigin = ValueOrigin.ComputeFrom(up, dfaBuilder);
            return AsPaths(valueOrigin).ToList();

            IEnumerable<Tuple<string, string>> AsPaths(ValueOrigin vo)
            {
                string variable = vo.Variable.FullDotName;
                if (vo.Value != null)
                {
                    string value = vo.Value.ToString();
                    if (vo.Variable.IsCondition)
                    {
                        yield return new Tuple<string, string>(value, $"\"{value}\"<-{variable}<-\"true\"");
                    }
                    else
                    {
                        yield return new Tuple<string, string>(value, $"{variable}<-\"{value}\"");
                    }
                }
                else
                {
                    System.Diagnostics.Debug.Assert(vo.Origins != null);
                    foreach (var voOrigin in vo.Origins)
                    {
                        foreach (var path in AsPaths(voOrigin))
                        {
                            yield return new Tuple<string, string>(path.Item1, $"{variable}<-{path.Item2}");
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Report a Call Use Point
        /// </summary>
        /// <param name="up">The Call Use Point to be reported</param>
        /// <param name="path">The Path to called Program</param>
        private void ReportUsePoint(DfaUsePoint<Node, Symbol> up, Tuple<string, string> path)
        {
            var name = path.Item1;
            string sourceText = up.Instruction.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ').Trim();
            int line = up.Instruction.CodeElement.Line;
            int column = up.Instruction.CodeElement.Column;

            //Remove all unneeded space
            sourceText = Regex.Replace(sourceText, @"\s+", " ");

            Writer.WriteLine(
                string.Format("ObjectName={0};Line={1};Column={2};Path={3};SourceText={4}",
                    name, line, column, path.Item2, sourceText));

        }

        /// <summary>
        /// Empty constructor
        /// </summary>
        public ZCallPgmReport() : this(null)
        {
        }

        /// <summary>
        /// Constructor this the list of cfg/dfa graphs to be reported.
        /// </summary>
        /// <param name="cfgs">All Control Flow Graphs DFA to be reported for ZCall Pgm</param>
        public ZCallPgmReport(IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs)
        {
            AllCfgs = cfgs;
        }

        public void Report(TextWriter writer, CompilationUnit unit = null)
        {
            if (unit != null)
            {
                if (!unit.TryGetAnalyzerResult<IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>>>(
                    CfgDfaAnalyzerFactory.CfgDfaIdentifier, out IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs))
                    return;
                AllCfgs = cfgs;
            }

            if (AllCfgs != null)
            {
                CallUsePoints = new List<DfaUsePoint<Node, Symbol>>();

                this.Writer = writer;
                foreach (var cfg in AllCfgs)
                {
                    ReportCfgCallUsePoint(cfg);
                }
            }
        }
    }
}
