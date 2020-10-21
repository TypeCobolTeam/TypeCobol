using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
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
        private static readonly string[] _AlternativeCallList = new string[]
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
        /// Analyzer identifier to use to retrieve graphs if not directly supplied in constructor
        /// </summary>
        private readonly string _analyzerId;

        /// <summary>
        /// All Control Flow Graphs for DFA Basic Block Information
        /// </summary>
        private IList<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>> _allCfgs;

        /// <summary>
        /// The list of all Use Point CallStatement Nodes
        /// </summary>
        private List<DfaUsePoint<Node, VariableSymbol>> _callUsePoints;

        /// <summary>
        /// Internal Writer.
        /// </summary>
        private TextWriter _writer;
        
        /// <summary>
        /// Visitor to Collect all level 88 symbols.
        /// </summary>
        class Level88SymbolCollector : AbstractSymbolAndTypeVisitor<Dictionary<VariableSymbol, VariableSymbol>, Dictionary<VariableSymbol, VariableSymbol>>
        {
            VariableSymbol ParentSymbol;
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="parentSymbol">The Parent symbol being collected</param>
            public Level88SymbolCollector(VariableSymbol parentSymbol)
            {
                ParentSymbol = parentSymbol;
            }

            public override Dictionary<VariableSymbol, VariableSymbol> VisitSymbol(Symbol s, Dictionary<VariableSymbol, VariableSymbol> symbols)
            {
                s.Type?.Accept(this, symbols);
                return symbols;
            }

            public override Dictionary<VariableSymbol, VariableSymbol> VisitType(Compiler.Types.Type t, Dictionary<VariableSymbol, VariableSymbol> symbols)
            {
                t.TypeComponent?.Accept(this, symbols);
                return symbols;
            }

            public override Dictionary<VariableSymbol, VariableSymbol> VisitVariableSymbol(VariableSymbol s, Dictionary<VariableSymbol, VariableSymbol> symbols)
            {
                if (s.Level == 88)
                {
                    symbols[s] = ParentSymbol;
                    return symbols;
                }

                return VisitSymbol(s, symbols);
            }

            public override Dictionary<VariableSymbol, VariableSymbol> VisitGroupType(Compiler.Types.GroupType t, Dictionary<VariableSymbol, VariableSymbol> symbols)
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
        private Dictionary<VariableSymbol, VariableSymbol> Level88SymbolParentSymbolMap;

        /// <summary>
        /// Callback method when a Use Point is encountered
        /// </summary>
        /// <param name="dfaBuilder">The Dfa Builder</param>
        /// <param name="up">The use Point</param>
        private void OnCallUsePoint(DataFlowGraphBuilder<Node, VariableSymbol> dfaBuilder, DfaUsePoint<Node, VariableSymbol> up)
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
                            foreach (var callStyle in _AlternativeCallList)
                            {
                                if (target != null && target.ToString().Equals(callStyle, StringComparison.OrdinalIgnoreCase))
                                {
                                    _callUsePoints.Add(up);
                                    if (up.Variable.Type.Tag == Compiler.Types.Type.Tags.Group)
                                    {
                                        //Compute the Map of Level88 variable to their parent..
                                        if (Level88SymbolParentSymbolMap == null)
                                        {
                                            Level88SymbolParentSymbolMap = new Dictionary<VariableSymbol, VariableSymbol>();
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
        private void OnDefPoint(DataFlowGraphBuilder<Node, VariableSymbol> dfaBuilder, DfaDefPoint<Node, VariableSymbol> dp)
        {
            VariableSymbol sym = dp.Variable;
            if (sym.Level == 88 && sym.Value != null && Level88SymbolParentSymbolMap != null && Level88SymbolParentSymbolMap.TryGetValue(sym, out var parent))
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
        private void ReportCfgCallUsePoint(ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>> cfg)
        {
            //Check that a semantic data has been associated to this node.
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode != null);
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode.SemanticData != null);
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode.SemanticData.SemanticKind == SemanticKinds.Symbol);

            //The semantic data must be a ProgramSymbol or a FunctionSymbol
            System.Diagnostics.Debug.Assert(cfg.ProgramOrFunctionNode.SemanticData.Kind == Symbol.Kinds.Program ||
                                            cfg.ProgramOrFunctionNode.SemanticData.Kind == Symbol.Kinds.Function);


            _callUsePoints.Clear();
            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfg);
            dfaBuilder.OnUsePointEvent += OnCallUsePoint;
            dfaBuilder.OnDefPointEvent += OnDefPoint;
            dfaBuilder.ComputeUseDefSet();
            //Report Call Use Points.            
            foreach (var up in _callUsePoints)
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
        }

        /// <summary>
        /// Compute a List of definition paths for a UsePoint.
        /// </summary>
        /// <param name="dfaBuilder">The current DataFlow Builder</param>
        /// <param name="up">The Use Point instance</param>
        /// <returns></returns>
        private List<Tuple<string, string>> ComputeUsePointDefPaths(DefaultDataFlowGraphBuilder dfaBuilder, DfaUsePoint<Node, VariableSymbol> up)
        {
            var valueOrigin = ValueOrigin.ComputeFrom(up, dfaBuilder);
            return AsPaths(valueOrigin).ToList();

            IEnumerable<Tuple<string, string>> AsPaths(ValueOrigin vo)
            {
                if (vo == null)
                {
                    //For an uninitialized variable, the ValueOrigin is null.
                    yield return new Tuple<string, string>(null, null);
                    yield break;
                }

                string variable = vo.Variable.FullDotName;
                if (vo.Value != null)
                {
                    System.Diagnostics.Debug.Assert(vo.Origins == null);
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
        private void ReportUsePoint(DfaUsePoint<Node, VariableSymbol> up, Tuple<string, string> path)
        {
            var name = path.Item1;
            string sourceText = up.Instruction.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ').Trim();
            int line = up.Instruction.CodeElement.Line;
            int column = up.Instruction.CodeElement.Column;

            //Remove all unneeded space
            sourceText = Regex.Replace(sourceText, @"\s+", " ");

            _writer.WriteLine($"ObjectName={name};Line={line};Column={column};Path={path.Item2};SourceText={sourceText}");
        }

        /// <summary>
        /// Builds a ZCallPgmReport based on an analyzer.
        /// </summary>
        /// <param name="analyzerId">Unique id of the analyzer to be used to get graphs.</param>
        public ZCallPgmReport(string analyzerId)
        {
            _analyzerId = analyzerId;
            _allCfgs = null;
        }

        /// <summary>
        /// Builds a ZCallPgmReport based on a list of cfg/dfa graphs to be reported.
        /// Used in unit tests.
        /// </summary>
        /// <param name="cfgs">All Control Flow Graphs DFA to be reported for ZCall Pgm</param>
        public ZCallPgmReport(IList<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>> cfgs)
        {
            _analyzerId = null;
            _allCfgs = cfgs;
        }

        public void Report(TextWriter writer, CompilationUnit unit = null)
        {
            //override graphs with results from analyzer
            if (unit != null
                &&
                _analyzerId != null
                &&
                unit.TryGetAnalyzerResult(_analyzerId, out IList<ControlFlowGraph<Node, DfaBasicBlockInfo<VariableSymbol>>> cfgs))
            {
                _allCfgs = cfgs;
            }

            if (_allCfgs != null)
            {
                _callUsePoints = new List<DfaUsePoint<Node, VariableSymbol>>();

                _writer = writer;
                foreach (var cfg in _allCfgs)
                {
                    ReportCfgCallUsePoint(cfg);
                }
            }
        }
    }
}
