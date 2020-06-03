using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Report;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.CodeElements;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Scopes;

namespace TypeCobol.Analysis.Report
{
    /// <summary>
    /// ZCallPgmReport class using DFA.
    /// </summary>
    public class ZCallPgmReport : AbstractReport
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
        /// The Cfg builder for DFA Basic Block Information
        /// </summary>
        public CfgDfaContext CfgDfaCtx
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
        /// Callback on a traverse of a Cfg Builder.
        /// </summary>
        /// <param name="cfgBuilder">The Control Flow Graph Builder instance</param>
        /// <returns>true</returns>
        private bool CfgBuilderCallback(ControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> cfgBuilder)
        {
            //Check that a semantic data has been associated to this node.
            System.Diagnostics.Debug.Assert(cfgBuilder.Cfg.ProgramNode != null);
            System.Diagnostics.Debug.Assert(cfgBuilder.Cfg.ProgramNode.SemanticData != null);
            System.Diagnostics.Debug.Assert(cfgBuilder.Cfg.ProgramNode.SemanticData.SemanticKind == SemanticKinds.Symbol);

            //The semantic data must be a ProgramSymbol or a FunctionSymbol
            System.Diagnostics.Debug.Assert(((Symbol)cfgBuilder.Cfg.ProgramNode.SemanticData).Kind == Symbol.Kinds.Program ||
                                            ((Symbol)cfgBuilder.Cfg.ProgramNode.SemanticData).Kind == Symbol.Kinds.Function);

            ProgramSymbol program = (ProgramSymbol)cfgBuilder.Cfg.ProgramNode.SemanticData;

            CallUsePoints.Clear();
            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(cfgBuilder.Cfg);
            dfaBuilder.OnUsePointEvent += OnCallUsePoint;
            dfaBuilder.OnDefPointEvent += OnDefPoint;
            dfaBuilder.ComputeUseDefSet();
            //Report Call Use Points.            
            foreach (DfaUsePoint<Node, Symbol> up in CallUsePoints)
            {
                List < Tuple<string, string> > defPaths = ComputeUsePointDefPaths(dfaBuilder, program, up);
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
        /// The Algorithm works like that:
        /// (1) If the UsePoint has no associated UseDef set then tries to see if variable has an initial value.
        /// (2) The UsePoint has a UseDef set.
        /// (2.1) For each Definition Point of the UseDef set
        /// (2.1.1) Only take in account those DefinitionPoints associated to a MOVE or a SET instruction.
        /// (2.1.1.1.1)If the MoveStatement is a SimpleMoveStatement and the sending variable is a Literal alphabetic value
        ///     then append a definition path with this value.
        /// (2.1.1.1.2)If the MoveStatement is a SimpleMoveStatement and the sending variable is an Identifier.
        ///             then (2.1.1.1.2.1) Resolve the sending identifier in the current program
        ///             (2.1.1.1.2.2) The sending identifier variable is resolved -> look for its UsePoint instruction in the definition block
        ///             (2.1.1.1.2.3) The UsePoint of the sending identifier is found then recurse to compute its definition paths
        /// (2.1.1.2) a Set Statement
        ///     (2.1.1.2.1) The Set instruction is Condition variable set
        ///     (2.1.1.2.2) The values are the values of the level 88 variable
        /// </summary>
        /// <param name="dfaBuilder">The current DataFlow Builder</param>
        /// <param name="program">The Current Program</param>
        /// <param name="up">The Use Point instance</param>
        /// <returns>a List of Tuple(VariablName, Definition Path)</VariablName></returns>
        private List<Tuple<string, string>> ComputeUsePointDefPaths(TypeCobolDataFlowGraphBuilder dfaBuilder, ProgramSymbol program, DfaUsePoint<Node, Symbol> up)
        {
            List<Tuple<string, string>> paths = new List<Tuple<string, string>>();
            if (up.UseDef == null || up.UseDef.Cardinality() == 0)
            {//(1)
             //No Definitions ==> try to see if the variable has an Initial Value
                VariableSymbol varSym = up.Variable as VariableSymbol;
                if (varSym.Value != null && varSym.Value is TypeCobol.Compiler.CodeElements.Value)
                {
                    TypeCobol.Compiler.CodeElements.Value value = (TypeCobol.Compiler.CodeElements.Value)varSym.Value;
                    string name = value.ToString();
                    paths.Add(new Tuple<string, string>(name, string.Format(@"{0}<-""{1}""", up.Variable.FullDotName, name)));
                }
            }
            else
            {//(2)
                int nextDef = -1;
                while ((nextDef = up.UseDef.NextSetBit(nextDef + 1)) >= 0)
                {//(2.1)
                    System.Text.StringBuilder path = new StringBuilder();
                    DfaDefPoint<Node, Symbol> def = dfaBuilder.DefList[nextDef];
                    switch (def.Instruction.CodeElement.Type)
                    {//(2.1.1)Only move or set statements are interresting
                        case CodeElementType.MoveStatement:
                            {//(2.1.1.1)So we can detect only if the source of the move is a literal string or an identifier.
                                MoveStatement move = (MoveStatement)def.Instruction.CodeElement;
                                switch (move.StatementType)
                                {
                                    case StatementType.MoveSimpleStatement:
                                        {
                                            MoveSimpleStatement simpleMove = (MoveSimpleStatement)move;
                                            if (simpleMove.SendingVariable.IsLiteral && simpleMove.SendingVariable.AlphanumericValue != null)
                                            {//(2.1.1.1.1)A Literal alphabetic value
                                                string name = simpleMove.SendingVariable.AlphanumericValue.Value.ToString();
                                                path.Append(string.Format(@"{0}<-""{1}""", up.Variable.FullDotName, name));
                                                paths.Add(new Tuple<string, string>(name, path.ToString()));
                                            }
                                            else if (!simpleMove.SendingVariable.IsLiteral &&
                                                simpleMove.SendingVariable.StorageArea != null &&
                                                simpleMove.SendingVariable.StorageArea.SymbolReference != null)
                                            {//(2.1.1.1.2)An Identifier --> Recurse     
                                             //Find the use point of the sending identifier in the DefPoint's Block.
                                             //(2.1.1.1.2.1) Resolve the identifier variable
                                                SymbolReference symRef = simpleMove.SendingVariable.StorageArea.SymbolReference;
                                                Container<VariableSymbol>.Entry result = program.ResolveReference(symRef, true);
                                                System.Diagnostics.Debug.Assert(result != null);
                                                if (result.Count == 1)
                                                {//(2.1.1.1.2.2) The sending identifier variable is resolved -> look for its UsePoint instruction in the definition block
                                                    Symbol sendingVariable = result.Symbol; ;
                                                    Graph.BasicBlock<Node, DfaBasicBlockInfo<Symbol>> upBlock = dfaBuilder.Cfg.AllBlocks[def.BlockIndex];
                                                    DfaBasicBlockInfo<Symbol> upBlockData = upBlock.Data;
                                                    for (int i = upBlockData.UseListFirstIndex; i < upBlockData.UseListFirstIndex + upBlockData.UseCount; i++)
                                                    {
                                                        if (dfaBuilder.UseList[i].Instruction == def.Instruction && dfaBuilder.UseList[i].Variable == sendingVariable)
                                                        {   //(2.1.1.1.2.3) The UsePoint of the sending identifier is found then recurse compute its definition paths
                                                            StringBuilder newSB = new StringBuilder();
                                                            List<Tuple<string, string>> newPtahs = ComputeUsePointDefPaths(dfaBuilder, program, dfaBuilder.UseList[i]);
                                                            foreach (var item in newPtahs)
                                                            {
                                                                paths.Add(new Tuple<string, string>(item.Item1, string.Format(@"{0}<-{1}", up.Variable.FullDotName, item.Item2)));
                                                            }
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        break;
                                    case StatementType.MoveCorrespondingStatement:
                                        {
                                        }
                                        break;
                                }
                            }
                            break;
                        case CodeElementType.SetStatement:
                            {//(2.1.1.2) a Set Statement
                                SetStatement set = (SetStatement)def.Instruction.CodeElement;
                                switch (set.StatementType)
                                {
                                    case StatementType.SetStatementForConditions://This a level 88 variable set.
                                        {   //(2.1.1.2.1) The Set instruction is Condition variable set
                                            //Use reflection to get sending value, because the class SetStatementForConditions is Internal to TypeCobol
                                            System.Reflection.PropertyInfo info = set.GetType().GetProperty("SendingValue");
                                            if (info != null)
                                            {
                                                object ovalue = info.GetValue(set);
                                                if (ovalue != null)
                                                { 
                                                    BooleanValue bvalue = (BooleanValue)ovalue;
                                                    //Only set if the SendingValue is true: It should always be the case in COBOL, but not in TypeCobol
                                                    VariableSymbol parent = (VariableSymbol)def.Variable;
                                                    System.Diagnostics.Debug.Assert(def.UserData != null);
                                                    VariableSymbol sym = (VariableSymbol)def.UserData;
                                                    if (sym.Value != null && sym.Value is TypeCobol.Compiler.CodeElements.Value[])
                                                    {//(2.1.1.2.2) The values are the values of the level 88 variable
                                                        TypeCobol.Compiler.CodeElements.Value[] values = (TypeCobol.Compiler.CodeElements.Value[])sym.Value;
                                                        foreach (var value in values)
                                                        {
                                                            path.Append(string.Format(@"{0}<-""{1}""", sym.FullDotName, "true"));
                                                            paths.Add(new Tuple<string, string>(value.AlphanumericValue.ToString(), path.ToString()));
                                                        }
                                                    }                                                    
                                                }
                                            }
                                        }
                                        break;
                                }
                            }
                            break;
                    }
                }
            }
            return paths;
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
        /// Constructor
        /// </summary>
        /// <param name="cfgDfaCtx">The Cfg/Dfa context for building DFA Basic Block Information</param>
        /// <param name="filepath">The output file patah</param>
        public ZCallPgmReport(CfgDfaContext cfgDfaCtx, string filepath)
        {
            CfgDfaCtx = cfgDfaCtx;
            Filepath = filepath;
            CallUsePoints = new List<DfaUsePoint<Node, Symbol>>();
        }

        public override void Report(TextWriter writer)
        {
            this.Writer = writer;
            CfgDfaCtx.CfgDfaBuilder.TraverseAllCfgBuilders(CfgBuilderCallback);            
        }
    }
}
