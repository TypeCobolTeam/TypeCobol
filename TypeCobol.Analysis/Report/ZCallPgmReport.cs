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

namespace TypeCobol.Analysis.Report
{
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
        /// The Cfg builder for FDA Basic Block Information
        /// </summary>
        public ControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> DfaCfgBuilder
        {
            get;
            private set;
        }

        /// <summary>
        /// Internal Writer.
        /// </summary>
        private TextWriter Writer { get; set; }

        /// <summary>
        /// Callback method when a Use Point is encountered
        /// </summary>
        /// <param name="dfaBuilder">The Dfa Builder</param>
        /// <param name="up">The use Point</param>
        void OnCallUsePoint(DataFlowGraphBuilder<Node, DfaBasicBlockInfo<Symbol>, Symbol> dfaBuilder, DfaUsePoint<Node, Symbol> up)
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
                                }
                            }
                        }
                    }
                    break;
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

        private List<Tuple<string, string>> ComputeUsePointDefPaths(TypeCobolDataFlowGraphBuilder dfaBuilder, ProgramSymbol program, DfaUsePoint<Node, Symbol> up)
        {
            List<Tuple<string, string>> paths = new List<Tuple<string, string>>();
            if (up.UseDef == null || up.UseDef.Cardinality() == 0)
            {
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
            {
                int nextDef = -1;
                while ((nextDef = up.UseDef.NextSetBit(nextDef + 1)) >= 0)
                {
                    System.Text.StringBuilder path = new StringBuilder();
                    DfaDefPoint<Node, Symbol> def = dfaBuilder.DefList[nextDef];
                    switch (def.Instruction.CodeElement.Type)
                    {//Only move statement is interresting
                        case CodeElementType.MoveStatement:
                            {//So we can detect only if the source of the move is a literal string.
                                MoveStatement move = (MoveStatement)def.Instruction.CodeElement;
                                switch (move.StatementType)
                                {
                                    case StatementType.MoveSimpleStatement:
                                        {
                                            MoveSimpleStatement simpleMove = (MoveSimpleStatement)move;
                                            if (simpleMove.SendingVariable.IsLiteral && simpleMove.SendingVariable.AlphanumericValue != null)
                                            {//A Literal alphabetic value
                                                string name = simpleMove.SendingVariable.AlphanumericValue.Value.ToString();
                                                path.Append(string.Format(@"{0}<-""{1}""", up.Variable.FullDotName, name));
                                                paths.Add(new Tuple<string, string>(name, path.ToString()));
                                            }
                                            else if (!simpleMove.SendingVariable.IsLiteral &&
                                                simpleMove.SendingVariable.StorageArea != null &&
                                                simpleMove.SendingVariable.StorageArea.SymbolReference != null)
                                            {//An Identifier --> Recurse     
                                             //Find the use point of the identifier in the DefPoint's Block.
                                                SymbolReference symRef = simpleMove.SendingVariable.StorageArea.SymbolReference;
                                                Compiler.Scopes.Scope<VariableSymbol>.MultiSymbols result = program.ResolveReference(symRef, true);
                                                System.Diagnostics.Debug.Assert(result != null);
                                                if (result.Count == 1)
                                                {
                                                    Symbol sendingVariable = result.Symbol; ;
                                                    Graph.BasicBlock<Node, DfaBasicBlockInfo<Symbol>> upBlock = dfaBuilder.Cfg.AllBlocks[def.BlockIndex];
                                                    DfaBasicBlockInfo<Symbol> upBlockData = upBlock.Data;
                                                    for (int i = upBlockData.UseListFirstIndex; i < upBlockData.UseListFirstIndex + upBlockData.UseCount; i++)
                                                    {
                                                        if (dfaBuilder.UseList[i].Instruction == def.Instruction && dfaBuilder.UseList[i].Variable == sendingVariable)
                                                        {   //Now recurse
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
        /// <param name="dfaCfgBuilder">The Cfg builder for FDA Basic Block Information</param>
        /// <param name="filepath">The output file patah</param>
        public ZCallPgmReport(ControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> dfaCfgBuilder, string filepath)
        {
            DfaCfgBuilder = dfaCfgBuilder;
            Filepath = filepath;
            CallUsePoints = new List<DfaUsePoint<Node, Symbol>>();
        }

        public override void Report(TextWriter writer)
        {
            this.Writer = writer;
            DfaCfgBuilder.TraverseAllCfgBuilders(CfgBuilderCallback);            
        }
    }
}
