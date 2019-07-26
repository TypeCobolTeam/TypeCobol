using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Graph;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Data Flow Graph Builder for Data Flow Analysis.
    /// </summary>
    /// <typeparam name="N">Instruction type</typeparam>
    /// <typeparam name="D">Data info type</typeparam>
    /// <typeparam name="V">Variable generic Type</typeparam>
    public abstract class DataFlowGraphBuilder<N, D, V>
    {
        /// <summary>
        /// The underlying Control Flow Graph to be added with Data Flow Information. 
        /// </summary>
        public ControlFlowGraph<N, DfaBasicBlockInfo<N>> Cfg
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">Underlying Control Flow Graph</param>
        public DataFlowGraphBuilder(ControlFlowGraph<N, DfaBasicBlockInfo<N>> cfg)
        {
            System.Diagnostics.Debug.Assert(cfg != null);
            this.Cfg = cfg;
        }

        /// <summary>
        /// The USE List
        /// </summary>
        public List<DfaUsePoint<N,V>> UseList
        {
            get;
            internal set;
        }

        /// <summary>
        /// The DEF List
        /// </summary>
        public List<DfaDefPoint<N, V>> DefList
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Definitions counter
        /// </summary>
        private int DefCounter = 0;

        /// <summary>
        /// Get Use variables for a given Node
        /// </summary>
        /// <param name="node"></param>
        /// <returns>The USE variable set</returns>
        public abstract HashSet<V> GetUseVariables(N node);

        /// <summary>
        /// Get Def variables for a given Node
        /// </summary>
        /// <param name="node"></param>
        /// <returns>The USE variable set</returns>
        public abstract HashSet<V> GetDefVariables(N node);

        /// <summary>
        /// Compute the Use List
        /// </summary>
        public void ComputeUseList()
        {
            UseList = new List<DfaUsePoint<N, V>>();
            foreach (var block in Cfg.AllBlocks)
            {
                if (block.Instructions != null)
                {
                    if (block.Data == null)
                    {
                        block.Data = new DfaBasicBlockInfo<N>();
                    }
                    foreach (var instr in block.Instructions)
                    {
                        HashSet<V> uses = GetUseVariables(instr);
                        if (uses != null && uses.Count > 0 )
                        {                            
                            if (block.Data.UseCount == 0)
                            {
                                block.Data.UseListFirstIndex = UseList.Count;
                            }                            
                            foreach (var v in uses)
                            {
                                DfaUsePoint<N, V> up = new DfaUsePoint<N, V>();
                                up.Instruction = instr;
                                up.Variable = v;
                                up.BlockIndex = block.Index;
                                UseList.Add(up);
                            }
                            block.Data.UseCount += uses.Count;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Compute the Definition List
        /// </summary>
        public void ComputeDefList()
        {
            DefList = new List<DfaDefPoint<N, V>>();
            foreach (var block in Cfg.AllBlocks)
            {
                if (block.Instructions != null)
                {
                    if (block.Data == null)
                    {
                        block.Data = new DfaBasicBlockInfo<N>();
                    }
                    foreach (var instr in block.Instructions)
                    {
                        HashSet<V> defs = GetDefVariables(instr);
                        if (defs != null && defs.Count > 0)
                        {
                            if (block.Data.DefCount == 0)
                            {
                                block.Data.DefListFirstIndex = DefList.Count;
                            }
                            foreach (var v in defs)
                            {
                                DfaDefPoint<N, V> dp = new DfaDefPoint<N, V>();
                                dp.Index = DefCounter++;
                                dp.Instruction = instr;
                                dp.Variable = v;
                                dp.BlockIndex = block.Index;
                                DefList.Add(dp);
                            }
                            block.Data.DefCount += defs.Count;
                        }
                    }
                }
            }
        }

    }
}
