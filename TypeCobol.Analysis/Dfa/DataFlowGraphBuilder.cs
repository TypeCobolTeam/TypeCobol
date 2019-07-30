using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis.Util;

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
        public ControlFlowGraph<N, DfaBasicBlockInfo<V>> Cfg
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">Underlying Control Flow Graph</param>
        public DataFlowGraphBuilder(ControlFlowGraph<N, DfaBasicBlockInfo<V>> cfg)
        {
            System.Diagnostics.Debug.Assert(cfg != null);
            this.Cfg = cfg;
        }

        /// <summary>
        /// The USE List
        /// </summary>
        public List<DfaUsePoint<N, V>> UseList
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
            lock (this)
            {
                if (UseList != null)
                    return;
                UseList = new List<DfaUsePoint<N, V>>();
                foreach (var block in Cfg.AllBlocks)
                {
                    if (block.Instructions != null)
                    {
                        if (block.Data == null)
                        {
                            block.Data = new DfaBasicBlockInfo<V>();
                        }
                        foreach (var instr in block.Instructions)
                        {
                            HashSet<V> uses = GetUseVariables(instr);
                            if (uses != null && uses.Count > 0)
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
        }

        /// <summary>
        /// Compute the Definition List
        /// </summary>
        public void ComputeDefList()
        {
            lock (this)
            {
                if (DefList != null)
                    return;
                DefList = new List<DfaDefPoint<N, V>>();
                foreach (var block in Cfg.AllBlocks)
                {
                    if (block.Instructions != null)
                    {
                        if (block.Data == null)
                        {
                            block.Data = new DfaBasicBlockInfo<V>();
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

        /// <summary>
        /// Determine if GEN set as been calculated
        /// </summary>
        public bool IsGenSetCalculated
        {
            get;
            internal set;
        }

        /// <summary>
        /// Determine if KILL set as been calculated
        /// </summary>
        public bool IsKillSetCalculated
        {
            get;
            internal set;
        }

        /// <summary>
        /// Build a GEN set for each basic block.
        /// This is the set of data created to identify which data definitions are made within a basic block.
        /// An element of the GEN set represents a definition of a variable made within a basic block which reaches the end of the block.
        /// If there are several definitions of the same variables made in a basic block, then the GEN definition is the last such definition.
        /// </summary>
        public void ComputeGenSet()
        {
            lock (this)
            {
                if (IsGenSetCalculated)
                    return;
                //prerequisites: Compute the DefList
                ComputeDefList();

                foreach (var block in Cfg.AllBlocks)
                {
                    if (block.Instructions != null && block.Data.DefCount > 0)
                    {
                        if (block.Data == null)
                        {
                            block.Data = new DfaBasicBlockInfo<V>();
                        }
                        Dictionary<V, int> variables = new Dictionary<V, int>();
                        //Allocate a temporary gen set.
                        BitSet gen = new BitSet(DefList.Count);
                        for (int i = block.Data.DefListFirstIndex + block.Data.DefCount - 1; i >= block.Data.DefListFirstIndex; i--)
                        {
                            V variable = DefList[i].Variable;
                            if (!variables.ContainsKey(variable))
                            {//This is the last definition of the variable
                                gen.Set(i);
                                variables[variable] = i;
                            }
                        }
                        //Set the gen set of the basic block.
                        block.Data.Gen = gen;
                        block.Data.GenVariableDictionary = variables;
                    }
                }
                IsGenSetCalculated = true;
            }
        }

        /// <summary>
        /// Build the KILL set for each basic block.
        /// The Kill set represents those variables defined outside the basic block which also have definitions inside the block.
        /// Those definitions outside the block are destroyed by the redefinitions inside the block.
        /// </summary>
        public void ComputeKillSet()
        {
            lock (this)
            {
                if (IsKillSetCalculated)
                    return;
                //prerequisites: Compute the GEN set
                ComputeGenSet();
                foreach (var block in Cfg.AllBlocks)
                {
                    if (block.Instructions == null || block.Data == null || block.Data.DefCount == 0)
                        continue;//No Def variables in the block.

                    //Allocate a temporary KILL set.
                    BitSet kill = new BitSet(DefList.Count);

                    //for each definition outside the basic block
                    foreach (var def in DefList)
                    {
                        if (def.BlockIndex == block.Index)
                            continue;//Inside the current block
                        int defIndex;
                        if (block.Data.GenVariableDictionary.TryGetValue(def.Variable, out defIndex))
                        {//The def variable is in the Block's GEN, kill the definition outside the block.
                            kill.Set(def.Index);
                        }
                    }
                    block.Data.Kill = kill;
                }
                IsKillSetCalculated = true;
            }
        }
    }
}
