using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis.Util;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Data Flow Graph Builder for Data Flow Analysis.
    /// </summary>
    /// <typeparam name="N">Instruction type</typeparam>
    /// <typeparam name="V">Variable generic Type</typeparam>
    public abstract class DataFlowGraphBuilder<N, V>
    {
        /// <summary>
        /// The underlying Control Flow Graph to be added with Data Flow Information. 
        /// </summary>
        public ControlFlowGraph<N, DfaBasicBlockInfo<V>> Cfg { get; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="cfg">Underlying Control Flow Graph</param>
        protected DataFlowGraphBuilder(ControlFlowGraph<N, DfaBasicBlockInfo<V>> cfg)
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
            private set;
        }

        /// <summary>
        /// The DEF List
        /// </summary>
        public List<DfaDefPoint<N, V>> DefList
        {
            get;
            private set;
        }

        /// <summary>
        /// The dictionary which gives for each Variable defined its Def set.
        /// That is to say all definitions of this variable.
        /// This Dictionary id built during the GEN set computation.
        /// </summary>
        public Dictionary<V, BitSet> VariableDefMap
        {
            get;
            private set;
        }

        /// <summary>
        /// The Definitions counter
        /// </summary>
        private int _defCounter;

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
        /// On Use Point Delegate
        /// </summary>
        /// <param name="dfaBuilder">The Dfa Builder in which the Use Point is seen</param>
        /// <param name="usePoint">The Use point</param>
        public delegate void OnUsePoint(DataFlowGraphBuilder<N, V> dfaBuilder, DfaUsePoint<N, V> usePoint);

        /// <summary>
        /// On Use Point Event
        /// </summary>
        public event OnUsePoint OnUsePointEvent;

        /// <summary>
        /// On Def Point Delegate
        /// </summary>
        /// <param name="dfaBuilder">The Dfa Builder in which the Def Point is seen</param>
        /// <param name="defPoint">The Def Point</param>
        public delegate void OnDefPoint(DataFlowGraphBuilder<N, V> dfaBuilder, DfaDefPoint<N, V> defPoint);

        /// <summary>
        /// On Def Point Event
        /// </summary>
        public event OnDefPoint OnDefPointEvent;

        /// <summary>
        /// Compute the Use List
        /// </summary>
        public void ComputeUseList()
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
                    int instrIndex = 0;
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
                                up.InstructionIndex = instrIndex;
                                up.Variable = v;
                                up.BlockIndex = block.Index;
                                UseList.Add(up);

                                //Dispatch to Listeners
                                OnUsePointEvent?.Invoke(this, up);
                            }
                            block.Data.UseCount += uses.Count;
                        }
                        instrIndex++;
                    }
                }
            }
        }

        /// <summary>
        /// Compute the Definition List
        /// </summary>
        public void ComputeDefList()
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
                    int instrIndex = 0;
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
                                dp.Index = _defCounter++;
                                dp.Instruction = instr;
                                dp.InstructionIndex = instrIndex;
                                dp.Variable = v;
                                dp.BlockIndex = block.Index;
                                DefList.Add(dp);

                                //Dispatch to Listeners
                                OnDefPointEvent?.Invoke(this, dp);
                            }
                            block.Data.DefCount += defs.Count;
                        }
                        instrIndex++;
                    }
                }
            }
        }

        /// <summary>
        /// Determine if GEN set has been calculated
        /// </summary>
        public bool IsGenSetCalculated
        {
            get;
            private set;
        }

        /// <summary>
        /// Determine if KILL set as been calculated
        /// </summary>
        public bool IsKillSetCalculated
        {
            get;
            private set;
        }

        /// <summary>
        /// Build a GEN set for each basic block.
        /// This is the set of data created to identify which data definitions are made within a basic block.
        /// An element of the GEN set represents a definition of a variable made within a basic block which reaches the end of the block.
        /// If there are several definitions of the same variables made in a basic block, then the GEN definition is the last such definition.
        /// </summary>
        public void ComputeGenSet()
        {
            if (IsGenSetCalculated)
                return;
            //prerequisites: Compute the DefList
            ComputeDefList();

            VariableDefMap = new Dictionary<V, BitSet>();

            foreach (var block in Cfg.AllBlocks)
            {
                if (block.Instructions != null && block.Data.DefCount > 0)
                {
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
                        if (!VariableDefMap.TryGetValue(variable, out var vdefs))
                        {
                            vdefs = new BitSet(DefList.Count);
                            VariableDefMap[variable] = vdefs;
                        }
                        vdefs.Set(i);
                    }
                    //Set the gen set of the basic block.
                    block.Data.Gen = gen;
                    block.Data.GenVariableDictionary = variables;
                }
            }
            IsGenSetCalculated = true;
        }

        /// <summary>
        /// Build the KILL set for each basic block.
        /// The Kill set represents those variables defined outside the basic block which also have definitions inside the block.
        /// Those definitions outside the block are destroyed by the redefinitions inside the block.
        /// </summary>
        public void ComputeKillSet()
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

                    if (block.Data.GenVariableDictionary.ContainsKey(def.Variable))
                    {//The def variable is in the Block's GEN, kill the definition outside the block.
                        kill.Set(def.Index);
                    }
                }
                block.Data.Kill = kill;
            }
            IsKillSetCalculated = true;
        }

        /// <summary>
        /// Determine if InOuts sets as been calculated
        /// </summary>
        public bool IsInOutSetCalculated
        {
            get;
            private set;
        }

        /// <summary>
        /// Initialize the Computation of IN and OUT Sets
        /// </summary>
        private void InitializeInOutSetsComputation()
        {
            //prerequisites: Compute the KILL sets which will force GEN sets calculation.
            ComputeKillSet();

            //Initialization
            foreach (var block in Cfg.AllBlocks)
            {
                if (block.Data == null)
                {
                    block.Data = new DfaBasicBlockInfo<V>(DefList.Count);
                }
                else
                {
                    System.Diagnostics.Debug.Assert(block.Data.In == null);
                    block.Data.In = new BitSet(DefList.Count);
                    System.Diagnostics.Debug.Assert(block.Data.Out == null);
                    block.Data.Out = new BitSet(DefList.Count);
                }
                if (block.Data.Gen != null)
                { //Initialize OUT(b) = GEN(b)
                    block.Data.Out.Or(block.Data.Gen);
                }
            }
        }

        /// <summary>
        /// Build IN and OUT sets for each basic Block.
        /// IN sets represent data which reach a point just before the first instruction of a block.
        /// OUT sets represent definitions which reach a point just after the last instruction of a basic block.
        /// The equations relating IN and OUT sets are:
        /// 
        ///     OUT(b) = (IN(b) - KILL(b)) U GEN(b)
        ///     IN(b) = U OUT(p)  where p belongs to Predecessor(b)
        /// </summary>
        public void ComputeInOutSet()
        {
            if (IsInOutSetCalculated)
                return;

            //Initialization
            InitializeInOutSetsComputation();

            //Compute Predecessors
            this.Cfg.SetupPredecessorEdges();

            BitSet newIn = new BitSet(DefList.Count);
            bool bChange = true;
            while (bChange)
            {
                bChange = false;
                foreach (var b in Cfg.AllBlocks)
                {
                    // IN(b) = U OUT(p)  where p belongs to Predecessor(b)
                    foreach (var p in b.PredecessorEdges)
                    {
                        var a = Cfg.PredecessorEdges[p];
                        newIn.Or(a.Data.Out);
                    }

                    if (!newIn.Equals(b.Data.In))
                    {
                        bChange = true;
                        b.Data.In.Copy(newIn);
                    }
                    newIn.Clear();

                    //OUT(b) = (IN(b) - KILL(b)) U GEN(b)
                    BitSet live = b.Data.Kill != null ? b.Data.In.Difference(b.Data.Kill) : b.Data.In.Clone();
                    if (b.Data.Gen != null)
                    {
                        live.Or(b.Data.Gen);
                    }
                    b.Data.Out = live;
                }
            }

            IsInOutSetCalculated = true;
        }

        /// <summary>
        /// Determine if UseDef set has been calculated
        /// </summary>
        public bool IsUseDefSetCalculated
        {
            get;
            private set;
        }

        /// <summary>
        /// Build the USE-DEF chain for each basic block
        /// The UseDef set is a set of information which tells where particular uses of a variable within a basic block 
        /// are defined.
        /// </summary>
        public void ComputeUseDefSet()
        {
            if (IsUseDefSetCalculated)
                return;

            //prerequisites: UseList and Compute the IN-OUT sets
            ComputeUseList();
            ComputeInOutSet();

            foreach (var b in Cfg.AllBlocks)
            {
                if (b.Data != null && b.Data.UseCount > 0)
                {   //For each USE point in the block
                    for (int i = b.Data.UseListFirstIndex; i < b.Data.UseListFirstIndex + b.Data.UseCount; i++)
                    {
                        DfaUsePoint<N, V> up = UseList[i];
                        //Check if there is a definition of the Variable in this block.
                        bool bFound = false;
                        for (int j = b.Data.DefListFirstIndex + b.Data.DefCount - 1; j >= b.Data.DefListFirstIndex && !bFound; j--)
                        {
                            DfaDefPoint<N, V> dp = DefList[j];
                            if (up.Variable.Equals(dp.Variable) && dp.InstructionIndex < up.InstructionIndex)
                            {//We got the definition of the variable preceding its usage
                                up.UseDef = new BitSet(DefList.Count);
                                up.UseDef.Set(dp.Index);
                                bFound = true;
                            }
                        }
                        if (!bFound)
                        {//No definition was found in the basic block for the use, 
                         //so the definitions of this use in the IN set form the UseDef?
                            if (VariableDefMap.TryGetValue(up.Variable, out var vdefs))
                            {
                                BitSet vInDefs = vdefs.Intersection(b.Data.In);
                                if (vInDefs.Cardinality() > 0)
                                {
                                    up.UseDef = vInDefs;
                                }
                            }
                        }
                    }
                }
            }
            IsUseDefSetCalculated = true;
        }
    }
}
