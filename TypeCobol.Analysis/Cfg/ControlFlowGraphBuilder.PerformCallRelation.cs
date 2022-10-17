using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Represent the perform call chain of a PerformProcedure caller
        /// </summary>
        private class PerformCallChain
        {
            /// <summary>
            /// Source Perform Procedure instruction
            /// </summary>
            internal PerformProcedure PerformCaller;
            /// <summary>
            /// List of PerformProcedure (Nodes) that form the call chain from the 'PerformCaller'
            /// </summary>
            internal List<Node> PerformCallees;
            /// <summary>
            /// All target procedures
            /// </summary>
            internal HashSet<Procedure> Procedures;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="performCaller"></param>
            internal PerformCallChain(PerformProcedure performCaller)
            {
                this.PerformCaller = performCaller;
                this.PerformCallees = new List<Node>();
                this.Procedures = new HashSet<Procedure>();
            }
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="performCaller"></param>
            /// <param name="performCallees"></param>
            /// <param name="procedures"></param>
            internal PerformCallChain(PerformProcedure performCaller, List<Node> performCallees, HashSet<Procedure> procedures)
            {
                this.PerformCaller = performCaller;
                this.PerformCallees = performCallees;
                this.Procedures = procedures;
            }
        }

        /// <summary>
        /// Representation of Perform Call Relation. 
        /// It is a dictionary which gives for each Procedure its Perform Call chain.
        /// </summary>
        private class PerformCallRelation : Dictionary<Procedure, PerformCallChain>
        {
            /// <summary>
            /// The Relation's domain
            /// </summary>
            internal HashSet<Procedure> Domain;

            internal PerformCallRelation()
            {
                Domain = new HashSet<Procedure>();
            }
            /// <summary>
            /// Add a PERFORM CALL relation : Caller => Callee (Procedure)
            /// Example of code:
            ///        main.
            ///             perform a
            ///             goback
            ///             .
            ///        a.
            ///             perform b
            ///        .
            /// The relation is: a => b
            /// </summary>
            /// <param name="performCaller">PERFORM procedure caller, source instruction: in the example (perform a)</param>
            /// <param name="caller">Caller procedure: in the example 'a'</param>/// 
            /// <param name="performCallee">The PERFORM procedure called, target instruction: in the example (perform b)</param>
            /// <param name="callee">The called procedure: in the example 'b' </param>
            internal void AddToRelation(PerformProcedure performCaller, Procedure caller, PerformProcedure performCallee, Procedure callee)
            {
                if (!TryGetValue(caller, out var performCallChain))
                {
                    performCallChain = new PerformCallChain(performCaller);
                    Add(caller, performCallChain);
                }
                performCallChain.Procedures.Add(callee);
                if (!performCallChain.PerformCallees.Contains(performCallee))
                {
                    performCallChain.PerformCallees.Add(performCallee);
                }
                Domain.Add(caller);
                Domain.Add(callee);
            }

            /// <summary>
            /// Compute the transitive closure of the PERFORM CALL relation.
            /// We browse the graph represented using the Call Relation, then we found during the visit cycles.
            /// In the same time we compute the call chain.
            /// </summary>
            internal void TransitiveClosure()
            {
                // The dfs stack of all procedures in active consideration.
                Stack<Procedure> dfsStack = new Stack<Procedure>();
                // The dictionary dfsMark is used for three purposes
                // 1) To record unmarked procedures (dfsMark[p] == 0).
                // 2) To associate a positive number value with procedures that are active consideration (0 < dfsMark[p] < Infnity).
                // 3) To mark Infinity Procedures whose cycles have already been found
                Dictionary<Procedure, int> dfsMark = new Dictionary<Procedure, int>();

                // For each procedure in the relation's domain
                foreach (var procedure in Domain)
                { // For each procedure not yet visted, treat it.
                    var proc_visitMark = Visited(procedure);
                    if (proc_visitMark == 0)
                        dfs(procedure);
                }

                // Depth First Search Traversal.
                void dfs(Procedure proc_a)
                {
                    dfsStack.Push(proc_a);
                    int depth = dfsStack.Count;
                    dfsMark[proc_a] = depth;
                    var proca_callChain = GetPerformCallChain(proc_a);
                    if (proca_callChain.Procedures != null)
                    {   //For all procedure b called by a
                        foreach (var proc_b in proca_callChain.Procedures.ToArray())
                        {
                            int procb_VisitMark = Visited(proc_b);
                            if (procb_VisitMark == 0)
                            {
                                dfs(proc_b);//Procedure not visited yet
                                procb_VisitMark = Visited(proc_b); ;
                            }
                            int proca_VisitMark = Visited(proc_a);
                            dfsMark[proc_a] = Math.Min(proca_VisitMark, procb_VisitMark); // N[b] < N[a] : we find a cycle
                            var procb_callChain = GetPerformCallChain(proc_b);
                            // proca_callChain = proca_callChain U procb_callChain
                            // We update the call chain
                            UpdatePerformCallChain(procb_callChain, proca_callChain);
                        }
                    }
                    var proca_visitMark = Visited(proc_a);
                    if (proca_visitMark == depth)
                    {   // 'a' is an entry point of a calculated component, that is to say the start procedure of a cycle.
                        // because if na != d then 'a' has been included in another cycle whose root is not 'a'.
                        do
                        {
                            dfsMark[dfsStack.Peek()] = Int32.MaxValue; // The Node(The procedure) is terminated
                            if (TryGetValue(dfsStack.Peek(), out var top))
                            {
                                UpdatePerformCallChain(proca_callChain, top);
                            }
                            else
                            {
                                Add(dfsStack.Peek(), new PerformCallChain(
                                    proca_callChain.PerformCaller, new List<Node>(proca_callChain.PerformCallees), new HashSet<Procedure>(proca_callChain.Procedures)));
                            }
                        } while (dfsStack.Count > 0 && dfsStack.Pop() != proc_a);
                    }
                }
                // Get the visited mark of the procedure
                int Visited(Procedure procedure)
                {
                    if (dfsMark.TryGetValue(procedure, out var visitMark))
                        return visitMark;
                    return 0;
                }
                // Get the PerformCallChain value of the procedure a
                PerformCallChain GetPerformCallChain(Procedure procedure)
                {
                    if (TryGetValue(procedure, out var callChain))
                        return callChain;
                    else
                        return new PerformCallChain(null, new List<Node>(), new HashSet<Procedure>());
                }
                // Update PerformCallChain value of procedure 'to' when prodecure 'from' call
                // procedure 'to' this the transitive aspect: all procedures called by 'from' are
                // also called by 'to'
                void UpdatePerformCallChain(PerformCallChain from, PerformCallChain to)
                {
                    foreach (var perform in from.PerformCallees)
                    {
                        if (!to.PerformCallees.Contains(perform))
                        {
                            to.PerformCallees.Add(perform);
                        }
                    }
                    foreach (var procedure in from.Procedures)
                    {
                        to.Procedures.Add(procedure);
                    }
                }
            }
        }
    }
}

