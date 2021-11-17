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
            /// We browse the the graph represented using the Call Relation, then we found during the visit cycles.
            /// In the same time we compute the call chain.
            /// </summary>
            internal void TransitiveClosure()
            {
                // The dfs stack of all procedures in active consideration.
                Stack<Procedure> S = new Stack<Procedure>();
                // The dictionary N is used for three purposes
                // 1) To record unmarked procedures (N[p] == 0).
                // 2) To associate a positive number value with procedures that are active consideration (0 < N[p] < Infnity).
                // 3) To mark Infinity Procedures whose cycles have already been found
                Dictionary<Procedure, int> N = new Dictionary<Procedure, int>();

                // For each procedure in the relation's domain
                foreach (var a in Domain)
                { // For each procedure 'a' not yet considered, treat it.
                    var na = Visited(a);
                    if (na == 0)
                        dfs(a);
                }

                // Depth First Search Traversal.
                void dfs(Procedure a)
                {
                    S.Push(a);
                    int d = S.Count;
                    N[a] = d;
                    var fa = GetPerformCallChain(a);
                    if (fa.Procedures != null)
                    {   //For all procedure b called by a
                        foreach (var b in fa.Procedures.ToArray())
                        {
                            int nb = Visited(b);
                            if (nb == 0)
                                dfs(b);//Procedure not visited yet
                            int _na = Visited(a);
                            int _nb = Visited(b);
                            N[a] = Math.Min(_na, _nb); // N[b] < N[a] : we find a cycle
                            var fb = GetPerformCallChain(b);
                            // f(a) = f(a) U f(b)
                            // We update the call chain
                            UpdatePerformCallChain(fb, fa);
                        }
                    }
                    var na = Visited(a);
                    if (na == d)
                    {   // 'a' is an entry point of a calculated component, that is to say the start procedure of a cycle.
                        // because if na != d then 'a' has been included in another cycle whose root is not 'a'.
                        do
                        {
                            N[S.Peek()] = Int32.MaxValue; // The Node(The procedure) is terminated
                            if (TryGetValue(S.Peek(), out var top))
                            {
                                UpdatePerformCallChain(fa, top);
                            }
                            else
                            {
                                Add(S.Peek(), new PerformCallChain(
                                    fa.PerformCaller, new List<Node>(fa.PerformCallees), new HashSet<Procedure>(fa.Procedures)));
                            }
                        } while (S.Count > 0 && S.Pop() != a);
                    }
                }
                // Get the visited mark of the procedure a
                int Visited(Procedure a)
                {
                    if (N.TryGetValue(a, out var na))
                        return na;
                    return 0;
                }
                // Get the PerformCallChain value of the procedure a
                PerformCallChain GetPerformCallChain(Procedure a)
                {
                    if (TryGetValue(a, out var v))
                        return v;
                    else
                        return new PerformCallChain(null, new List<Node>(), new HashSet<Procedure>());
                }
                // Update PerformCallChain value of procedure 'to' when prodecure 'from' call
                // procedure 'to' this the transitive aspect: all procedures called by 'from' are
                // also called by 'to'
                void UpdatePerformCallChain(PerformCallChain from, PerformCallChain to)
                {
                    foreach (var p in from.PerformCallees)
                    {
                        if (!to.PerformCallees.Contains(p))
                        {
                            to.PerformCallees.Add(p);
                        }
                    }
                    foreach (var p in from.Procedures)
                    {
                        to.Procedures.Add(p);
                    }
                }
            }
        }
    }
}

