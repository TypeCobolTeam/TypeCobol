using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Utilities
{
    /// <summary>
    /// A Class used to match the target node of a completion.
    /// </summary>
    public class CompletionNodeMatcher : TypeCobol.Compiler.CodeElements.AbstractAstVisitor
    {
        /// <summary>
        /// Target completion mode.
        /// </summary>
        public enum CompletionMode
        {
            /// <summary>
            /// Completion on PERFORM Token
            /// </summary>
            Perform,
            Call,
            Type,
        }

        /// <summary>
        /// The Completion mode.
        /// </summary>
        public CompletionMode Mode
        {
            get;
            private set;
        }

        /// <summary>
        /// The Matching completion token.
        /// </summary>
        public TypeCobol.Compiler.Scanner.Token MatchingToken
        {
            get;
            private set;
        }

        /// <summary>
        /// Procedure Division counter, because a FunctionDeclaration can have a procedure
        /// Division;
        /// </summary>
        int ProcedureDivisionCount
        {
            get;
            set;
        }

        /// <summary>
        /// Getter if we are in a Procedure Division.
        /// </summary>
        private bool IsInProcedureDivision
        {
            get
            {
                return ProcedureDivisionCount > 0;
            }            
        }

        bool m_IsStopVisitingChildren;
        public override bool IsStopVisitingChildren
        {
            get
            {
                return m_IsStopVisitingChildren;
            }
        }


        /// <summary>
        /// The Node that matches.
        /// </summary>
        public TypeCobol.Compiler.Nodes.Node MatchingNode
        {
            get;
            protected set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mode">Completion mode</param>
        /// <param name="matchingToken">Matching token</param>
        public CompletionNodeMatcher(CompletionMode mode, Compiler.Scanner.Token matchingToken)
        {
            Mode = mode;
            MatchingToken = matchingToken;
            m_IsStopVisitingChildren = false;
        }

        public override bool IsSymbolInformationForTokensEnabled
        {
            get
            {
                return false;
            }
        }

        /// <summary>
        /// Begin Node acceptor.
        /// </summary>
        /// <param name="node">The node to accept</param>
        /// <returns>true if the node can be visited, false otherwise.</returns>
        public override bool BeginNode(Node node)
        {
            switch(Mode)
            {
                case CompletionMode.Perform: {
                    bool canVisit = this.MatchingNode  == null && (node is SourceFile || 
                        node is TypeCobol.Compiler.CodeModel.Program || 
                        node is ProcedureDivision || 
                        IsInProcedureDivision);
                    if (canVisit && node is ProcedureDivision)
                    {
                        ProcedureDivisionCount++;
                    }
                    return canVisit;
                }
                case CompletionMode.Call:
                case CompletionMode.Type:
                    {
                        if(node.CodeElement !=null)
                        {
                            var consumedTokens = node.CodeElement.ConsumedTokens;
                            if (consumedTokens != null && consumedTokens.Any(t => t.Line <= MatchingToken.Line && t.Column <= MatchingToken.Column))
                            {
                                MatchingNode = node;
                            }
                            else if (!consumedTokens.Any(t => t.Line <= MatchingToken.Line && t.Column <= MatchingToken.Column))
                                return false;
                        }
                        return true;
                    }
                default:
                    return true;
            }            
        }

        /// <summary>
        /// Ending a node
        /// </summary>
        /// <param name="node"></param>
        public override void EndNode(Node node)
        {
            switch (Mode)
            {
                case CompletionMode.Perform:
                    {
                        if (node is ProcedureDivision)
                        {
                            ProcedureDivisionCount--;                            
                        }
                    }
                    break;
                case CompletionMode.Call:
                    break;
                default:
                    break;
            }
        }

        /// <summary>
        /// Checks if the given node match the token
        /// </summary>
        /// <param name="node">The Node to check</param>
        /// <returns>true if the nod ematches the token, fals eotherwise.</returns>
        private bool CheckMatchingTokenNode(Node node)
        {
            if (node.CodeElement != null && node.CodeElement.ConsumedTokens != null)
            {
                bool bResult = (node.CodeElement.ConsumedTokens.Contains(MatchingToken));
                if (bResult)
                {
                    this.MatchingNode = node;
                    m_IsStopVisitingChildren = true;
                }
                return bResult;
            }
            return false;
        }

        public override bool Visit(Perform perform)
        {
            switch (Mode)
            {
                case CompletionMode.Perform:
                    return !CheckMatchingTokenNode(perform);
                default:
                    return true;
            }
        }

        public override bool Visit(PerformProcedure performProcedure)
        {
            switch (Mode)
            {
                case CompletionMode.Perform:
                    return !CheckMatchingTokenNode(performProcedure);
                default:
                    return true;
            }
        }
    }
}
