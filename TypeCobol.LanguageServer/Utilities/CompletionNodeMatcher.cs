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
            Perform = 0x01 << 0
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

        private bool IsInProcedureDivision
        {
            get;
            set;
        }

        public override bool IsStopVisitingChildren
        {
            get
            {
                return false;
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
        public CompletionNodeMatcher(CompletionMode mode, TypeCobol.Compiler.Scanner.Token matchingToken)
        {
            Mode = mode;
            MatchingToken = matchingToken;
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
                        IsInProcedureDivision = true;
                    }
                    return canVisit;
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
                            IsInProcedureDivision = false;
                        }
                    }
                    break;
                default:
                    break;
            }
        }

        public override bool Visit(Perform perform)
        {
            switch (Mode)
            {
                case CompletionMode.Perform:
                    if (perform.CodeElement != null && perform.CodeElement.ConsumedTokens != null)
                    {
                        bool bResult = (perform.CodeElement.ConsumedTokens.Contains(MatchingToken));
                        if (bResult)
                            this.MatchingNode = perform;
                        return !bResult;
                    }
                    return true;
                default:
                    return true;
            }
        }

        public override bool Visit(PerformProcedure performProcedure)
        {
            switch (Mode)
            {
                case CompletionMode.Perform:
                    if (performProcedure.CodeElement != null && performProcedure.CodeElement.ConsumedTokens != null)
                    {
                        bool bResult = (performProcedure.CodeElement.ConsumedTokens.Contains(MatchingToken));
                        if (bResult)
                            this.MatchingNode = performProcedure;
                        return !bResult;
                    }
                    return true;
                default:
                    return true;
            }
        }
    }
}
