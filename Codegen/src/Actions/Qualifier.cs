using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// The Qualifier Action. This action is used to detect if a node is suject
    /// to Type Cobol Qualifier Style.
    /// </summary>
    public class Qualifier : EventArgs, Action
    {
        /// <summary>
        /// Internal visitor class.
        /// </summary>
        internal class TypeCobolCobolQualifierVistor : TypeCobol.Compiler.CodeElements.AbstractAstVisitor
        {
            /// <summary>
            /// The Current Node
            /// </summary>
            public Node CurrentNode;

            /// <summary>
            /// Qualifier items
            /// </summary>
            public IList<SymbolReference> Items;
            /// <summary>
            /// Visitor
            /// </summary>
            /// <param name="typeCobolQualifiedSymbolReference"></param>
            /// <returns></returns>
            public override bool Visit(TypeCobol.Compiler.CodeElements.TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference)
            {   //Yes it has TypeCobol qualifier
                Items = typeCobolQualifiedSymbolReference.AsList();                
                return true;
            }

            public override bool BeginNode(Node node)
            {
                this.CurrentNode = node;
                return base.BeginNode(node);
            }

            public override void EndNode(Node node)
            {
                base.EndNode(node);
                if (HasMatch)
                {
                    Qualifier.Perform(node, this);
                    Items = null;
                }
                node.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
            }

            /// <summary>
            /// Have we matched a Type Cobol Qualifier ?
            /// </summary>
            public bool HasMatch
            {
                get
                {
                    return Items != null;
                }
            }
        }

        /// <summary>
        /// The Code Element of a Qualifier Token.
        /// </summary>
        internal class QualifierTokenCodeElement : TypeCobol.Compiler.CodeElements.CodeElement
        {
            public QualifierTokenCodeElement(TypeCobol.Compiler.Scanner.Token token) : base((CodeElementType)0)
            {
                base.ConsumedTokens = new List<TypeCobol.Compiler.Scanner.Token>();
                base.ConsumedTokens.Add(token);
            }
        }

        /// <summary>
        /// A Node to just generate Qualifier tokens.
        /// </summary>
        internal class GenerateQualifierToken : Compiler.Nodes.Node, GeneratedAndReplace
        {
            /// <summary>
            /// 
            /// </summary>
            /// <param name="codelement">The Code element of this Node</param>
            /// <param name="code">The replace code</param>
            /// <param name="sourcePositions">The Positions of the Source Node</param>
            public GenerateQualifierToken(CodeElement codelement, string code, Tuple<int, int, int, List<int>,List<int>> sourcePositions)
                : base(codelement)
            {
                ReplaceCode = code;
                SourceNodePositions = sourcePositions;
            }

            /// <summary>
            /// Source Node Positions
            /// </summary>
            public Tuple<int, int, int, List<int>, List<int>> SourceNodePositions
            {
                get;
                private set;
            }

            public string ReplaceCode
            {
                get;
                private set;
            }


            public bool IsLeaf
            {
                get { return true; }
            }

            public override bool VisitNode(IASTVisitor astVisitor)
            {
                return true;
            }
        }

        /// <summary>
        /// The Source of the Qualifation
        /// </summary>
        private Node Source;

        /// <summary>
        /// Node to text for qualificers
        /// </summary>
        /// <param name="node">The source node</param>
        public Qualifier(Node node)
        {
            this.Source = node;
        }
        public string Group
        {
            get 
            {
                return null;
            }
        }

        /// <summary>
        /// Perform the qualification action
        /// </summary>
        /// <param name="sourceNode">The source Node on which to perform teh action</param>
        /// <param name="visitor">The Visitor which as locate teh Source Node</param>
        internal static void Perform(Node sourceNode, TypeCobolCobolQualifierVistor visitor)
        {
            if (sourceNode.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                return;
            //Now this Node Is Visited
            sourceNode.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);

            Tuple<int, int, int, List<int>, List<int>> sourcePositions = sourceNode.FromToPositions;
            Stack<GenerateQualifierToken> stack = new Stack<GenerateQualifierToken>();
            IList<TypeCobol.Compiler.Scanner.Token> nodeTokens = sourceNode.CodeElement.ConsumedTokens;
            int i = 0;
            for (int j = visitor.Items.Count - 1; j >= 0; j--)
            {
                SymbolReference sr = visitor.Items[j];
                for (; i < nodeTokens.Count; i++)
                {
                    if (nodeTokens[i] == sr.NameLiteral.Token)
                    {
                        TypeCobol.Compiler.Scanner.Token tokenColonColon = null;
                        //Look for the corresponding ::
                        for (++i; i < nodeTokens.Count; i++)
                        {
                            if (!(nodeTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken))
                            {
                                if (nodeTokens[i].Text.Equals(string.Intern("::")))
                                {
                                    tokenColonColon = nodeTokens[i];
                                    i++;
                                    break;
                                }
                            }
                        }
                        //We got It ==> Create our Generate Nodes
                        GenerateQualifierToken item = new GenerateQualifierToken(
                            new QualifierTokenCodeElement(visitor.Items[visitor.Items.Count - 1 - j].NameLiteral.Token), sr.ToString(),
                            sourcePositions);
                        stack.Push(item);
                        if (tokenColonColon != null)
                        {
                            item = new GenerateQualifierToken(new QualifierTokenCodeElement(tokenColonColon), string.Intern(" OF "),
                                sourcePositions);
                            stack.Push(item);
                        }
                        break;//We got it
                    }
                }
            }
            //Now Insert all new nodes. Has Children of this node
            while (stack.Count > 0)
            {
                GenerateQualifierToken node = stack.Pop();
                node.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                sourceNode.Add(node);
            }
            //Now Comment the Source Node
            sourceNode.Comment = true;
        }

        /// <summary>
        /// Execute the Qualification action
        /// </summary>
        public void Execute()
        {
            if (Source.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                return;
            TypeCobolCobolQualifierVistor visitor = new TypeCobolCobolQualifierVistor();
            Source.AcceptASTVisitor(visitor);
        }
    }
}
