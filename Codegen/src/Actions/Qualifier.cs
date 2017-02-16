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
            /// The Generator Instance
            /// </summary>
            internal Generator Generator;
            /// <summary>
            /// The Current Node
            /// </summary>
            public Node CurrentNode;
            /// <summary>
            /// Qualifier items
            /// </summary>
            public IList<SymbolReference> Items;
            public IList< IList<SymbolReference> > AllItemsList;
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="generator">The Generator instance</param>
            internal TypeCobolCobolQualifierVistor(Generator generator)
            {
                this.Generator = generator;
                this.AllItemsList = new List<IList<SymbolReference>>();
            }

            /// <summary>
            /// Visitor
            /// </summary>
            /// <param name="typeCobolQualifiedSymbolReference"></param>
            /// <returns></returns>
            public override bool Visit(TypeCobol.Compiler.CodeElements.TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference)
            {   //Yes it has TypeCobol qualifier
                IList<SymbolReference> items = typeCobolQualifiedSymbolReference.AsList();
                if (Items == null)
                {
                    Items = items;
                }
                else
                {                    
                    //Check if the first element is already in
                    SymbolReference first = items[0];
                    if (Containss(first))
                        return true;
                    if (Contains(first))
                    {
                        foreach (SymbolReference sr in items)
                        {
                            if (!Contains(sr))
                                Items.Add(sr);
                        }
                    }
                    else
                    {
                        AllItemsList.Add(Items);
                        Items = items;
                    }
                }
                return true;
            }

            /// <summary>
            /// Checks if the current symbol reference is contained in the aready collected Items list.
            /// </summary>
            /// <param name="sr">The Symbol Reference to Check</param>
            /// <returns>True if it is aready in, false otherwise</returns>
            private bool Contains(SymbolReference sr)
            {
                if (Items == null)
                    return false;
                foreach (SymbolReference r in Items)
                    if (sr == r)
                        return true;
                return false;
            }

            /// <summary>
            /// Checks if the current symbol reference is contained in the aready collected Items list.
            /// </summary>
            /// <param name="sr">The Symbol Reference to Check</param>
            /// <returns>True if it is aready in, false otherwise</returns>
            private bool Containss(SymbolReference sr)
            {
                if (AllItemsList == null)
                    return false;
                foreach (var l in AllItemsList)
                {
                    foreach (SymbolReference r in l)
                        if (sr == r)
                            return true;
                }
                return false;
            }

            public override bool BeginNode(Node node)
            {
                PerformMatch();
                this.CurrentNode = node;
                return base.BeginNode(node);
            }

            public override void EndNode(Node node)
            {
                base.EndNode(node);
                PerformMatch();
                node.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
            }

            private void PerformMatch()
            {
                if (HasMatch)
                {
                    //Add in the list the last qualified sequence
                    AllItemsList.Add(Items);
                    Perform(this.CurrentNode);
                    Items = null;
                    AllItemsList.Clear();
                }
            }

            /// <summary>
            /// Have we matched a Type Cobol Qualifier ?
            /// </summary>
            public bool HasMatch
            {
                get
                {
                    return this.CurrentNode != null && Items != null;
                }
            }

            /// <summary>
            /// For all Items the indices of their token in the consumed Tokens.
            /// </summary>
            /// <returns>The List of boundary tuple.</returns>
            private List<Tuple<int, int>> ItemsListIndexBoundary(IList<TypeCobol.Compiler.Scanner.Token> nodeTokens)
            {
                List<Tuple<int, int>> boundaries = new List<Tuple<int, int>>();
                foreach (var items in AllItemsList)
                {
                    SymbolReference sr1 = items[0];
                    int index1 = FindSymbolRefererenceTokenIndex(sr1, nodeTokens);
                    SymbolReference sr2 = items[items.Count - 1];
                    int index2 = FindSymbolRefererenceTokenIndex(sr2, nodeTokens);
                    boundaries.Add(new Tuple<int, int>(index2, index1));
                }
                return boundaries;
            }

            /// <summary>
            /// Get the index of the corresponding SymbolReference token in the given token list.
            /// </summary>
            /// <param name="sr">The SymbolReference token</param>
            /// <param name="nodeTokens">The loken list</param>
            /// <returns>The index if found, -1 otherwise.</returns>
            private static int FindSymbolRefererenceTokenIndex(SymbolReference sr, IList<TypeCobol.Compiler.Scanner.Token> nodeTokens)
            {
                for (int i = 0; i < nodeTokens.Count; i++)
                {
                    if (nodeTokens[i] == sr.NameLiteral.Token)
                        return i;
                }
                return -1;
            }

            /// <summary>
            /// Perform the qualification action
            /// </summary>
            /// <param name="sourceNode">The source Node on which to perform teh action</param>
            /// <param name="visitor">The Visitor which as locate teh Source Node</param>
            internal void Perform(Node sourceNode)
            {
                if (sourceNode.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                    return;
                //Now this Node Is Visited
                sourceNode.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                Tuple<int, int, int, List<int>, List<int>> sourcePositions = this.Generator.FromToPositions(sourceNode);
                IList<TypeCobol.Compiler.Scanner.Token> nodeTokens = sourceNode.CodeElement.ConsumedTokens;
                List<Tuple<int, int>> boundaries = ItemsListIndexBoundary(nodeTokens);
                int b = 0;
                foreach (var items in AllItemsList)
                {
                    Tuple<int, int> range = boundaries[b++];
                    Items = items;
                    int i = range.Item1;
                    for (int j = 0; j < Items.Count; j++)
                    {
                        SymbolReference sr = Items[Items.Count - j - 1];
                        for (; i <= range.Item2; i++)
                        {
                            if (nodeTokens[i] == sr.NameLiteral.Token)
                            {
                                TypeCobol.Compiler.Scanner.Token tokenColonColon = null;
                                //Look for the corresponding ::
                                for (++i; i <= range.Item2; i++)
                                {
                                    if (!(nodeTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken))
                                    {
                                        if (nodeTokens[i].TokenType == TypeCobol.Compiler.Scanner.TokenType.QualifiedNameSeparator)
                                        {
                                            tokenColonColon = nodeTokens[i];
                                            i++;
                                            break;
                                        }                                        
                                    }
                                }
                                //We got It ==> Create our Generate Nodes
                                GenerateQualifierToken item = new GenerateQualifierToken(
                                    new QualifierTokenCodeElement(sr.NameLiteral.Token), Items[j].ToString(),
                                    sourcePositions);
                                item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                                sourceNode.Add(item);
                                if (tokenColonColon != null)
                                {
                                    item = new GenerateQualifierToken(new QualifierTokenCodeElement(tokenColonColon), string.Intern(" OF "),
                                        sourcePositions);
                                    item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                                    sourceNode.Add(item);
                                }
                                break;//We got it
                            }
                        }
                    }
                }
                //Now Comment the Source Node
                sourceNode.Comment = true;
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
        /// The Generator Instance
        /// </summary>
        internal Generator Generator;
        /// <summary>
        /// Node to text for qualificers
        /// </summary>
        /// <param name="node">The source node</param>
        /// <param name="generator">The Generator instance</param>
        public Qualifier(Generator generator, Node node)
        {
            this.Source = node;
            this.Generator = generator;
        }
        public string Group
        {
            get 
            {
                return null;
            }
        }

        /// <summary>
        /// Execute the Qualification action
        /// <param name="generator">The Genarator instance</param>
        /// </summary>
        public void Execute()
        {
            if (Source == null)
                return;
            if (Source.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                return;
            TypeCobolCobolQualifierVistor visitor = new TypeCobolCobolQualifierVistor(Generator);
            Source.AcceptASTVisitor(visitor);
        }
    }
}
