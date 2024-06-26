﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Expand action, that creates a new Node that will create expanded nodes from the source node.
    /// The Source Node will be commented and its children cleared.
    /// The Expanded node will be added in the Destination's parent node as child at the right index.
    /// </summary>
    public class Expand : EventArgs, Action, IEraseAction
    {
        public string Group { get; private set; }
        internal Node Source;
        internal Node Destination;
        internal string DestinationURI;
        /// <summary>
        /// Get the list of Erased Nodes
        /// </summary>
        public IList<Node> ErasedNodes
        {
            get;
            private set;
        }

        /// <summary>
        /// The Map that gives for The source Node's CodeElement System.Type object is System.Type expander instance.
        /// </summary>
        private Dictionary<Type, Type> Generators = new Dictionary<Type, Type> {
				{ typeof(DataDescriptionEntry), typeof(TypedDataNode) },
				{ typeof(FunctionDeclarationHeader), typeof(Codegen.Nodes.FunctionDeclarationCG) },
				{ typeof(ProcedureStyleCallStatement), typeof(Codegen.Nodes.ProcedureStyleCall) }
			};

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="source">The source Node to be expanded</param>
        /// <param name="destination">The destination node of the new expanded node, the new new node will added in
        /// Destination's parent node at the right index.</param>
        /// <param name="destinationURI">The dotted path of the destination, that willl be used to calculate the
        /// Destination parent's node index to which to insert the new expanded node as child.</param>
        public Expand(Node source, Node destination, string destinationURI)
        {
            this.Source = source;
            this.Destination = destination;
            this.DestinationURI = destinationURI;
        }

        /// <summary>
        /// Perform the expansion.
        /// </summary>
        public IList<Action> Execute()
        {
            //Bug correction: Don't expand commented Nodes
            if (this.Source.Comment != null ? this.Source.Comment.Value : false)
                return null;

            var typegen = GetGeneratedNode(this.Source.CodeElement.GetType());

            // retrieve data
            int index;

            if (DestinationURI.EndsWith(".end")) index = this.Destination.Parent.Children.Count - 1;
            else index = this.Destination.Parent.IndexOf(this.Destination);

            if (Source is FunctionDeclaration fun && fun.GenerateAsNested)
            {
                if (fun.CodeElement.Visibility == AccessModifier.Public)
                {
                    Destination = Destination.Root.MainProgram.Children.First();
                }
                else
                {
                    Destination = Destination.Children.First();
                }
                index = this.Destination.Parent.Children.Count - 2;
            }

            Node nodegen = null;
            if (index > -1)
            {                
                nodegen = (Node)Activator.CreateInstance(typegen, this.Source);
                this.Destination.Parent.Add(nodegen, index + 1);
                if (nodegen is TypeCobol.Codegen.Nodes.FunctionDeclarationCG)
                {   //This is a workaround, the TypeCobol.Codegen.Nodes.FunctionDeclarationCG must have
                    //Its destination has parent.
                    nodegen.SetParent(Destination);
                }
            }
            // comment out original "line" (=~ non expanded node)
            this.Source.Comment = true;
            //Get Erased Nodes
            List<Node> erasedNodes = new List<Node>();
            this.Source.ListChildren(erasedNodes);
            erasedNodes.TrimExcess();
            ErasedNodes = erasedNodes;
            this.Source.RemoveAllChildren();
            if (nodegen is TypedDataNode)
            {
                erasedNodes.ForEach(n =>
                {
                    if (n is IndexDefinition )
                        this.Source.Add((n)); // Re-Add IndexDefinition Node to the original Node for further generation
                });
                ErasedNodes = erasedNodes.Where(n => !(n is IndexDefinition || n is Qualifier.GenerateToken)).ToList();
            }

            //Special case for NodeIndex and Qualifier node
            if (nodegen != null)
            {//Remove from the Erased nodes all reused nodes.
                List<Node> ng_child = new List<Node>();
                nodegen.ListChildren(ng_child);
                foreach(Node n in ng_child)
                {
                    ErasedNodes.Remove(n);
                }                
            }

            return null;
        }

        /// <summary>
        /// Get the System.Type instance whose instance objets are the expanded Nodes.
        /// </summary>
        /// <param name="type">The </param>
        /// <returns>The System.Type instance of expanded nodes</returns>
        private Type GetGeneratedNode(Type type)
        {
            try { return Generators[type]; }
            catch (KeyNotFoundException) { throw new ArgumentException("Unknown type " + type); }
        }
    }
}