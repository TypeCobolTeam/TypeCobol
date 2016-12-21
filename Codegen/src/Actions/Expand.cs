using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    public class Expand : Action
    {
        public string Group { get; private set; }
        internal Node Source;
        internal Node Destination;
        internal string DestinationURI;
        private Dictionary<Type, Type> Generators = new Dictionary<Type, Type> {
				{ typeof(DataDescriptionEntry), typeof(TypedDataNode) },
				{ typeof(FunctionDeclarationHeader), typeof(Codegen.Nodes.FunctionDeclaration) },
				{ typeof(ProcedureStyleCallStatement), typeof(Codegen.Nodes.ProcedureStyleCall) },
				{ typeof(MoveSimpleStatement), typeof(Codegen.Nodes.TypeCobolQualifier) },
			};

        public Expand(Node source, Node destination, string destinationURI)
        {
            this.Source = source;
            this.Destination = destination;
            this.DestinationURI = destinationURI;
        }

        public void Execute()
        {
            // retrieve data
            int index;
            if (DestinationURI.EndsWith(".end")) index = this.Destination.Parent.Children.Count - 1;
            else index = this.Destination.Parent.IndexOf(this.Destination);

            if (index > -1)
            {
                var typegen = GetGeneratedNode(this.Source.CodeElement.GetType());
                var nodegen = (Node)Activator.CreateInstance(typegen, this.Source);
                this.Destination.Parent.Add(nodegen, index + 1);
            }
            // comment out original "line" (=~ non expanded node)
            this.Source.Comment = true;
            this.Source.RemoveAllChildren();
        }

        private Type GetGeneratedNode(Type type)
        {
            try { return Generators[type]; }
            catch (KeyNotFoundException) { throw new ArgumentException("Unknown type " + type); }
        }
    }
}
