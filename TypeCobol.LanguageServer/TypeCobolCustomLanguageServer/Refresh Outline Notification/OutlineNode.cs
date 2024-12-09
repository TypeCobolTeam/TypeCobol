using System.Text;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class OutlineNode
    {
        public static OutlineNode BuildFrom(Node node)
        {
            var instance = new OutlineNode();
            instance.name = node.Name;
            instance.type = node.GetType().Name;
            instance.parentName = node.Parent?.Name;
            instance.childIndex = 0;

            if (node is FunctionDeclaration fun)
            {
                StringBuilder args = new StringBuilder();

                //Format the arguments in one line
                if (fun.Profile.InputParameters.Count > 0)
                    args.Append("in| " + string.Join(", ", fun.Profile.InputParameters.Select(p => p.Name + ":" + p.DataType.Name)) + ";");

                if (fun.Profile.InoutParameters.Count > 0)
                    args.Append("inout| " + string.Join(", ", fun.Profile.InoutParameters.Select(p => p.Name + ":" + p.DataType.Name)) + ";");

                if (fun.Profile.OutputParameters.Count > 0)
                    args.Append("out| " + string.Join(", ", fun.Profile.OutputParameters.Select(p => p.Name + ":" + p.DataType.Name)) + ";");

                if (args.Length > 0)
                    args.Remove(args.Length - 1, 1);

                instance.arguments = args.ToString();
            }

            return instance;
        }

        /// <summary>
        /// The node's name
        /// </summary>
        public string name;

        /// <summary>
        /// The node's type
        /// </summary>
        public string type;

        /// <summary>
        /// The arguments that are needed (for procedure declaration)
        /// </summary>
        public string arguments;

        /// <summary>
        /// The node's parent name
        /// </summary>
        public string parentName;

        /// <summary>
        /// The line where the node begins
        /// </summary>
        public int line;

        /// <summary>
        /// If the content of the OutlineNode is modified
        /// </summary>
        public bool isUpdated = false;

        /// <summary>
        /// All of the nodes that have this node as parent
        /// </summary>
        public List<OutlineNode> childNodes;

        private int childIndex;

        /// <summary>
        /// Update recursively all the OutlineNodes with the source file content
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        public bool Update(Node node)
        {
            int i = 0;
            //Gets the nodes that we will evaluate and transform into OutlineNodes
            var interestingNodes = node.Children
                .Where(c => c is Sentence == false && c is FunctionEnd == false && c is End == false && c is DataDescription == false && c is DataRedefines == false)
                .ToArray();

            if (interestingNodes.Length > 0)
            {
                if (childNodes == null)
                {
                    this.childNodes = new List<OutlineNode>();
                }
            }
            else
            {
                if (this.childNodes != null)
                {
                    this.childNodes = null;
                    return true;
                }
                
                return false;
            }

            int childrenCount = Math.Max(this.childNodes.Count, interestingNodes.Length);

            //Reset the updated status
            this.isUpdated = false;
            while (i < childrenCount)
            {
                // Remove the OutlineNode if if there is more OutlineNode than Nodes
                if (i >= interestingNodes.Length)
                {
                    if (i >= this.childNodes.Count)
                        break;

                    this.childNodes.RemoveAt(i);
                    this.isUpdated = true;
                    continue;
                }

                // Create and insert the OutlineNode if there is less OutlineNode than Nodes
                if (i >= this.childNodes.Count)
                {
                    this.childIndex = i;
                    this.childNodes.Insert(i, BuildFrom(interestingNodes[i]));
                    this.childNodes[i].isUpdated = true;
                    continue;
                }

                var derivationNode = this.childNodes[i].IsDerivationNode(interestingNodes[i]) ? interestingNodes[i] : null;

                //If the current OutlineNode is the equivalent of the current Node
                if (derivationNode != null)
                {
                    this.childNodes[i].isUpdated = this.childNodes[i].Update(derivationNode);

                    if (this.childNodes[i].childIndex != i)
                    {
                        this.childNodes[i].childIndex = i;
                        this.childNodes[i].isUpdated = true;
                    }

                    //Get first line of the Node that is not a commented line
                    var tokensLine = derivationNode.Lines.OfType<TokensLine>().FirstOrDefault(l => l.ScanState.InsideFormalizedComment == false && l.ScanState.InsideMultilineComments == false && l.IndicatorChar != '%' && l.IndicatorChar != '*');
                    if (tokensLine != null) 
                    {
                        //Compare lines index, if different, replace
                        if (this.childNodes[i].line != tokensLine.LineIndex + 1)
                        {
                            this.childNodes[i].line = tokensLine.LineIndex + 1;
                            this.childNodes[i].isUpdated = true;
                        }
                    }
                    //if the Node does not have lines of code but a CodeElement, compare lines index, if different, replace
                    else if (derivationNode.CodeElement != null && this.childNodes[i].line != derivationNode.CodeElement.Line || 
                        derivationNode.CodeElement == null && this.childNodes[i].line != 0)
                    {
                        this.childNodes[i].line = derivationNode.CodeElement?.Line ?? 0;
                        this.childNodes[i].isUpdated = true;
                    }


                }
                else
                {
                    //Create and insert new OutlineNodes with no Node equivalent
                    this.childNodes.Insert(i, BuildFrom(interestingNodes[i]));
                    this.childNodes[i].isUpdated = true;
                    this.childNodes[i].childIndex = i;
                    childrenCount++;
                    continue;
                }

                i++;
            }

            //returns true if the node or any child node is updated
            return this.isUpdated || this.childNodes.Any(c => c.isUpdated);
        }

        /// <summary>
        /// Check if the property of the OutlineNode are the same as those from the Node
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        public bool IsDerivationNode(Node node)
        {
            //Check the common properties
            if (this.name == node.Name && 
                this.type == node.GetType().Name &&
                this.parentName == node.Parent.Name)
            {
                //Check the special properties of a function declaration
                if (node is FunctionDeclaration fun)
                {
                    //Get the strings for each passingType (in, inout, out)
                    string[] passingTypes = new string[0];

                    //Get the strings for each passingType (in, inout, out) written in the outline node
                    if (!string.IsNullOrEmpty(this.arguments))
                        passingTypes = this.arguments.Split(';');

                    //Check if there the count of PassingType in the outline node matches the number of PassingType in the document
                    if (passingTypes.Length != fun.Profile.Parameters.GroupBy(p => p.PassingType).Count())
                        return false;

                    //Check if number of parameters in outline node matches the number of parameters in the document
                    if (passingTypes.Sum(pt => pt.Split('|')[1].Split(',').Length) != fun.Profile.Parameters.Count)
                        return false;

                    //Check if each parameter from the document is described in the outline node
                    foreach (ParameterDescription parameter in fun.Profile.Parameters)
                    {
                        if (!passingTypes.Any(pt => pt.Split('|')[1].Split(',').Any(n => n.Trim() == parameter.Name + ":" + parameter.DataType.Name)))
                            return false;
                    }

                }
                return true;
            }

            return false;
        }
    }
}
