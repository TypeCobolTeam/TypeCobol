using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class OutlineNode
    {
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

        public OutlineNode(Node node, OutlineNode parent = null)
        {
            this.name = node.Name;
            this.type = node.GetType().Name;
            this.parentName = node.Parent?.Name;
            this.childNodes = new List<OutlineNode>();
            if (node is FunctionDeclaration fun)
            {
                StringBuilder args = new StringBuilder();

                //Format the arguments in one line
                if (fun.Profile.InputParameters.Count > 0)
                    args.Append("in: " + string.Join(", ", fun.Profile.InputParameters.Select(p => p.Name)) + "; ");

                if (fun.Profile.InoutParameters.Count > 0)
                    args.Append("inout: " + string.Join(", ", fun.Profile.InoutParameters.Select(p => p.Name)) + "; ");

                if (fun.Profile.OutputParameters.Count > 0)
                    args.Append("out: " + string.Join(", ", fun.Profile.OutputParameters.Select(p => p.Name)) + "; ");

                if (args.Length > 0)
                    args.Remove(args.Length - 2, 2);

                this.arguments = args.ToString();
            }
        }

        /// <summary>
        /// Update recursively all the OutlineNodes with the source file content
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        public bool Update(Node node)
        {
            int i = 0;
            //Gets the nodes that we will evaluate and transform into OutlineNodes
            var interestingNodes = node.Children.Where(c => c is Sentence == false && c is FunctionEnd == false && c is End == false);
            int childrenCount = Math.Max(this.childNodes.Count, interestingNodes.Count());

            //Reset the updated status
            this.isUpdated = false;
            while (i < childrenCount)
            {
                // Remove the OutlineNode if if there is more OutlineNode than Nodes
                if (i >= interestingNodes.Count())
                {
                    if (i >= this.childNodes.Count)
                        break;

                    this.childNodes.RemoveAt(i);
                    this.isUpdated = true;
                    continue;
                }

                // Create and insert the OutlineNode if if there is less OutlineNode than Nodes
                if (i >= this.childNodes.Count)
                {
                        this.childNodes.Insert(i, new OutlineNode(interestingNodes.ElementAt(i), this));
                        this.childNodes[i].isUpdated = true;
                        continue;
                }

                var derivationNode = this.childNodes[i].IsDerivationNode(interestingNodes.ElementAt(i)) ? interestingNodes.ElementAt(i) : null;

                //If the current OutlineNode is the equivalent of the current Node
                if (derivationNode != null)
                {
                    this.childNodes[i].isUpdated = this.childNodes[i].Update(derivationNode);

                    //Get first line of the Node that is not a commented line
                    var tokensLine = derivationNode.Lines.OfType<TokensLine>().FirstOrDefault(l => l.ScanState.InsideFormalizedComment == false && l.ScanState.InsideMultilineComments == false && l.IndicatorChar != '*');
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
                    this.childNodes.Insert(i, new OutlineNode(interestingNodes.ElementAt(i), this));
                    this.childNodes[i].isUpdated = true;
                    childrenCount++;
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
            if (this.name == node.Name && 
                this.type == node.GetType().Name &&
                this.parentName == node.Parent.Name)
            {
                if (this.arguments == null)
                    return true;

                if (node is FunctionDeclaration fun)
                {
                    foreach (string substring in this.arguments.Split(';'))
                    {
                        string[] parameters = substring.Split(':');
                        string[] s = parameters[1].Split(',');
                        switch (parameters[0].Trim())
                        {
                            case "in":
                                if (!CheckArguments(s, fun.Profile.InputParameters))
                                {
                                    return false;
                                }
                                break;
                            case "inout":
                                if (!CheckArguments(s, fun.Profile.InoutParameters))
                                {
                                    return false;
                                }
                                break;
                            case "out":
                                if (!CheckArguments(s, fun.Profile.OutputParameters))
                                {
                                    return false;
                                }
                                break;
                        }
                    }
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Check if the arguments of the OutlineNode are the same as the one in the Node
        /// </summary>
        /// <param name="parameters"></param>
        /// <param name="nodeParameters"></param>
        /// <returns></returns>
        private bool CheckArguments(string[] parameters, IList<ParameterDescription> nodeParameters)
        {
            if (parameters.Length == nodeParameters.Count)
            {
                for (int i = 0; i < parameters.Length; i++)
                {
                    if (parameters[i].Trim() != nodeParameters[i].Name)
                    {
                        return false;
                    }
                }

                return true;
            }

            return false;
        }
    }
}
