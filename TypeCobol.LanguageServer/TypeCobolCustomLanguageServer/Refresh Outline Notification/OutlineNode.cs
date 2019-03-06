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

        public bool Update(Node node)
        {
            int i = 0;
            var interestingNodes = node.Children.Where(c => c is Sentence == false && c is FunctionEnd == false && c is End == false);
            int childrenCount = Math.Max(this.childNodes.Count, interestingNodes.Count());
            this.isUpdated = false;
            while (i < childrenCount)
            {
                if (i >= interestingNodes.Count())
                {
                    if (i >= this.childNodes.Count)
                        break;

                    this.childNodes.RemoveAt(i);
                    this.isUpdated = true;
                    continue;
                }

                if (i >= this.childNodes.Count)
                {
                        this.childNodes.Insert(i, new OutlineNode(interestingNodes.ElementAt(i), this));
                        this.childNodes[i].isUpdated = true;
                        continue;
                }

                var derivationNode = this.childNodes[i].GetDerivationNode(interestingNodes.ElementAt(i));

                if (derivationNode != null)
                {
                    this.childNodes[i].isUpdated = this.childNodes[i].Update(derivationNode);

                    var tokensLine = derivationNode.Lines.OfType<TokensLine>().FirstOrDefault(l => l.ScanState.InsideFormalizedComment == false && l.ScanState.InsideMultilineComments == false && l.IndicatorChar != '*');
                    if (tokensLine != null) 
                    {
                        if (this.childNodes[i].line != tokensLine.LineIndex + 1)
                        {
                            this.childNodes[i].line = tokensLine.LineIndex + 1;
                            this.childNodes[i].isUpdated = true;
                        }
                    }
                    else if (derivationNode.CodeElement != null && this.childNodes[i].line != derivationNode.CodeElement.Line || 
                        derivationNode.CodeElement == null && this.childNodes[i].line != 0)
                    {
                        this.childNodes[i].line = derivationNode.CodeElement?.Line ?? 0;
                        this.childNodes[i].isUpdated = true;
                    }


                }
                else
                {
                    this.childNodes.Insert(i, new OutlineNode(interestingNodes.ElementAt(i), this));
                    this.childNodes[i].isUpdated = true;
                    childrenCount++;
                }

                i++;
            }

            return this.isUpdated || this.childNodes.Any(c => c.isUpdated);
        }

        public Node GetDerivationNode(Node node)
        {
            if (this.name == node.Name && 
                this.type == node.GetType().Name &&
                this.parentName == node.Parent.Name)
            {
                if (node is FunctionDeclaration fun)
                {
                    bool isDerivation = true;
                    foreach (string substring in this.arguments.Split(';'))
                    {
                        string[] parameters = substring.Split(':');
                        switch (parameters[0])
                        {
                            case "in":
                                if (parameters[1].Split(',').Length != fun.Profile.InputParameters.Count)
                                    isDerivation = false;
                                    break;
                            case "inout":
                                if (parameters[1].Split(',').Length != fun.Profile.InoutParameters.Count)
                                    isDerivation = false;
                                break;
                            case "out":
                                if (parameters[1].Split(',').Length != fun.Profile.OutputParameters.Count)
                                    isDerivation = false;
                                break;
                        }
                    }

                    if (isDerivation)
                        return node;
                }
                else if (this.arguments == null)
                    return node;
            }

            return null;
        }
    }
}
