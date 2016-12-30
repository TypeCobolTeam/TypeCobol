using System;
using System.Collections;
using System.Collections.Generic;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Class that constructs the mapping between Nodes, Codelement and ICobolTextLine.
    /// </summary>
    public class DirectiveCodeElementMapper : NodeVisitor
    {
        /// <summary>
        /// All Imported Inputs, and Relocation flag are not.
        /// </summary>
        internal Dictionary<ICobolTextLine, bool> ImportedInput;
        /// <summary>
        /// A Map to associated imported code elements to their Cobol Text Line.
        /// </summary>
        internal Dictionary<TypeCobol.Compiler.CodeElements.CodeElement, TypeCobol.Compiler.Parser.CodeElementsLine> ImportedCodeElementsMap;
        /// <summary>
        /// The Map to associate a Node to is Directive ICobolTextLine instance.
        /// </summary>
        internal Dictionary<Node, ICobolTextLine> NodeDirectiveLineMap;

        /// <summary>
        /// Bit Array of commented line
        /// </summary>
        internal BitArray CommentedLines;
        /// <summary>
        /// A map which gives for a Source Text buffer its associated Nodes
        /// </summary>
        internal Dictionary<StringSourceText, List<Node> > SourceTexNodestMap;
        /// <summary>
        /// A map which gives for a line Its Source Text buffer
        /// </summary>
        internal Dictionary<int, StringSourceText > DuplicatedLineSourceTextMap;
        /// <summary>
        /// The Map that gives for a Node its From, To Position.
        /// </summary>
        internal Dictionary<Node, Tuple<Position,Position> > NodeFromToPositionMap;        
        /// <summary>
        /// A map which gives for a Node Its Source Text buffer
        /// </summary>
        internal Dictionary<Node, StringSourceText > NodeSourceTextMap;
        
        /// <summary>
        /// The Generator;
        /// </summary>
        internal Generator2 Generator;
        /// <summary>
        /// The Constructor
        /// </summary>
        /// <param name="Input">Cobol Text Lines in input</param>
        /// <param name="generator">The Generator</param>
        public DirectiveCodeElementMapper(IEnumerable<ICobolTextLine> Input, Generator2 generator)
        {
            Generator = generator;
            NodeDirectiveLineMap = new Dictionary<Node, ICobolTextLine>();
            NodeFromToPositionMap = new Dictionary<Node, Tuple<Position, Position>>();
            CollectImportedInput(Input);
        }

        /// <summary>
        /// Visit a node.
        /// </summary>
        /// <param name="node">The node to visit</param>
        public void Visit(Node node)
        {
            bool doVisitChildren = Process(node);
            if (doVisitChildren) foreach (var child in node.Children) child.Accept(this);
        }

        /// <summary>
        /// Determine if the given text line is relocated or not.
        /// </summary>
        /// <param name="textLine">The text line to be checked</param>
        /// <returns>true if the text line is relocated false otherwise</returns>
        public bool IsTextLineRelocated(ITextLine textLine)
        {
            return ImportedInput.ContainsKey((ICobolTextLine)textLine) ? ImportedInput[(ICobolTextLine)textLine] : false;
        }

        /// <summary>
        /// Map a Node to its Cobol Text Line
        /// </summary>
        /// <param name="node">The Nod eto be mapped</param>
        /// <returns>The node corresponding Cobol Text Line if any, null otherwise</returns>
        public ICobolTextLine Node2CobolTextLine(Node node)
        {
            return NodeDirectiveLineMap.ContainsKey(node) ? NodeDirectiveLineMap[node] : null;
        }

        /// <summary>
        /// Process the Node
        /// </summary>
        /// <param name="node">The nod eto be processed</param>
        /// <returns>true if children of the node must be processed, false otherwise</returns>
        private bool Process(Node node)
        {
            if (node.CodeElement != null)
            {

                Tuple<int, int, int> from_to = node.FromToPositions;
                if (from_to != null)
                {
                    bool bCommented = (node.Comment.HasValue ? node.Comment.Value : false);//Shall the node be generated in a commented line
                    int nLineIndex = node.CodeElement.ConsumedTokens[0].Line - 1;
                    var sourceLine = Generator.SourceLineMap[Generator.Parser.Results.TokensLines[nLineIndex]];
                    Tuple<Position, Position> positions = null;
                    //For a Generated Node Check if it is associated to a Line that must be commented
                    TypeCobol.Codegen.Nodes.Generated generated = node as TypeCobol.Codegen.Nodes.Generated;
                    int commentLineIndex = nLineIndex;
                    Node commentedNode = generated != null ? generated.CommentedNode : null;
                    if (commentedNode != null)
                    {//Get the Generated Comment line index
                        commentLineIndex = commentedNode.CodeElement.ConsumedTokens[0].Line - 1;                        
                    }                    
                    if (nLineIndex == commentLineIndex && CommentedLines[commentLineIndex])
                    {   //If the source line of this node target a comented line then
                        //it may generated in the target lien Source Text Buffer
                        if (bCommented)
                        {
                            // This is a commented Node in a duplicated commbeted node ==> Don't generate it
                            // An assign it a null positions
                            positions = null;
                        }
                        else
                        {
                            //This node will be generated in a separate Source Text Buffer of the original commented line
                            // => calculate the position where the generated code will be substituted in the source text buffer.
                            StringSourceText sourceText = DuplicatedLineSourceTextMap[nLineIndex];
                            Position from = new Position(from_to.Item1 - 1);
                            Position to = new Position(from_to.Item2);
                            sourceText.AddPosition(from);
                            sourceText.AddPosition(to);
                            positions = new Tuple<Position, Position>(from, to);
                            if (generated != null)
                            {//Only if generated
                                this.NodeSourceTextMap[node] = sourceText; //This node has a separate output buffer.
                                //This SourceTest is associated to this Node.
                                SourceTexNodestMap[sourceText].Add(node);
                            }
                        }
                    }
                    else
                    {
                        Position from = Generator.TargetDocument.Source.AddPosition(new Position(sourceLine.From + from_to.Item1 - 1 ));
                        Position to = Generator.TargetDocument.Source.AddPosition(new Position(sourceLine.From + from_to.Item2));
                        positions = new Tuple<Position, Position>(from, to);
                    }
                    this.NodeFromToPositionMap.Add(node, positions);                    
                    if (bCommented) //Remember line that will be commented
                    {
                        if (!DuplicatedLineSourceTextMap.ContainsKey(commentLineIndex))
                        {//Create the target Souce Text Buffer associated to this commented source line.
                            StringSourceText sourceText = new StringSourceText();
                            DuplicatedLineSourceTextMap[commentLineIndex] = sourceText;
                            SourceTexNodestMap[sourceText] = new List<Node>();

                            //Put each orginal line that must be commented in the Source Text Buffer
                            //Generated code substitution will operate in this text buffer.
                            var lines = node.Lines;
                            System.IO.StringWriter sw = new System.IO.StringWriter();
                            foreach (var line in lines)
                            {
                                string text = line.Text;
                                sw.WriteLine(text);
                            }
                            sourceText.Insert(sw.ToString(), 0, 0);
                        }
                        //Construct the line here
                        CommentedLines[nLineIndex] = true;                          
                    }
                }

                //Create the Node's From, To, Positions.
                if (ImportedCodeElementsMap.ContainsKey(node.CodeElement))
                {
                    var cel = ImportedCodeElementsMap[node.CodeElement];
                    NodeDirectiveLineMap.Add(node, ImportedCodeElementsMap[node.CodeElement]);
                    ImportedInput[cel] = true;//Mark it has relocated.
                }
            }
            return true;
        }

        /// <summary>
        /// Check to see if the given ICobolTextLine has Imported Tokens.
        /// </summary>
        /// <param name="ictl">The Cobol Text Line instance to be checked</param>
        /// <returns>true if the instance has Imported Tokens, false otherwise</returns>
        public bool HasImportedToken(ICobolTextLine ictl)
        {
            TypeCobol.Compiler.Parser.CodeElementsLine cel = (TypeCobol.Compiler.Parser.CodeElementsLine)ictl;
            if (cel.CodeElements != null)
            {
                foreach (TypeCobol.Compiler.CodeElements.CodeElement ce in cel.CodeElements)
                {
                    if (ce.IsInsideCopy())
                    {
                        ImportedCodeElementsMap[ce] = cel;
                        return true;
                    }
                }
            }
            return false;
        }

        /// <summary>
        /// Collect all Imported Input
        /// </summary>
        private void CollectImportedInput(IEnumerable<ICobolTextLine> Input)
        {
            CommentedLines = new BitArray(Generator.TargetDocument.LineCount);
            DuplicatedLineSourceTextMap = new Dictionary<int, StringSourceText>();
            NodeSourceTextMap = new Dictionary<Node, StringSourceText>();
            SourceTexNodestMap = new Dictionary<StringSourceText, List<Node>>();

            ImportedCodeElementsMap = new Dictionary<TypeCobol.Compiler.CodeElements.CodeElement, TypeCobol.Compiler.Parser.CodeElementsLine>();
            ImportedInput = new Dictionary<ICobolTextLine, bool>();
            foreach (ICobolTextLine ictl in Input)
            {
                if (HasImportedToken(ictl))
                {
                    ImportedInput[ictl] = false;
                }
            }            
        }
    }
}
