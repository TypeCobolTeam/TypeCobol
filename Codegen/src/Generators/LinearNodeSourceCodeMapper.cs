using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;


namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// This class create a mapping between Nodes and their target position in the source code
    /// for code generation. It built a linear data structure that map each line from the source
    /// document to its corresponding nodes. The Linear structure ins built in O(n) where n
    /// is the number of Nodes in the Abstract Tree Nodes.
    /// Each Node is then given an Index in to an Array Of Nodes which is constructed during a
    /// traverse of the Abstract Tree Nodes.
    /// Generating the code using this structure can be performed performed in the worst case in O(m*n) 
    /// where m is the number of line in the document and n the number of Nodes.
    /// In the best case if we consider that One node is associated to a a line the code generation will
    /// perform in O(m).
    /// </summary>
    public class LinearNodeSourceCodeMapper : NodeVisitor
    {
        /// <summary>
        /// Visitor Phase
        /// </summary>
        public enum Phase
        {
            /// <summary>
            /// The Phase wich Map the Line that must be commenetd.
            /// </summary>
            CommentedLines,
        }

        /// <summary>
        /// The Bit Array of line that must be commented
        /// </summary>
        public BitArray CommentedLines
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Bit Array of line that are associated to a node
        /// </summary>
        public BitArray NodedLines
        {
            get;
            internal set;
        }

        /// <summary>
        /// Line Informations
        /// </summary>
        public struct LineInfo
        {
            /// <summary>
            /// The Map which give for each line the associated Node.
            /// </summary>
            public List<Node> LineNodes;
            /// <summary>
            /// The Buffer associated to this line.
            /// </summary>
            public StringSourceText Buffer;
        }
        /// <summary>
        /// The Map which give for each line the associated Node.
        /// </summary>
        public LineInfo[] LineData;

        /// <summary>
        /// The Generator;
        /// </summary>
        public Generator2 Generator
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Count of Nodes.
        /// </summary>
        public int NodeCount
        {
            get;
            internal set;
        }

        /// <summary>
        /// Set the current phase
        /// </summary>
        public Phase CurrentPhase
        {
            get;
            internal set;
        }
        

        /// <summary>
        /// Structure that holds Data associated to Nodes
        /// </summary>
        public class NodeData
        {
            /// <summary>
            /// The target Node.
            /// </summary>
            public Node node;
            /// <summary>
            /// Node's Position
            /// Item1 = From
            /// Item2 = to
            /// Item3 = span on the last line
            /// Item4 = Node's Target Line Numbers.
            /// </summary>
            public Tuple<int, int, int, List<int>> Positions;
            /// <summary>
            /// The Buffer where this Node is Generated.
            /// </summary>
            public StringSourceText Buffer;
            /// <summary>
            /// From Position in the Source Text Buffer
            /// </summary>
            public Position From;
            /// <summary>
            /// To Position in the Source Text Buffer
            /// </summary>
            public Position To;
        }

        /// <summary>
        /// The List Of Node Data indexed by Node.NodeIndex
        /// </summary>
        public List<NodeData> Nodes
        {
            get;
            internal set;
        }

        /// <summary>
        /// The Constructor
        /// </summary>
        /// <param name="Input">Cobol Text Lines in input</param>
        /// <param name="generator">The Generator</param>
        public LinearNodeSourceCodeMapper(Generator2 generator)
        {
            NodeCount = 0;//Count of Nodes Treated.
            Generator = generator;
            int count = generator.Parser.Results.TokensLines.Count;
            CommentedLines = new BitArray(count);
            NodedLines = new BitArray(count);
            LineData = new LineInfo[count];            
        }

        /// <summary>
        /// The Phase wich Process commented lines.
        /// </summary>
        private bool ProcessCommentedLines(Node node)
        {
            //During the Commented Line Phase, collect data, index of all Nodes.
            var positions = node.FromToPositions;
            if (positions == null)
            {
                return true;//Node withous positions probably a generated node.
            }
            if (positions.Item4.Count == 0)
            {//This must be a Node in an imported COPY it has no lines associated to it
                return true;
            }
            NodeData data = new NodeData();
            data.node = node;
            data.Positions = positions;           
            node.NodeIndex = NodeCount++;//Give to this node its Index.            

            bool bCommented = node.Comment.HasValue ? node.Comment.Value : false;
            StringSourceText buffer = null;
            //The first line number of the target buffer
            int lineindex_buffer = -1;
            foreach (int i in data.Positions.Item4)
            {//For each line concerned by the Node
                int lineIndex = i - 1;
                NodedLines[lineIndex] = true;//This line is associated to a node.

                //This line is commented;
                if (bCommented)
                {//This node must be commented ==> Mark all its lines that must be commented.
                    CommentedLines[lineIndex] = true;
                }
                //Associate the node to it's line
                if (LineData[lineIndex].LineNodes == null)
                {
                    LineData[lineIndex].LineNodes = new List<Node>();
                }
                LineData[lineIndex].LineNodes.Add(node);
                //Associated all Lines to the Buffer of the First line in the list
                if (buffer == null)
                {
                    buffer = LineData[lineIndex].Buffer;
                    if (buffer == null)
                    {
                        buffer = new StringSourceText();
                    }
                    lineindex_buffer = lineIndex;
                }
                LineData[lineIndex].Buffer = buffer;
            }
            //Associate this node to its buffer
            data.Buffer = buffer;
            //Update the positions by translating the position to the real position in the source document..
            int line_from = data.Positions.Item1 - 1;
            int line_to = data.Positions.Item2;
            int span = data.Positions.Item3;
            List<int> lines = data.Positions.Item4;
            SourceDocument.SourceLine lineindex_srcline = Generator.TargetDocument[lineindex_buffer];            
            SourceDocument.SourceLine line = Generator.TargetDocument[lines[0] - 1];
            int delta = line.From - lineindex_srcline.From;
            data.Positions = new Tuple<int, int, int, List<int>>(delta + line_from, delta + line_to, span, lines);
            Nodes.Add(data);
            return true;
        }

        /// <summary>
        /// Accept this Node to be visited.
        /// </summary>
        /// <param name="node">The Node to be visited</param>
        public void Accept(Node node)
        {
            Nodes = new List<NodeData>();
            CurrentPhase = Phase.CommentedLines;
            Visit(node);
            Nodes.TrimExcess();
            //Create All SourceTextBuffer Content associated to Nodes
            var Input = Generator.Parser.Results.TokensLines;
            StringWriter sw = new StringWriter();
            for (int i = 0; i < LineData.Length; i++)
            {
                if (LineData[i].Buffer != null)
                {
                    TypeCobol.Compiler.Scanner.ITokensLine line = Input[i];
                    LineData[i].Buffer.Insert(line.Text, LineData[i].Buffer.Size, LineData[i].Buffer.Size);
                    LineData[i].Buffer.Insert(Environment.NewLine, LineData[i].Buffer.Size, LineData[i].Buffer.Size);
                }
            }
            //Create All Node's positions in the corresponding source text buffer.
            for (int i = 0; i < Nodes.Count; i++)
            {
                Position from = new Position(Nodes[i].Positions.Item1);
                Position to = new Position(Nodes[i].Positions.Item2);
                Nodes[i].Buffer.AddPosition(from);//from position
                Nodes[i].Buffer.AddPosition(to);//To Pos
                Nodes[i].From = from;
                Nodes[i].To = to;
            }
        }

        /// <summary>
        /// Visit a node.
        /// </summary>
        /// <param name="node">The node to visit</param>
        public void Visit(Node node)
        {
            bool doVisitChildren = false;
            switch (CurrentPhase)
            {
                case Phase.CommentedLines:
                    doVisitChildren = ProcessCommentedLines(node);
                    break;
            }            
            if (doVisitChildren) 
                foreach (var child in node.Children) 
                    child.Accept(this);
        }

        /// <summary>
        /// Check if the Two given list have an Intersection
        /// </summary>
        /// <param name="l1">The first list</param>
        /// <param name="l2">The second list</param>
        /// <returns>True if there is an intersection, false otherwise</returns>
        public static bool HasIntersection<A>(List<A> l1, List<A> l2)
        {
            if (l1 == l2)
                return true;
            if (l1 == null || l2 == null)
                return false;
            if (l1.Count == 0 || l2.Count == 0)
                return false;
            List<A> tmp;
            if (l1.Count > l2.Count)
            {
                tmp = l2;
                l2 = l1;
                l1 = tmp;
            }
            for (int i = 0; i < l1.Count; i++)
            {
                if (l2[i].Equals(l1[i]))
                    return true;
            }
            return false;
        }

        /// <summary>
        /// Dump All Structures.
        /// </summary>
        public void Dump()
        {
            //Dump Lines Information:
            System.Console.WriteLine("//<<<<<<<<<<<<<<<<<<<<<");
            System.Console.WriteLine("//<<<<<< LINES >>>>>>>");
            System.Console.WriteLine("//>>>>>>>>>>>>>>>>>>>>>");
            StringSourceText buffer = null;
            for (int i = 0; i < LineData.Length; i++)
            {
                if (LineData[i].Buffer != null && buffer != LineData[i].Buffer)
                {
                    buffer = LineData[i].Buffer;
                    System.Console.WriteLine("\\\\\\\\\\\\[StartBuffer]///////////");
                    buffer.Write(System.Console.Out);
                    System.Console.WriteLine("\\\\\\\\\\\\[EndBuffer]///////////");
                }
                if (LineData[i].Buffer == null)
                {
                    System.Console.WriteLine("!!!!!!!!!!![Start NULL Buffer]!!!!!!!!!!!");
                    System.Console.WriteLine("!!!!!!!!!!![End NULL Buffer]!!!!!!!!!!!");
                }
                StringBuilder nodes = new StringBuilder();
                if (LineData[i].LineNodes != null)
                {
                    foreach (Node n in LineData[i].LineNodes)
                    {
                        nodes.Append(n.NodeIndex);
                        nodes.Append(",");
                    }
                }
                System.Console.WriteLine("Line {0} : Commented = {1}, Nodes[{2}]", i + 1, CommentedLines[i],  nodes.ToString());
            }

            //Dump Lines Information:
            System.Console.WriteLine("//<<<<<<<<<<<<<<<<<<<<<");
            System.Console.WriteLine("//<<<<<< NODES >>>>>>>");
            System.Console.WriteLine("//>>>>>>>>>>>>>>>>>>>>>");
            for (int i = 0; i < Nodes.Count; i++)
            {
                NodeData data = Nodes[i];
                StringBuilder lines = new StringBuilder();
                foreach (int n in data.Positions.Item4)
                {
                    lines.Append(n);
                    lines.Append(",");
                }
                System.Console.WriteLine("Node {0}<{6}> : Index={1}, Positions[from={2}, To={3}, Span={4}, Lines={5}]", i,
                    i, data.Positions.Item1, data.Positions.Item2, data.Positions.Item3, lines.ToString(), data.node.GetType().FullName);
            }
        }
    }
}
