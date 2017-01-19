using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;


namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// This class create a mapping between Nodes and their target positions in the source code
    /// for code generation. It built a linear data structure that map each line from the source
    /// document to its corresponding nodes. The Linear structure is built in O(n) where n
    /// is the number of Nodes in the Abstract Tree Nodes.
    /// Each Node is then given an Index in to an Array Of Nodes which is constructed during a
    /// traverse of the Abstract Tree Nodes.
    /// Generating the code using this structure can be performed in the worst case in O(m*n) 
    /// where m is the number of line in the document and n the number of Nodes.
    /// In the best case if we consider that One node is associated to a line, the code generation will
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
            /// The Linearization Phase.
            /// </summary>
            Linearization,
            /// <summary>
            /// The Phase to deal with removed Nodes
            /// </summary>
            RemovedNodes,
            /// <summary>
            /// The Phase to deal with Function Declaration
            /// </summary>
            FunctionDeclaration,
        }

        /// <summary>
        /// Inteernal position 0
        /// </summary>
        private static Position Pos0 = new Position(0);

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
            /// The Map which give for each line the associated Node Index.
            /// </summary>
            public List<int> LineNodes;
            /// <summary>
            /// The Buffer associated to this line.
            /// </summary>
            public StringSourceText Buffer;
            /// <summary>
            /// The Buffer associated to this line when it is generated in a function body.
            /// </summary>
            public StringSourceText FunctionBodyBuffer;
        }
        /// <summary>
        /// The Map which give for each line the associated Node.
        /// </summary>
        public LineInfo[] LineData;

        /// <summary>
        /// The Generator;
        /// </summary>
        public Generator Generator
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
        /// The Map which gives for a buffer its associated first line number.
        /// </summary>
        public Dictionary<StringSourceText, int> BufferLineMap
        {
            get;
            private set;
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
        /// The Current Function Declaration Node.
        /// </summary>
        private Node CurrentFunctionDeclNode
        {
            get;
            set;
        }

        /// <summary>
        /// A Linear Generated Dummy Node
        /// </summary>
        internal class LinearGeneratedNode : Compiler.Nodes.Node
        {
            /// <summary>
            /// Empty constructor.
            /// </summary>
            public LinearGeneratedNode() : base(null)
            {
            }

            /// <summary>
            /// Prepaculated positions
            /// </summary>
            public Tuple<int, int, int, List<int>, List<int>> Positions
            {
                get;
                set;
            }

            public override bool VisitNode(TypeCobol.Compiler.CodeElements.IASTVisitor astVisitor)
            {
                //throw new NotImplementedException();
                return false;
            }
        }

        /// <summary>
        /// Structure that holds Data associated to Nodes
        /// </summary>
        public class NodeData
        {
            /// <summary>
            /// The corresponding Node.
            /// </summary>
            public Node node;
            /// <summary>
            /// Is This node removed?
            /// </summary>
            public bool Removed;
            /// <summary>
            /// If This node is in a function Body, this the Node of the target Function.
            /// </summary>
            public Node FunctionBodyNode;
            /// <summary>
            /// Node's Position
            /// Item1 = From
            /// Item2 = to
            /// Item3 = span on the last line
            /// Item4 = Node's Target Line Numbers.
            /// Item5 = Node's Target Line offsets.
            /// </summary>
            public Tuple<int, int, int, List<int>, List<int>> Positions;
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
        /// Data for a Function Declaration Node
        /// </summary>
        public class NodeFunctionData : NodeData
        {
            /// <summary>
            /// The Buffer where a Function Declaration Node is Generated.
            /// </summary>
            public StringSourceText FunctionDeclBuffer;
            /// <summary>
            /// Node Indices associated to Function Declaration.
            /// </summary>
            public List<int> FunctionDeclNodes;
            /// <summary>
            /// The first 1-Based line index of the function body
            /// </summary>
            public int BodyFistLineIndex;
            /// <summary>
            /// The last 1-Based line index of the function body
            /// </summary>
            public int BodyLastLineIndex;
            /// <summary>
            /// Constructor
            /// </summary>
            public NodeFunctionData()
            {
                FunctionDeclBuffer = new StringSourceText();
                FunctionDeclNodes = new List<int>();
            }
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
        /// The Indices of Function Declaration Nodes.
        /// </summary>
        public List<int> FunctionDeclarationNodeIndices
        {
            get;
            private set;
        }

        /// <summary>
        /// The Constructor
        /// </summary>
        /// <param name="Input">Cobol Text Lines in input</param>
        /// <param name="generator">The Generator</param>
        public LinearNodeSourceCodeMapper(Generator generator)
        {
            NodeCount = 0;//Count of Nodes Treated.
            Generator = generator;
            int count = generator.Parser.Results.TokensLines.Count;
            CommentedLines = new BitArray(count);
            NodedLines = new BitArray(count);
            LineData = new LineInfo[count];
            FunctionDeclarationNodeIndices = new List<int>();
            BufferLineMap = new Dictionary<StringSourceText, int>();
        }

        /// <summary>
        /// Check if at the end the buffer being flushed is generated empty.
        /// A buffer is generated empty if all its target lines are commented
        /// and all nodes in the target lines are commented in block and when the buffer
        /// is Trimmed it is empty.
        /// Such buffer can be ignored.
        /// </summary>
        /// <param name="buffer">The buffer to be tested</param>
        /// <returns>true if the buffer is generated empty</returns>
        internal bool IsGeneratedEmptyBuffer(StringSourceText buffer)
        {
            if (BufferLineMap.ContainsKey(buffer))
            {
                //For each contigüous line having the same buffer
                for (int i = BufferLineMap[buffer]; i < LineData.Length && LineData[i].Buffer == buffer; i++)
                {
                    if (LineData[i].LineNodes == null)
                        return false;
                    //Each Node of the line must be commented
                    foreach (int node_index in LineData[i].LineNodes)
                    {
                        if (Nodes[node_index].node.Comment == null ? false : !Nodes[node_index].node.Comment.Value)
                            return false;
                    }
                }
                if (Generator.Layout == ColumnsLayout.CobolReferenceFormat)
                {//Cobol Format
                    for (int i = 0, j = 0; i < buffer.Size; i++, j++)
                    {
                        char c = buffer[i];
                        if (!Char.IsWhiteSpace(c) && c != '\r' && c != '\n')
                        {
                            if (j >= 6 && j < 73)
                                return false;
                        }
                        if (c == '\n')
                            j = 0;
                    }
                }
                else
                {
                    //Now the buffer content must contains only whitespace, carriage return or line feed characters.
                    for (int i = 0; i < buffer.Size; i++)
                    {
                        char c = buffer[i];
                        if (!Char.IsWhiteSpace(c) && c != '\r' && c != '\n')
                            return false;
                    }
                }                
                return true;
            }
            return false;
        }

        /// <summary>
        /// The Function Declaration Processing Phase.
        /// <param name="node">The node that belongs to a Function Body</param>
        /// <returns>True if children of the given node must be visited, false otherwise</returns>
        /// </summary>
        private bool ProcessFunctionDeclaration(Node node)
        {
            //Function Node Data
            NodeFunctionData funData = (NodeFunctionData)Nodes[CurrentFunctionDeclNode.NodeIndex];
            //Create a New Data
            NodeData data = null;
            bool bHasCodeElement = (node.CodeElement == null || node.CodeElement.ConsumedTokens == null) ? false : node.CodeElement.ConsumedTokens.Count > 0;
            if (!bHasCodeElement)
            {
                //No Code Element ==> certainly a Generated node                
                node.NodeIndex = NodeCount++;//Give to this node its Index.    
                data = new NodeData();
                data.node = node;
                Nodes.Add(data);
            }
            else
            {//If The Node has positions then linearize it
                ProcessLinearization(node, true);
                if (node.NodeIndex >= 0)
                {
                    data = Nodes[node.NodeIndex];
                    if (funData.BodyFistLineIndex == 0)
                    {
                        funData.BodyFistLineIndex = data.Positions.Item4[0];
                    }
                    funData.BodyLastLineIndex = data.Positions.Item4[0];
                }
            }
            if (node.NodeIndex >= 0)
            {
                //Associate the Function Node
                data.FunctionBodyNode = CurrentFunctionDeclNode;
                //Add the node to the Function Decl nodes list
                funData.FunctionDeclNodes.Add(node.NodeIndex);
            }
            return true;
        }

        /// <summary>
        /// The Linearization Phase.
        /// <param name="node">The node to linearize</param>
        /// <param name="functionBody">true if the node belongs to a function body, false otherwise</param>
        /// <returns>True if children of the given node must be visited, false otherwise</returns>
        /// </summary>
        private bool ProcessLinearization(Node node, bool functionBody = false)
        {
            //During the Linearization Phase, collect data, index of all Nodes.
            var positions = this.Generator.FromToPositions(node);
            if (positions == null)
            {
                node.NodeIndex = -1;
                return true;//Node without positions probably a generated node.
            }
            if (positions.Item4.Count == 0)
            {//This must be a Node in an imported COPY it has no lines associated to it
                node.NodeIndex = -1;
                return true;
            }
            if (positions.Item1 > positions.Item2)
            {   //This is a very strange node that I encountered with position (from > to)
                //I encountered this situation with tests files like:
                //CCC1B045.PGM, CCTF0011.PGM, CCTZ015B, CCTZ0300B, etc..
                //With a DataDescription Node.
                //I Ignore this kind of Node, it is a work around because it seems that
                //such nodes are considered as end of file marking nodes without any code
                //attached to.
                return true;
            }
            bool isFunctionDecl = node is FunctionDeclaration;//Detect Function Declaration
            NodeData data = isFunctionDecl ? new NodeFunctionData() : new NodeData();
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
                    LineData[lineIndex].LineNodes = new List<int>();
                }
                LineData[lineIndex].LineNodes.Add(node.NodeIndex);
                //Associated all Lines to the Buffer of the First line in the list
                if (buffer == null)
                {
                    buffer = functionBody ? LineData[lineIndex].FunctionBodyBuffer : LineData[lineIndex].Buffer;
                    if (buffer == null)
                    {
                        buffer = new StringSourceText();
                        BufferLineMap[buffer] = lineIndex;
                    }
                    lineindex_buffer = BufferLineMap[buffer];
                }
                if (functionBody)
                    LineData[lineIndex].FunctionBodyBuffer = buffer;
                else
                    LineData[lineIndex].Buffer = buffer;
                //Propagate Comment from buffer line index to current line.
                if (bCommented)
                {
                    for (int k = lineindex_buffer; k < lineIndex; k++)
                        CommentedLines[k] = true;
                }
            }
            //Associate this node to its buffer
            data.Buffer = buffer;
            //Update the positions by translating the position to the real position in the source document..
            int line_from = data.Positions.Item1 - 1;
            int line_to = data.Positions.Item2;
            int span = data.Positions.Item3;
            List<int> lines = data.Positions.Item4;
            List<int> line_offsets = data.Positions.Item5;
            SourceDocument.SourceLine lineindex_srcline = Generator.TargetDocument[lineindex_buffer];            
            SourceDocument.SourceLine line = Generator.TargetDocument[lines[0] - 1];
            int delta = line.From - lineindex_srcline.From;
            data.Positions = new Tuple<int, int, int, List<int>, List<int>>(delta + line_from, delta + line_to, span, lines, line_offsets);
            Nodes.Add(data);
            //----------------------------------------------
            //Special case for a Function Declaration Node
            //----------------------------------------------
            if (isFunctionDecl && CurrentPhase == Phase.Linearization)
            {
                //Save the Current Phase
                Phase savePhase = CurrentPhase;
                //Switch to Function Declaration Phase
                CurrentPhase = Phase.FunctionDeclaration;
                //The Current Function Node
                CurrentFunctionDeclNode = node;
                //Add to the List of encountered function declaration
                FunctionDeclarationNodeIndices.Add(node.NodeIndex);
                //Visit Each Function body Node.
                foreach (Node body_node in node.Children)
                {
                    Visit(body_node);
                }                
                //Restore the phase
                CurrentPhase = savePhase;
                return false;//Don't deal with its Children
            }
            return true;
        }

        /// <summary>
        /// Collect all lines that have not been associated to a Node durring Function Declaration
        /// processing phase. The line are then associated to Dummy nodes, that have a buffer containing
        /// the source code of the line.
        /// </summary>
        /// <param name="funData"></param>
        private void CollectFunctionBodyUnNodedLines(NodeFunctionData funData)
        {            
            Tuple<int[], int[]> insertLines = ComputeFunctionBodyInsertionLines(funData);
            var Input = Generator.Parser.Results.TokensLines;
            int offset = 0;
            for (int i = funData.BodyFistLineIndex + 1; i < funData.BodyLastLineIndex; i++)
            {                
                if (LineData[i - 1].LineNodes == null)
                {//No Nodes associated.                    
                    int j = Array.BinarySearch(insertLines.Item1, i);
                    if (j < 0)
                        continue;///???
                    int insert_index = insertLines.Item2[j];                    
                    if (insert_index < 0)
                    {
                        insertLines.Item2[j] = -insert_index;                        
                    }
                    insert_index = insertLines.Item2[j] + offset;
                    LinearGeneratedNode dummy_node = new LinearGeneratedNode();
                    dummy_node.SetFlag(Node.Flag.ExtraGeneratedLinearNode, true);
                    dummy_node.NodeIndex = NodeCount++;
                    NodeData data = new NodeData();
                    data.node = dummy_node;
                    data.From = Pos0;
                    data.To = Pos0;
                    data.FunctionBodyNode = funData.node;
                    data.Buffer = new StringSourceText();
                    //Read the line of the text in the buffer
                    TypeCobol.Compiler.Scanner.ITokensLine line = Input[i - 1];
                    data.Buffer.Insert(line.Text, data.Buffer.Size, data.Buffer.Size);
                    data.Buffer.Insert(Environment.NewLine, data.Buffer.Size, data.Buffer.Size);
                    Nodes.Add(data);
                    LineData[i - 1].LineNodes = new List<int>();
                    LineData[i - 1].LineNodes.Add(dummy_node.NodeIndex);
                    LineData[i - 1].FunctionBodyBuffer = LineData[i - 1].Buffer = data.Buffer;
                    funData.FunctionDeclNodes.Insert(insert_index, dummy_node.NodeIndex);
                    offset++;
                    int from = 0;
                    int to = data.Buffer.Size;
                    int span = 0;
                    List<int> lines = new List<int>(){i};
                    List<int> offsets = new List<int>(){0};
                    Tuple<int, int, int, List<int>, List<int>> pos = new Tuple<int, int, int, List<int>, List<int>>(from, to, span, lines, offsets);
                    dummy_node.Positions = pos; 
                }
            }
        }

        /// <summary>
        /// Relocate all Function declaration nodes that don't have positions.
        /// Nodes without positions are relocated in the last valid buffer.
        /// </summary>
        /// <param name="funData">The Function Declaration Data</param>
        private void RelocateFunctionBodyNoPositionNodes(NodeFunctionData funData)
        {
            //The last line of the function declaration
            int lastBufferLineNumber = funData.Positions.Item4.Count > 0 ? funData.Positions.Item4[funData.Positions.Item4.Count - 1] :-1;
            for (int j = 0; j < funData.FunctionDeclNodes.Count; j++)
            {
                NodeData node_data = Nodes[funData.FunctionDeclNodes[j]];
                Tuple<int, int, int, List<int>, List<int>> positions = 
                    node_data.node.IsFlagSet(Node.Flag.ExtraGeneratedLinearNode) ?
                    ((LinearGeneratedNode)node_data.node).Positions : this.Generator.FromToPositions(node_data.node);
                if (positions != null && node_data.Buffer != null)
                {
                    lastBufferLineNumber = positions.Item4[0];
                }
                else if (positions == null && lastBufferLineNumber > 0)
                {//Put this buffer in the same line then the last know buffer
                    if (LineData[lastBufferLineNumber - 1].LineNodes == null)
                        LineData[lastBufferLineNumber - 1].LineNodes = new List<int>();
                    LineData[lastBufferLineNumber - 1].LineNodes.Add(funData.FunctionDeclNodes[j]);
                    node_data.node.SetFlag(Node.Flag.NoPosGeneratedNode, true);
                }
                else if (positions == null && lastBufferLineNumber < 0)
                {//Ok in this case we must put it in the line of the Function declaration
                    Debug.Assert(lastBufferLineNumber > 0);
                }
                if (j == funData.FunctionDeclNodes.Count - 1)
                {//Mark End Function declaration Node.
                    node_data.node.SetFlag(Node.Flag.EndFunctionDeclarationNode, true);
                }
            }
        }

        /// <summary>
        /// Compute Insertion lines of a Function declaration Body, that is to say line numbers
        /// that begin an instruction : a node.
        /// </summary>
        /// <param name="funData">The Dunction data</param>
        /// <returns>A List of Tuple(LineNumber, Insertion Position)</returns>
        private Tuple<int[], int[]> ComputeFunctionBodyInsertionLines(NodeFunctionData funData)
        {
            List<int> lineNumbers = new List<int>();
            List<int> insertPoints = new List<int>();            
            for (int j = 0; j < funData.FunctionDeclNodes.Count; j++)
            {
                NodeData node_data = Nodes[funData.FunctionDeclNodes[j]];
                Tuple<int, int, int, List<int>, List<int>> positions = this.Generator.FromToPositions(node_data.node);
                if (positions != null)
                {
                    if (!lineNumbers.Contains(positions.Item4[0]))
                    {
                        //Insert interval to the previous insertion point.
                        if (lineNumbers.Count > 0)
                        {
                            int prevPoint = insertPoints[insertPoints.Count - 1];
                            for (int i = lineNumbers[lineNumbers.Count - 1] + 1; i < positions.Item4[0]; i++)
                            {
                                lineNumbers.Add(i);
                                insertPoints.Add(-(prevPoint + 1));                        
                            }
                        }
                        lineNumbers.Add(positions.Item4[0]);
                        insertPoints.Add(j);                        
                    }
                }
            }
            Tuple<int[], int[]> insertLines = new Tuple<int[], int[]>(lineNumbers.ToArray(), insertPoints.ToArray());
            return insertLines;
        }

        /// <summary>
        /// Accept this Node to be visited.
        /// </summary>
        /// <param name="node">The Node to be visited</param>
        public void Accept(Node node)
        {
            Nodes = new List<NodeData>();
            //First Phase Linearization
            CurrentPhase = Phase.Linearization;
            Visit(node);
            //Second Phase Removed Nodes
            CurrentPhase = Phase.RemovedNodes;
            foreach (Node erased_node in this.Generator.ErasedNodes)
            {
                Visit(erased_node);
            }
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
                //Deal with Function buffer
                if (LineData[i].FunctionBodyBuffer != null)
                {
                    TypeCobol.Compiler.Scanner.ITokensLine line = Input[i];
                    LineData[i].FunctionBodyBuffer.Insert(line.Text, LineData[i].FunctionBodyBuffer.Size, LineData[i].FunctionBodyBuffer.Size);
                    LineData[i].FunctionBodyBuffer.Insert(Environment.NewLine, LineData[i].FunctionBodyBuffer.Size, LineData[i].FunctionBodyBuffer.Size);
                }
            }
            //Create All Node's positions in the corresponding source text buffer.
            for (int i = 0; i < Nodes.Count; i++)
            {
                if (Nodes[i].Positions != null)
                {//Only for Nodes with positions
                    Position from = new Position(Nodes[i].Positions.Item1);
                    Position to = new Position(Nodes[i].Positions.Item2);
                    Nodes[i].Buffer.AddPosition(from);//from position
                    Nodes[i].Buffer.AddPosition(to);//To Pos
                    Nodes[i].From = from;
                    Nodes[i].To = to;
                }
            }
            //Now Complete Function Declaration Lines dispatching
            //By Dealing with all lines that are not attached to a node.
            //And relocation nodes without positions
            foreach (int fun_index in FunctionDeclarationNodeIndices)
            {
                NodeFunctionData funData = (NodeFunctionData)Nodes[fun_index];
                CollectFunctionBodyUnNodedLines(funData);
                RelocateFunctionBodyNoPositionNodes(funData);
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
                case Phase.Linearization:
                    //If this node is removed then it has already been handled by the RemovedNode Phase
                    doVisitChildren = ProcessLinearization(node);
                    break;
                case Phase.RemovedNodes:
                    if (node.Comment.HasValue ? node.Comment.Value : false)
                    {//This node is also commented ==> Thus it has already been treated by linearization phase.
                    }
                    else
                    {
                        //Same treatment like Linearization phase.
                        ProcessLinearization(node);
                    }
                    if (node.NodeIndex >= 0)
                    {
                        if (Nodes[node.NodeIndex].node == node)
                        {   //Be sure this the node at the given index, because some remove nodes have not been visited
                            //and thus have their node index set to zero.
                            //So mark this node as removed.
                            Nodes[node.NodeIndex].Removed = true;
                        }
                    }
                    //Remove node phase don't visit Children
                    doVisitChildren = false;
                    break;
                case Phase.FunctionDeclaration:
                    doVisitChildren = ProcessFunctionDeclaration(node);
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
                if (l2.Contains(l1[i]))
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
                    foreach (int index in LineData[i].LineNodes)
                    {
                        nodes.Append(index);
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
                if (data.Positions != null)
                {
                    foreach (int n in data.Positions.Item4)
                    {
                        lines.Append(n);
                        lines.Append(",");
                    }
                }
                int from = data.Positions != null ? data.Positions.Item1 : -1;
                int to = data.Positions != null ? data.Positions.Item2 : -1;
                int span = data.Positions != null ? data.Positions.Item3 : -1;
                System.Console.WriteLine("Node {0}<{6}> {7}: Index={1}, Positions[from={2}, To={3}, Span={4}, Lines={5} {8}]", i,
                    i, from, to, span, lines.ToString(), data.node.GetType().FullName, data.Removed ? "?REMOVED?" : "",
                    data.node.Comment != null ? (data.node.Comment.Value ? "COMMENTED" : "") : "");
            }
        }
        /// <summary>
        /// Dump All Structures In the Debugger Output.
        /// </summary>
        public void DebugDump()
        {
            //Dump Lines Information:
            Debug.WriteLine("//<<<<<<<<<<<<<<<<<<<<<");
            Debug.WriteLine("//<<<<<< LINES >>>>>>>");
            Debug.WriteLine("//>>>>>>>>>>>>>>>>>>>>>");
            StringSourceText buffer = null;
            for (int i = 0; i < LineData.Length; i++)
            {
                if (LineData[i].Buffer != null && buffer != LineData[i].Buffer)
                {
                    buffer = LineData[i].Buffer;
                    Debug.WriteLine("\\\\\\\\\\\\[StartBuffer]///////////");
                    Debug.Write(buffer.GetTextAt(0, buffer.Size));                    
                    Debug.WriteLine("\\\\\\\\\\\\[EndBuffer]///////////");
                }
                if (LineData[i].Buffer == null)
                {
                    Debug.WriteLine("!!!!!!!!!!![Start NULL Buffer]!!!!!!!!!!!");
                    Debug.WriteLine("!!!!!!!!!!![End NULL Buffer]!!!!!!!!!!!");
                }
                StringBuilder nodes = new StringBuilder();
                if (LineData[i].LineNodes != null)
                {
                    foreach (int index in LineData[i].LineNodes)
                    {
                        nodes.Append(index);
                        nodes.Append(",");
                    }
                }
                Debug.WriteLine("Line {0} : Commented = {1}, Nodes[{2}]", i + 1, CommentedLines[i], nodes.ToString());
            }

            //Dump Lines Information:
            Debug.WriteLine("//<<<<<<<<<<<<<<<<<<<<<");
            Debug.WriteLine("//<<<<<< NODES >>>>>>>");
            Debug.WriteLine("//>>>>>>>>>>>>>>>>>>>>>");
            for (int i = 0; i < Nodes.Count; i++)
            {
                NodeData data = Nodes[i];
                StringBuilder lines = new StringBuilder();
                if (data.Positions != null)
                {
                    foreach (int n in data.Positions.Item4)
                    {
                        lines.Append(n);
                        lines.Append(",");
                    }
                }
                int from = data.Positions != null ? data.Positions.Item1 : -1;
                int to = data.Positions != null ? data.Positions.Item2 : -1;
                int span = data.Positions != null ? data.Positions.Item3 : -1;
                Debug.WriteLine("Node {0}<{6}> {7}: Index={1}, Positions[from={2}, To={3}, Span={4}, Lines={5} {8}]", i,
                    i, from, to, span, lines.ToString(), data.node.GetType().FullName, data.Removed ? "?REMOVED?" : "",
                    data.node.Comment != null ? (data.node.Comment.Value ? "COMMENTED" : "") : "");
            }
        }

    }
}
