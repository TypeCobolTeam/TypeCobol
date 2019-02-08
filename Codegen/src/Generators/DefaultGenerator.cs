using System;
using System.Collections;
using System.Collections.Generic;
using System.Dynamic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Codegen.Actions;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Diagnostics;
using static TypeCobol.Codegen.Generators.LinearNodeSourceCodeMapper;
using System.Runtime.CompilerServices;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Default Generator
    /// </summary>
    public class DefaultGenerator : Generator
    {
        const int MAX_COBOL_LINE_LENGTH = 80;
        const int LEGAL_COBOL_LINE_LENGTH = 72;
        /// <summary>
        /// Flag to indicate that the buffer has alreday been in line overflow states.
        /// </summary>
        const int ALREADY_LINE_OVERFLOW = 0x01 << 0;
        /// <summary>
        /// The Map of Exceed Lines over column 72, associated to their (start position and length).
        /// </summary>
        private Dictionary<int, Tuple<int /*line start position*/, int /*line length*/>> ExceedLines;
        /// <summary>
        /// The Map which give for a Buffer its exceed positions by line .
        /// </summary>
        Dictionary<SourceText, Dictionary<int, List<Position>>> BufferExceedMap;
        /// <summary>
        /// The Set to indicates which line number has alreday been checked for characters that were in column 73-80.
        /// </summary>
        private HashSet<int> Lines_73_80_Flags;
        /// <summary>
        /// Minimal splitting column.
        /// </summary>
        private const int MIN_SPLIT_COLUMN = 12;

        /// <summary>
        /// The 0 based, Line Number where the TypeCobol version can be added.
        /// </summary>
        public int TypeCobolVersionLineNumber { get; private set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public DefaultGenerator(TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, List<Skeleton> skeletons, string typeCobolVersion)
            : base(document, destination, skeletons, null)
        {
            TypeCobolVersion = typeCobolVersion;
            int count = CompilationResults.TokensLines.Count;
        }

        public override void GenerateLineMapFile(Stream stream)
        {
        }

        public override bool HasLineMapData => false;

        /// <summary>
        /// 
        /// </summary>
        /// <param name="bTrackFirtNonCblDirectiveLine"></param>
        /// <returns></returns>
        protected override int CreateTargetDocument(bool bTrackFirtNonCblDirectiveLine)
        {
            int i = base.CreateTargetDocument(true);
            TypeCobolVersionLineNumber = Math.Max(0, i);
            return i;
        }

        /// <summary>
        /// Perform Tree to Code generation
        /// </summary>
        protected override void TreeToCode()
        {
            LinearNodeSourceCodeMapper mapper = new LinearNodeSourceCodeMapper(this);
            mapper.Accept(RootNode);
            //mapper.DebugDump();
            SourceText generatedDocument = LinearGeneration(mapper, CompilationResults.TokensLines);
            // Step 3: Write target document
            //TCCODEGEN_NO_TRAILING_SPACES
            generatedDocument.Write(Destination);
        }

        /// <summary>
        /// Line Mapping context
        /// </summary>
        public class LineMappingCtx
        {
            /// <summary>
            /// Starting the line mapping at ine 1
            /// </summary>
            public int startLineMapCounter;
            /// <summary>
            /// Current base offset in the source code advance.
            /// </summary>
            public int currentGenLineOffset = 0;
            /// <summary>
            /// The current function declaration data if any
            /// </summary>
            public NodeFunctionData funData;
            /// <summary>
            /// The line mapping data
            /// </summary>
            public Tuple<int, int>[] LineMapping;
            /// <summary>
            /// Set of relocated lines from a function declaration.
            /// </summary>
            public HashSet<int> RelocatedLines;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="lineMap"></param>
            public LineMappingCtx(Tuple<int,int>[] lineMap)
            {
                this.LineMapping = lineMap;
                this.startLineMapCounter = 1;
            }

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="lineMap"></param>
            /// <param name="funData"></param>
            public LineMappingCtx(Tuple<int, int>[] lineMap, NodeFunctionData funData)
            {
                this.LineMapping = lineMap;
                this.funData = funData;
                this.startLineMapCounter = 0;
            }

            /// <summary>
            /// Add arelocated line
            /// </summary>
            /// <param name="l"></param>
            private void AddRelocatedLine(int l)
            {
                if (RelocatedLines == null)
                {
                    RelocatedLines = new HashSet<int>();
                }
                RelocatedLines.Add(l);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private bool IsRelocatedLine(int i)
            {
                return RelocatedLines != null ? RelocatedLines.Contains(i) : false;
            }
            /// <summary>
            /// Update the line map from the given buffer.
            /// </summary>
            /// <param name="i">Current zero based line number</param>
            /// <param name="targetSourceText">The buffer containing the text</param>
            /// <param name="flushingBuffer">The buffer whish being flushed</param>
            public void UpdateLineMap(int i, SourceText targetSourceText, LineStringSourceText flushingBuffer)
            {
                if (flushingBuffer == null)
                {//No buffer ==> This is a line that must be generated as it is in the original source code.
                    System.Diagnostics.Debug.Assert(LineMapping[i] == null || IsRelocatedLine(i));
                    if (!(LineMapping[i] == null || IsRelocatedLine(i)))
                        return;//Don't take risk if assertion failure.
                    if (!IsRelocatedLine(i))
                    {
                        //We are inside a function declaration.
                        if (funData != null && funData.LineMapFirstIndex == 0)
                        {
                            funData.LineMapFirstIndex = i;
                        }
                        var interval = GenLineMapping(startLineMapCounter, currentGenLineOffset, targetSourceText);
                        LineMapping[i] = interval;
                        currentGenLineOffset = targetSourceText.Size;
                        startLineMapCounter += interval.Item2 - interval.Item1 + 1;
                        if (funData != null)
                        {
                            funData.LineMapLastIndex = i;
                        }
                    }
                }
                else
                {
                    if (flushingBuffer.Lines != null)
                    {
                        var interval = GenLineMapping(startLineMapCounter, currentGenLineOffset, targetSourceText);
                        foreach (var l in flushingBuffer.Lines)
                        {
                            System.Diagnostics.Debug.Assert(LineMapping[l] == null || flushingBuffer.Reallocated);
                            if (!(LineMapping[l] == null || flushingBuffer.Reallocated))
                                return;//Don't take risk if assertion failure.
                            if (funData != null && funData.LineMapFirstIndex == 0)
                            {
                                funData.LineMapFirstIndex = l;
                            }
                            LineMapping[l] = interval;
                            if (flushingBuffer.Reallocated)
                            {
                                AddRelocatedLine(l);
                            }
                            if (funData != null)
                            {
                                funData.LineMapLastIndex = l;
                            }
                        }
                        startLineMapCounter += interval.Item2 - interval.Item1 + 1;
                        currentGenLineOffset = targetSourceText.Size;
                    }                                        
                }
            }

            /// <summary>
            /// Relocate the Range mapping of a function declaration
            /// </summary>
            /// <param name="funData">Function declaration data</param>
            /// <param name="funSourceText">The current function source code for relocation</param>
            /// <param name="mainSourceText">The main source code buffer into which the relocation is performed </param>
            /// <param name="bNested">true if we are relocating a function declaration generated as a nested program, false otherwise</param>
            public void RelocateFunctionRanges(NodeFunctionData funData, StringSourceText funSourceText, SourceText mainSourceText, bool bNested)
            {
                Tuple<int, int> interval = GenLineMapping(0, 0, funSourceText);
                if (funData.LineMapFirstIndex == 0 && funData.LineMapLastIndex == 0)
                {//Special case the body of the function is empty. ==> So take the range of lines in its buffer
                    foreach (var l in funData.Buffer.Lines)
                    {
                        if (funData.LineMapFirstIndex == 0)
                        {
                            funData.LineMapFirstIndex = l;
                        }
                        else
                        {
                            funData.LineMapFirstIndex = Math.Min(funData.LineMapFirstIndex, l);
                        }
                    }
                    funData.LineMapLastIndex = funData.LineMapFirstIndex;                   
                    LineMapping[funData.LineMapFirstIndex] = interval;
                }

                for (int i = funData.LineMapFirstIndex; i <= funData.LineMapLastIndex; i++)
                {
                    if (LineMapping[i] != null)
                    {
                        LineMapping[i] = new Tuple<int, int>(LineMapping[i].Item1 + startLineMapCounter, LineMapping[i].Item2 + startLineMapCounter);
                    }
                }
                //We update the new position.
                startLineMapCounter += interval.Item2 - interval.Item1 + 1;
                if (bNested)
                {
                    currentGenLineOffset = mainSourceText.Size;
                }
            }

            /// <summary>
            /// Generate the Line
            /// </summary>        
            /// <param name="startLine">The starting line number</param>
            /// <param name="interval">[out] The interval</param>
            /// <param name="startOffset">The starting offset in the document</param>
            /// <param name="targetSourceText">The current source codument</param>
            /// <returns></returns>
            private Tuple<int, int> GenLineMapping(int startLine, int startOffset, SourceText targetSourceText)
            {
                int count = targetSourceText.LineCount(startOffset, targetSourceText.Size);
                return new Tuple<int, int>(startLine, startLine + count - 1);
            }

            /// <summary>
            /// Update all delta based on the content of the urrent buffer.
            /// </summary>
            /// <param name="targetSourceText">The current buffer</param>
            internal void UpdateDeltas(SourceText targetSourceText)
            {
                var interval = GenLineMapping(startLineMapCounter, currentGenLineOffset, targetSourceText);
                currentGenLineOffset = targetSourceText.Size;
                startLineMapCounter += interval.Item2 - interval.Item1 + 1;
            }
        }

        /// <summary>
        /// Perform a linear Generation
        /// //1) A Non commented line with no Associated nodes is generated without any change.
        /// //2) If the line is commented then first comment all following lines that have the same intersection with the corresponding target Nodes.
        /// //3) For each node related to a line, and not already generated the corresponding code.
        /// //4) Flush of Function declations.
        /// <param name="mapper">The linearization representation</param>
        /// <param name="Input">Input source lines</param>
        /// <returns>The Generated Source Document</returns>
        /// </summary>
        protected virtual SourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, IReadOnlyList<A> Input) where A : ITextLine
        {
            return LinearGeneration<A>(mapper, Input, null);
        }

        /// <summary>
        /// Perform a linear Generation
        /// //1) A Non commented line with no Associated nodes is generated without any change.
        /// //2) If the line is commented then first comment all following lines that have the same intersection with the corresponding target Nodes.
        /// //3) For each node related to a line, and not already generated the corresponding code.
        /// //4) Flush of Function declations.
        /// <param name="mapper">The linearization representation</param>
        /// <param name="Input">Input source lines</param>
        /// <param name="lmCtx">The Line Mapping instance</param>"
        /// <returns>The Generated Source Document</returns>
        /// </summary>
        protected SourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, IReadOnlyList<A> Input, LineMappingCtx lmCtx) where A : ITextLine
        {
            /// Marker of all line that have beeen already generated has commented.
            BitArray CommentedLinesDone = new BitArray(CompilationResults.TokensLines.Count);
            //Final Generated source code
            SourceText targetSourceText = new GapSourceText();
            //Stack Used to save current generation buffer when switching in a function declaration generation.
            //Beacuse a function declartion has its own buffer.
            Stack<SourceText> stackOuterBuffer = new Stack<SourceText>();
            Stack<SourceText> stackLocalBuffer = new Stack<SourceText>();
            Stack<LineMappingCtx> stackLineMappingCtx = new Stack<LineMappingCtx>();
            //Bit Array of Generated Nodes.
            BitArray generated_node = new BitArray(mapper.NodeCount);
            //For detecting line having characters in columns [73-80]
            Lines_73_80_Flags = new HashSet<int>();
            //The previous line generation buffer 
            LineStringSourceText previousBuffer = null;
            bool insideMultilineComments = false;            

            for (int i = 0; i < mapper.LineData.Length; i++)
            {
                if (i == TypeCobolVersionLineNumber && this.TypeCobolVersion != null)
                {
                    targetSourceText.Insert("      *TypeCobol_Version:" + this.TypeCobolVersion, targetSourceText.Size, targetSourceText.Size);
                    targetSourceText.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
                }

                //--------------------------------------------------------------------------------------------------------------
                //1) A Non commented line with no Associated nodes is generated without any change.
                if (!mapper.CommentedLines[i] && mapper.LineData[i].LineNodes == null)
                {
                    if (!mapper.LineData[i].Skip)
                    {
                        //If there was a previous buffer ==> Flush it
                        if (previousBuffer != null)
                        {
                            if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                            {
                                AppendBufferContent(targetSourceText, previousBuffer);
                                lmCtx?.UpdateLineMap(i, targetSourceText, previousBuffer);
                            }
                            previousBuffer = null;
                        }
                        string text = Input[i].Text;

                        // Comment the multilines comments

                        if ((Input[i] as Compiler.Scanner.TokensLine)?.SourceTokens.Any(st => st.TokenType == Compiler.Scanner.TokenType.MULTILINES_COMMENTS_START) == true)
                        {
                            insideMultilineComments = true;
                        }

                        if (text.Length >= 7 && insideMultilineComments)
                        {
                            text = text.Substring(0, 6) + '*' + text.Substring(7, text.Length - 7);
                        }

                        if (mapper.LineData[i].Buffer != null)
                        {
                            //This line has been assigned a target Buffer
                            mapper.LineData[i].Buffer.Insert(text, mapper.LineData[i].Buffer.Size, mapper.LineData[i].Buffer.Size);
                            mapper.LineData[i].Buffer.Insert(Environment.NewLine, mapper.LineData[i].Buffer.Size,
                                mapper.LineData[i].Buffer.Size);
                        }
                        else
                        {
                            targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                            targetSourceText.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
                        }

                        if ((Input[i] as Compiler.Scanner.TokensLine)?.SourceTokens.Any(st => st.TokenType == Compiler.Scanner.TokenType.MULTILINES_COMMENTS_STOP) == true)
                        {
                            insideMultilineComments = false;
                        }
                    }
                    if (mapper.LineData[i].Buffer == null)
                    {
                        lmCtx?.UpdateLineMap(i, targetSourceText, null);
                    }
                    continue;
                }
                //--------------------------------------------------------------------------------------------------------------
                //2) If the line is commented then first comment all following lines that have the same intersection with
                // the corresponding target Nodes.
                List<int> line_nodes = mapper.LineData[i].LineNodes;
                //If there was a previous buffer ==> Flush it
                if (previousBuffer != null && mapper.CommentedLines[i])
                {
                    if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                    {
                        AppendBufferContent(targetSourceText, previousBuffer);
                        lmCtx?.UpdateLineMap(i, targetSourceText, previousBuffer);
                    }
                    previousBuffer = null;
                }
                for (int j = i; mapper.CommentedLines[j]; j++)
                {                    
                    List<int> current_nodes = mapper.LineData[j].LineNodes;
                    if (!CommentedLinesDone[j])
                    {//Don't comment twice same lines
                        if (!LinearNodeSourceCodeMapper.HasIntersection(line_nodes, current_nodes))
                            break; //This commented line has no nodes which intersect with the previous line.
                        IEnumerable<ITextLine> lines = Indent(Input[j], true);
                        foreach (var line in lines)
                        {
                            string text = line.Text.TrimEnd();
                            targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                            targetSourceText.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
                            CheckFunctionDeclCommentedheader(mapper, current_nodes, text);
                        }

                        if (current_nodes != null)
                        {
                            //Inform only if the node has other nodes
                            mapper.CommentedLines[j] = false; //This commented line has been generated now
                        }
                        CommentedLinesDone[j] = true;
                    }
                    
                    line_nodes = current_nodes;
                }
                //--------------------------------------------------------------------------------------------------------------
                //3)For each node related to this line, and not already generated.
                line_nodes = mapper.LineData[i].LineNodes;
                if (line_nodes == null)
                {//No Node to generate
                    continue;
                }
                foreach(int node_index in line_nodes)
                {
                    if (node_index == -1 || mapper.Nodes[node_index].node.IsFlagSet(Node.Flag.GeneratorCanIgnoreIt))
                    {//bad Node
                        continue;
                    }
                    if (generated_node[node_index])
                        continue;//Already Generated.
                    bool bFunctionBodyNode = mapper.Nodes[node_index].FunctionBodyNode != null;//Is this node in a function body ?
                    LineStringSourceText curSourceText = mapper.Nodes[node_index].Buffer;
                    if (curSourceText != previousBuffer && previousBuffer != null)
                    {//Flush previous buffer
                        if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                        {
                            AppendBufferContent(targetSourceText, previousBuffer);
                            lmCtx?.UpdateLineMap(i, targetSourceText, previousBuffer);
                        }
                        previousBuffer = null;
                    }
                    Node node = mapper.Nodes[node_index].node;
                    bool bGenerated = node is Generated;
                    bool bForceGenerateLines = true;
                    if (!bGenerated)
                    {   //This Node is not Generated: If it removed then remove its source code otherwise do Nothing it is already in the source buffer.
                        bForceGenerateLines = false;
                        if (mapper.Nodes[node_index].Removed)
                        {//If this node is removed
                            //var sourceLine = TargetDocument[i];
                            Position from = mapper.Nodes[node_index].From;
                            Position to = mapper.Nodes[node_index].To;
                            //Delete <==> Replace with blanks
                            ReplaceByBlanks(curSourceText, from.Pos, to.Pos);
                        }
                        else if (mapper.Nodes[node_index].node.IsFlagSet(Node.Flag.ForceGetGeneratedLines))
                        {//As lines to generate and replace
                            bForceGenerateLines = true;
                        }
                    }
                    if (bForceGenerateLines)
                    {
                        bool bIsFunctionDecl = mapper.Nodes[node_index] is LinearNodeSourceCodeMapper.NodeFunctionData;
                        bool bFirst = true;
                        Position from = mapper.Nodes[node_index].From;
                        Position to = mapper.Nodes[node_index].To;
                        bool bIsGenerateAndReplace = node is GeneratedAndReplace;
                        if (bIsGenerateAndReplace)
                        {//The node has a source code that must be replaced
                            string code = (node as GeneratedAndReplace).ReplaceCode;
                            GenerateIntoBufferCheckLineExceed(from, to, curSourceText, code, i + 1);
                        }
                        else foreach (var line in NodeLines(node, generated_node))
                            {
                                bool bInsertSplit = false;
                                StringWriter sw = new StringWriter();
                                if (bFirst && !bIsFunctionDecl && curSourceText != null)
                                {//The first element don't ident it just insert it a the right position
                                 //issue #892 => Anyway Handle splitting 
                                    sw.WriteLine(line.Text);
                                    bFirst = false;
                                    bInsertSplit = true;
                                }
                                else foreach (var l in Indent(line, null))
                                    {
                                        sw.WriteLine(l.Text.TrimEnd());
                                    }
                                sw.Flush();
                                string text = sw.ToString();
                                if (bIsFunctionDecl)
                                {   //This the Function Header output.
                                    LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[node_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                                    int f = Math.Min(from.Pos, curSourceText.Size);
                                    int t = Math.Min(to.Pos, curSourceText.Size);
                                    if (f != t)
                                    {
                                        //Create a the erase string to erase in the original source code
                                        //The Function header.
                                        //Erase in the original source code the Function header?
                                        ReplaceByBlanks(curSourceText, f, t);
                                        //Output the pre-stored comment header
                                        InsertLineMaybeSplit(funData.FunctionDeclBuffer, funData.CommentedHeader.ToString(), funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size, bInsertSplit);
                                    }
                                    //Insert the sequence
                                    InsertLineMaybeSplit(funData.FunctionDeclBuffer, text, funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size, bInsertSplit);
                                }
                                else
                                {
                                    if (curSourceText == null)
                                        InsertLineMaybeSplit(targetSourceText, text, targetSourceText.Size, targetSourceText.Size, bInsertSplit);
                                    else
                                        InsertLineMaybeSplit(curSourceText, text, Math.Min(from.Pos, curSourceText.Size), Math.Min(to.Pos, curSourceText.Size), bInsertSplit);
                                }
                                from = to;
                                sw.Close();
                            }
                        //Don't pad in case of replacement or insertion in a function declaration
                        if (!bIsGenerateAndReplace && !bIsFunctionDecl)
                        {
                            //Pad a splitted segment
                            if (mapper.Nodes[node_index].Positions != null)
                            {
                                int span = mapper.Nodes[node_index].Positions.Item3;
                                string pad = new string(' ', span);
                                curSourceText.Insert(pad, to.Pos, to.Pos);
                            }
                        }
                        if (bIsFunctionDecl)
                        {   //Switch in function declaration -> push the current buffers
                            LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[node_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                            stackLocalBuffer.Push(curSourceText);
                            stackOuterBuffer.Push(targetSourceText);
                            if (lmCtx != null)
                            {
                                stackLineMappingCtx.Push(lmCtx);
                                lmCtx = new LineMappingCtx(lmCtx.LineMapping, funData);
                            }
                            //Now Generate in Function Declaration Buffer.
                            targetSourceText = funData.FunctionDeclBuffer;
                            curSourceText = null;
                        }
                    }
                    //This node is now generated.
                    generated_node[node_index] = true;
                    if (mapper.Nodes[node_index].node.IsFlagSet(Node.Flag.EndFunctionDeclarationNode))
                    {   //Leaving function declaration --> Pop saved buffers.
                        if (previousBuffer != null)
                        {
                            if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                            {
                                AppendBufferContent(targetSourceText, previousBuffer);
                                lmCtx?.UpdateLineMap(i, targetSourceText, previousBuffer);
                            }
                            previousBuffer = null;
                        }
                        System.Diagnostics.Debug.Assert(stackOuterBuffer.Count > 0 && stackLocalBuffer.Count > 0);
                        if (stackOuterBuffer.Count > 0 && stackLocalBuffer.Count > 0)
                        {
                            targetSourceText = stackOuterBuffer.Pop();
                            curSourceText = (LineStringSourceText)stackLocalBuffer.Pop();
                            if (lmCtx != null)
                            {
                                lmCtx = stackLineMappingCtx.Pop();
                            }
                        }
                        previousBuffer = curSourceText;
                    }
                    else if (mapper.Nodes[node_index].node.CodeElement is Compiler.CodeElements.ProgramEnd)
                    {
                        //for each typecobol nested procedure declaration in procedure division
                        foreach (var functionDeclaration in mapper.Nodes[node_index].node.Parent.Children.OfType<FunctionDeclarationCG>().Where(c => c.GenerateAsNested))
                        {
                            //for each procedure generated in pgm
                            foreach (var functionIndex in mapper.FunctionDeclarationNodeIndices)
                            {
                                LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[functionIndex] as LinearNodeSourceCodeMapper.NodeFunctionData;
                                FunctionDeclarationCG function = funData.node as FunctionDeclarationCG;
                                if (funData.node.QualifiedName.Matches(functionDeclaration.QualifiedName) && 
                                    function?.OriginalHash != null && function.OriginalHash == functionDeclaration.OriginalHash)
                                {
                                    AppendBufferContent(targetSourceText, funData.FunctionDeclBuffer);
                                    lmCtx?.RelocateFunctionRanges(funData, funData.FunctionDeclBuffer, targetSourceText, true);
                                }
                            }
                        }

                        previousBuffer = curSourceText;
                    }
                    else
                    {
                        previousBuffer = curSourceText;
                    }
                }
                //--------------------------------------------------------------------------------------------------------------
            }
            //If there was a previous buffer ==> Flush it
            if (previousBuffer != null)
            {
                if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                {
                    AppendBufferContent(targetSourceText, previousBuffer);
                    lmCtx?.UpdateLineMap(mapper.LineData.Length, targetSourceText, previousBuffer);
                }
                previousBuffer = null;
            }            
            //--------------------------------------------------------------------------------------------------------------
            //4)//Flush of Function declaration body that shouldn't be generated as nested
            foreach (int fun_index in mapper.FunctionDeclarationNodeIndices)
            {
                FunctionDeclarationCG functionDeclaration = mapper.Nodes[fun_index].node as FunctionDeclarationCG;
                if (functionDeclaration != null && !functionDeclaration.GenerateAsNested)
                {
                    LinearNodeSourceCodeMapper.NodeFunctionData funData =
                        mapper.Nodes[fun_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                    AppendBufferContent(targetSourceText, funData.FunctionDeclBuffer);
                    lmCtx?.RelocateFunctionRanges(funData, funData.FunctionDeclBuffer, targetSourceText, false);
                }
            }
            ////5)//Generate stacked program for the global-storage section
            //if (mapper.UseGlobalStorageSection)
            //{
            //    StringWriter sw = new StringWriter();                
            //    //Uncomment all Global Storages
            //    (new Comment(this.RootNode.GlobalStorageProgram, false)).Execute();
            //    var Actions = new GeneratorActions(this, null, this.CompilationResults, new TypeCobol.Codegen.Actions.Skeletons());

            //    foreach (var gs in this.RootNode.GlobalStorageProgram.Children)
            //    {
            //        gs.SetFlag(Node.Flag.IgnoreCommentAction, true);
            //        gs.SetFlag(Node.Flag.GeneratorErasedNode, false, true);
            //        Actions.Perform(gs);
            //    }
            //    var lines = this.RootNode.GlobalStorageProgram.Lines;
            //    foreach (var line in lines)
            //    {
            //        foreach (var l in Indent(line, null))
            //        {
            //            sw.WriteLine(l.Text.TrimEnd());
            //        }
            //    }
            //    InsertLineMaybeSplit(targetSourceText, sw.ToString(), targetSourceText.Size, targetSourceText.Size, false);
            //}

            //6)//Generate Line Exceed Diagnostics
                GenerateExceedLineDiagnostics();
            return targetSourceText;
        }

        /// <summary>
        /// Insert in the buffer a text line that can be split.
        /// </summary>
        /// <param name="text">The text</param>
        /// <param name="from">from position in the buffer</param>
        /// <param name="to">to position in the buffer</param>
        /// <param name="bInsertSplit">true if splitting must handle, false otherwise</param>
        private void InsertLineMaybeSplit(SourceText buffer, string text, int from, int to, bool bInsertSplit)
        {
            if (bInsertSplit && this.Layout == ColumnsLayout.CobolReferenceFormat)
            {
                int crPos = text.LastIndexOf('\r');
                int lfPos = text.LastIndexOf('\n');
                int crlf = 0;
                crlf += crPos >= 0 ? 1 : 0;
                crlf += lfPos >= 0 ? 1 : 0;
                int lineStartOffset;
                int lineEndOffset;
                int lineLen = buffer.GetLineInfo(from, out lineStartOffset, out lineEndOffset);
                if (((from - lineStartOffset) + (text.Length - crlf)) >= LEGAL_COBOL_LINE_LENGTH)
                {
                    string lefttext = buffer.GetTextAt(lineStartOffset, from);
                    ICollection<ITextLine> lines = CobolTextLine.CreateCobolLines(this.Layout, -1, ' ', "",
                        lefttext + (crlf > 0 ? text.Substring(0, text.Length - crlf) : text), LEGAL_COBOL_LINE_LENGTH, 65, false);
                    StringWriter sw = new StringWriter();
                    foreach (var line in lines)
                    {
                        sw.WriteLine(line.Text);
                    }
                    //We must insert "\r\n" if the target line is empty and the inserted test has one.
                    if ((lineEndOffset == lineStartOffset) && crlf > 0)
                    {
                        sw.Write(Environment.NewLine);
                    }
                    sw.Flush();
                    text = sw.ToString();
                    buffer.Insert(text, lineStartOffset, to);
                }
                else
                {
                    buffer.Insert(text, from, to);
                }
            }
            else
            {
                buffer.Insert(text, from, to);
            }
        }

        /// <summary>
        /// Determine if a position can be split without overflow.
        /// </summary>
        /// <param name="firstSplitablePos"></param>
        /// <param name="lineLength"></param>
        /// <param name="originalPos"></param>
        /// <param name="resultColumn"></param>
        /// <returns></returns>
        public bool IsPlitablePosition(int firstSplitablePos, int lineLength, int originalPos, out int resultColumn)
        {
            resultColumn = MIN_SPLIT_COLUMN - 1;
            if ((firstSplitablePos + lineLength - originalPos) < LEGAL_COBOL_LINE_LENGTH)
            {
                resultColumn = firstSplitablePos;
                return true;
            }
            else if (((MIN_SPLIT_COLUMN - 1) + lineLength - originalPos) < LEGAL_COBOL_LINE_LENGTH)
            {
                resultColumn = firstSplitablePos;
                return true;
            }
            return false;
        }

        /// <summary>
        /// Append a source buffer at the end of a target buffer
        /// </summary>
        /// <param name="dstBuffer"></param>
        /// <param name="srcBuffer"></param>
        private void AppendBufferContent(SourceText dstBuffer, SourceText srcBuffer)
        {
            if (BufferExceedMap != null)
            {
                if (BufferExceedMap.ContainsKey(srcBuffer))
                {//Create all start positions in the buffer.

                    //originalPosition keep the original position of the position so that we can calculate
                    //the exceed length, for the minimal spliting.
                    Dictionary<Position, int> originalPositions = new Dictionary<Position, int>();
                    foreach (var linePos in BufferExceedMap[srcBuffer])
                    {
                        foreach (var start in linePos.Value)
                        {
                            originalPositions[start] = start.Pos;
                            srcBuffer.AddPosition(start);
                        }
                    }
                    bool bLineStillTooLong = false;
                    //Split each exceeded line based on the right most exceed position.
                    foreach (var linePos in BufferExceedMap[srcBuffer])
                    {
                        List<Position> splittingPositionStack = new List<Position>();
                        int lineNumber = linePos.Key;
                        Tuple<int, int> lineStartLength = ExceedLines[lineNumber];
                        int lineStartPos = lineStartLength.Item1;
                        int lineLength = lineStartLength.Item2;
                        int lastEndOrgPos = lineStartPos + lineLength;
                        int firstSplitablePos = originalPositions[linePos.Value[0]];
                        int targeSplittColumn = 0;
                        for (int i = linePos.Value.Count - 1; i >= 0; i--)
                        {
                            Position start = linePos.Value[i];
                            int originalPos = originalPositions[start];

                            if (IsPlitablePosition(firstSplitablePos, lineLength, originalPos, out targeSplittColumn))
                            {   //we can split here => so check if the rest at the left still exceed
                                //Push this splitting position
                                splittingPositionStack.Add(start);
                                if ((originalPos - lineStartPos) < LEGAL_COBOL_LINE_LENGTH)
                                {//The line is till too long?
                                    break; //No ==> Stop splitting this line
                                }
                            }
                            else
                            {//This means that the line will stil too long.
                                bLineStillTooLong = true;
                            }
                        }
                        if (splittingPositionStack.Count > 0)
                        {//This line can be splitted so split it.                            
                            int rightPos = lineLength;
                            for (int i = splittingPositionStack.Count - 1; i >= 0; i--)
                            {
                                Position start = splittingPositionStack[i];
                                int originalPos = originalPositions[start];
                                for (int j = 0; j <= i; j++)
                                {
                                    if (IsPlitablePosition(firstSplitablePos, rightPos, originalPos, out targeSplittColumn))
                                    {
                                        int delta = (lineStartPos + lineLength) - originalPos;

                                        while ((targeSplittColumn - lineStartPos + delta) > LEGAL_COBOL_LINE_LENGTH)
                                        {
                                            targeSplittColumn--;
                                        }
                                        if ((targeSplittColumn - lineStartPos) >= (MIN_SPLIT_COLUMN - 1))
                                        {
                                            string pad = new StringBuilder().Append(Environment.NewLine).Append(new string(' ', Math.Abs(targeSplittColumn - lineStartPos))).ToString();
                                            //So break here
                                            srcBuffer.Insert(pad, start.Pos, start.Pos);
                                            i = j;
                                            break;
                                        }
                                    }
                                    else
                                    {
                                        Position jstart = splittingPositionStack[j];
                                        rightPos = originalPositions[jstart];
                                    }
                                }
                            }
                            //Remove it from exceed, if the line has been correctly splitted
                            if (!bLineStillTooLong)
                            {
                                ExceedLines.Remove(lineNumber);
                            }
                        }
                    }
                }
            }
            int pos = dstBuffer.Size;
            dstBuffer.Insert(srcBuffer, pos, pos);
        }

        /// <summary>
        /// Insert a code in a buffer at given position, and check if line exceed int Cobol ReferenceFormat
        /// </summary>
        /// <param name="from">The from position int the buffer</param>
        /// <param name="to">The to position in the buffer</param>
        /// <param name="buffer">The target buffer</param>
        /// <param name="code">The code to insert</param>
        /// <param name="lineNumber">The current lien number</param>
        private void GenerateIntoBufferCheckLineExceed(Position from, Position to, SourceText buffer, string code, int lineNumber)
        {   //Lines_73_80_Map
            int start = Math.Min(from.Pos, buffer.Size);
            int end = Math.Min(to.Pos, buffer.Size);
            if (this.Layout != ColumnsLayout.CobolReferenceFormat)
            {//Maybe Free Format
                buffer.Insert(code, start, end);
                return;
            }
            int lineLen = -1;
            int lineStartOffset = -1;
            int lineEndOffset = -1;
            if (ExceedLines != null)
            {
                ExceedLines.Remove(lineNumber);
            }
            lineLen = buffer.GetLineInfo(start, out lineStartOffset, out lineEndOffset);
            if (!Lines_73_80_Flags.Contains(lineNumber))
            {//Replace by spaces any characters in columns[73-80]
                Lines_73_80_Flags.Add(lineNumber);
                if (lineLen > LEGAL_COBOL_LINE_LENGTH)
                {
                    int replace_len = lineLen - LEGAL_COBOL_LINE_LENGTH;
                    buffer.Insert(new string(' ', replace_len), lineStartOffset + LEGAL_COBOL_LINE_LENGTH, lineStartOffset + lineLen);
                }
            }
            buffer.Insert(code, start, end);
            int delta = -(end - start) + code.Length;
            int newLineLen = lineLen + delta;
            bool newHas73Chars = false;
            if (newLineLen > LEGAL_COBOL_LINE_LENGTH)
            {
                for (int k = LEGAL_COBOL_LINE_LENGTH; k < newLineLen & !newHas73Chars; k++)
                {
                    char ch = buffer[lineStartOffset + k];
                    newHas73Chars = !(ch == '\r' || ch == '\n' || Char.IsWhiteSpace(ch));
                }
                //Error
                //Emit an error.
                if (newHas73Chars)
                {
                    if (ExceedLines == null)
                    {
                        ExceedLines = new Dictionary<int, Tuple<int, int>>();
                    }
                    ExceedLines.Add(lineNumber, new Tuple<int, int>(lineStartOffset, newLineLen));
                }
                else if (newLineLen > MAX_COBOL_LINE_LENGTH)
                {//Here we know that the line the line exceed with only white characters. ==> Remove extra white characters
                    buffer.Delete(lineStartOffset + LEGAL_COBOL_LINE_LENGTH, lineStartOffset + newLineLen);
                }
            }

            //Hanlde Buffer Exceed Map.
            if (ExceedLines != null && ExceedLines.ContainsKey(lineNumber))
            {
                if (BufferExceedMap == null)
                {
                    BufferExceedMap = new Dictionary<SourceText, Dictionary<int, List<Position>>>();
                }
                if (!BufferExceedMap.ContainsKey(buffer))
                {
                    BufferExceedMap[buffer] = new Dictionary<int, List<Position>>();
                }
                if (!BufferExceedMap[buffer].ContainsKey(lineNumber))
                {
                    BufferExceedMap[buffer][lineNumber] = new List<Position>();
                }
                //Add the position at the code added
                BufferExceedMap[buffer][lineNumber].Add(new Position(start));
                //Add the position after the code added
                BufferExceedMap[buffer][lineNumber].Add(new Position(start + code.Length));
            }
            else
            {//Remove any line reference in the buffer exceed map.
                if (BufferExceedMap != null)
                {
                    if (BufferExceedMap.ContainsKey(buffer))
                    {
                        if (BufferExceedMap[buffer].ContainsKey(lineNumber))
                        {
                            BufferExceedMap[buffer].Remove(lineNumber);
                            if (BufferExceedMap[buffer].Count == 0)
                            {
                                BufferExceedMap.Remove(buffer);
                            }
                        }
                    }
                }
            }

        }

        /// <summary>
        /// Generate Line Exceed diagnotics
        /// </summary>
        private void GenerateExceedLineDiagnostics()
        {
            if (ExceedLines != null)
            {
                foreach (int lineNumber in ExceedLines.Keys)
                {
                    Diagnostic diag = new Diagnostic(MessageCode.GenerationErrorLineExceed, 0, 0, lineNumber);
                    AddDiagnostic(diag);
                }
            }
        }

        /// <summary>
        /// Get the Lines gnerated for a Node.
        /// </summary>
        /// <param name="node">The node to get the lines</param>
        /// <returns>The Node's lines</returns>
        IEnumerable<ITextLine> NodeLines(Node node)
        {
            node.Layout = Layout;
            return node.Lines;
        }

        /// <summary>
        /// Recursively get all lines of a node and its children.
        /// </summary>
        /// <param name="node">The node to get all line</param>
        /// <param name="all_lines">All line accumulator</param>
        void RecursiveNodeLines(Node node, BitArray generated_node, List<ITextLine> all_lines)
        {
            foreach (var l in NodeLines(node))
                all_lines.Add(l);
            foreach (Node child in node.Children)
            {
                if (child.NodeIndex >= 0)
                    generated_node[child.NodeIndex] = true;
                RecursiveNodeLines(child, generated_node, all_lines);
            }
        }
        /// <summary>
        /// Get all lines to be Generated by a Node. If the Node was inserted by a factory then
        /// all its children as also generated by a factory, they are generated with it.
        /// </summary>
        /// <param name="node"></param>
        /// <param name="generated_node"></param>
        /// <returns></returns>
        public virtual IEnumerable<ITextLine> NodeLines(Node node, BitArray generated_node)
        {
            if (node.IsFlagSet(Node.Flag.FullyGenerateRecursivelyFactoryGeneratedNode))
            {
                List<ITextLine> all_lines = new List<ITextLine>();
                RecursiveNodeLines(node, generated_node, all_lines);
                foreach (var l in all_lines)
                    yield return l;
            }
            else
            {
                foreach (var l in NodeLines(node))
                    yield return l;
                if (node.IsFlagSet(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex))
                {
                    //All its Children that are not inserted at a specific index are generated with it
                    foreach (Node child in node.Children)
                    {
                        if (child.IsFlagSet(Node.Flag.FactoryGeneratedNode) &&
                            (!child.IsFlagSet(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex)))
                        {
                            if (child.NodeIndex >= 0)
                                generated_node[child.NodeIndex] = true;
                            foreach (var cl in NodeLines(child))
                            {
                                yield return cl;
                            }
                        }
                    }
                }
            }
        }
        /// <summary>
        /// Check if the given commented text is one of a Function declaration Header, if so the text is added to the
        /// Function Data commented header string buffer.
        /// </summary>
        /// <param name="nodes">The nodes indices that can correspond to afUnction Declaration node</param>
        /// <param name="text">The Commented Text</param>
        private void CheckFunctionDeclCommentedheader(LinearNodeSourceCodeMapper mapper, List<int> nodes, string text)
        {
            if (nodes == null)
                return;
            if (text == null)
                return;
            if (text.Length == 0)
                return;
            foreach (int node in nodes)
            {
                if (node >= 0 && mapper.Nodes[node] is LinearNodeSourceCodeMapper.NodeFunctionData)
                {
                    LinearNodeSourceCodeMapper.NodeFunctionData fundata = (LinearNodeSourceCodeMapper.NodeFunctionData)mapper.Nodes[node];
                    if (fundata.CommentedHeader.Length == 0)
                    {//Add a first empty comment line
                        TextLineSnapshot h = new TextLineSnapshot(-1, "*", null);
                        string crlf = "";
                        foreach (var hl in Indent(h, null))
                        {
                            fundata.CommentedHeader.Append(crlf);
                            fundata.CommentedHeader.Append(hl.Text.TrimEnd());
                            crlf = Environment.NewLine;
                        }
                        fundata.CommentedHeader.Append(crlf);
                    }
                    fundata.CommentedHeader.Append(text);
                    fundata.CommentedHeader.Append(Environment.NewLine);
                }
            }
        }

        /// <summary>
        /// Create a Delete string Corresponding to a segment in a buffer.
        /// We want to create a replacement string that only contains whitespaces
        /// and '\r' or \n' characters.
        /// </summary>
        /// <param name="curSourceText"></param>
        /// <param name="from">The start position in the buffer</param>
        /// <param name="to">The ending position in the buffer</param>
        /// <returns>The replacement characters</returns>
        protected char[] GetDeleteString(StringSourceText sourceText, int from, int to)
        {
            char[] result = new char[to - from];
            for (int i = 0; i < result.Length; i++)
            {
                char c = sourceText[from + i];
                result[i] = (c == '\r' || c == '\n' || Char.IsWhiteSpace(c)) ? c : ' ';
            }
            return result;
        }

        /// <summary>
        /// Directly replace the the corresponding Segment by Blanks, preserving '\r' and '\n'
        /// characters.
        /// </summary>
        /// <param name="curSourceText"></param>
        /// <param name="from">The start position in the buffer</param>
        /// <param name="to">The ending position in the buffer</param>
        private void ReplaceByBlanks(StringSourceText sourceText, int from, int to)
        {
            for (int i = from; i < to; i++)
            {
                char c = sourceText[i];
                sourceText[i] = (c == '\r' || c == '\n' || Char.IsWhiteSpace(c)) ? c : ' ';
            }
        }

        /// <summary>
        /// Does nothing
        /// </summary>
        /// <param name="node">Target node</param>
        /// <returns>return false</returns>
        protected override bool Process(Compiler.Nodes.Node node)
        {
            return false;
        }

        /// <summary>
        /// Produce an indented version of a text line. The indentation depends on the current Layout format
        /// ColumnsLayout.CobolReferenceFormat or ColumnsLayout.FreeTextFormat.
        /// </summary>
        /// <param name="line">The text line to be produced an indented version</param>
        /// <param name="isComment">if null then this is the identity function, if true a commented line is produced, otherwise an uncommented line is produced.</param>
        /// <returns>The idented line</returns>
        private IEnumerable<ITextLine> Indent(ITextLine line, bool? isComment)
        {
            var results = new List<ITextLine>();
            var cobol = line as CobolTextLine;
            if (cobol != null)
            {
                if (Layout == ColumnsLayout.CobolReferenceFormat)
                {
                    results.Add(SetComment(line, isComment));
                }
                else
                    if (Layout == ColumnsLayout.FreeTextFormat)
                {
                    results.Add(SetComment(new TextLineSnapshot(-1, cobol.SourceText ?? "", null), isComment));
                }
                else
                    throw new System.NotImplementedException("Unsuported columns layout: " + Layout);
            }
            else
            {
                if (Layout == ColumnsLayout.CobolReferenceFormat)
                {
                    var lines = CobolTextLine.Create(line.Text, Layout, line.LineIndex);
                    foreach (var l in lines) results.Add(SetComment(l, isComment));
                }
                else if (Layout == ColumnsLayout.FreeTextFormat)
                {
                    results.Add(SetComment(line, isComment));
                }
                else
                    throw new System.NotImplementedException("Unsuported columns layout: " + Layout);
            }
            if (results.Count < 1)
                throw new System.NotImplementedException("Unsuported ITextLine type: " + line.GetType());
            return results;
        }

        /// <summary>
        /// Produces a commented or an uncommeneted text line from a text line
        /// </summary>
        /// <param name="line">the line</param>
        /// <param name="isComment">if null then this is the identity function, if true a commented line is produced, otherwise an uncommented line is produced.</param>
        /// <returns>if isComment is null the same line is return, if true a commneted line is returned otherwise an uncommented line</returns>
        private static ITextLine SetComment(ITextLine line, bool? isComment)
        {
            if (isComment == true)
                return Comment(line);
            else
                if (isComment == false)
                return Uncomment(line);
            else // null
                return line;
        }

        /// <summary>
        /// Produces a commented text line of a text line
        /// </summary>
        /// <param name="line">The text line to be procuded a commented text line </param>
        /// <returns>The commente dtext line</returns>
        private static ITextLine Comment(ITextLine line)
        {
            var cobol = line as CobolTextLine;
            if (cobol != null)
            {
                StringBuilder text = new StringBuilder(cobol.Text);
                if (text.Length > 6)
                    text[6] = '*';
                var lines = CobolTextLine.Create("*" + cobol.SourceText, cobol.ColumnsLayout, cobol.LineIndex);
                foreach (var l in lines) return l;// there's only one in the collection
                throw new System.NotImplementedException("I should have at least one item!");
            }
            else
            {
                return new TextLineSnapshot(line.LineIndex, "*" + line.Text, null);
            }
        }

        /// <summary>
        /// Produces an uncommented text line from a commented text line
        /// </summary>
        /// <param name="line">The text line to produce the uncommented text line.</param>
        /// <returns>The uncommented text line</returns>
        private static ITextLine Uncomment(ITextLine line)
        {
            var cobol = line as CobolTextLine;
            if (cobol != null)
            {
                StringBuilder text = new StringBuilder(cobol.Text);
                text[6] = ' ';
                var lines = CobolTextLine.Create(text.ToString(), cobol.ColumnsLayout, cobol.LineIndex);
                foreach (var l in lines)
                    return l;// there's only one in the collection
                throw new System.NotImplementedException("I should have at least one item!");
            }
            else
            {
                StringBuilder text = new StringBuilder(line.Text);
                int index = line.Text.IndexOf('*');
                text[index] = ' ';
                return new TextLineSnapshot(line.LineIndex, text.ToString(), null);
            }
        }
    }
}
