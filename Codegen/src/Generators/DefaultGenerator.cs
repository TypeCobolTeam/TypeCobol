using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;
using System.Runtime.CompilerServices;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;

using static TypeCobol.Codegen.Generators.LinearNodeSourceCodeMapper;

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
        /// <param name="document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="typeCobolVersion">Current version of the TypeCobol parser/codegen</param>
        public DefaultGenerator(TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, string typeCobolVersion)
            : base(document, destination, typeCobolVersion)
        {

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
            //If the node has Cloned node create a second linear node source code mapper and visit each cloned node.
            LinearNodeSourceCodeMapper clonedMapper = null;
            if (ClonedNodes.Count > 0)
            {
                clonedMapper = new LinearNodeSourceCodeMapper(this);
                clonedMapper.LinearMode = Mode.Cloned;
                foreach (var cloned in ClonedNodes)
                {
                    clonedMapper.Accept(cloned);
                }
            }
            SourceText generatedDocument = LinearGeneration(mapper, CompilationResults.TokensLines, clonedMapper);
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
            /// The 1-based, Delta of the Global Storage line in this Line Mapping if any.
            /// </summary>
            public List<int> AllGlobalStorageLineDelta;
            /// <summary>
            /// The Inverse Line Mappling list if any. The inverse line mapping is a line number
            /// from the output source to original source code.
            /// </summary>
            public List<Tuple<int, int>> InverseLineMapping;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="lineMap"></param>
            public LineMappingCtx(Tuple<int, int>[] lineMap)
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
                //If we have GlobalStorage relocate it also
                if (funData.GlobalStorageLineDelta != 0)
                {
                    funData.GlobalStorageLineDelta += startLineMapCounter;
                    AddGlobalStorageLineDelta(funData.GlobalStorageLineDelta);
                }

                //We update the new position.
                startLineMapCounter += interval.Item2 - interval.Item1 + 1;
                if (bNested)
                {
                    currentGenLineOffset = mainSourceText.Size;
                }
            }

            /// <summary>
            /// Add a Global Storage declaration delta from the Main Program or a Nested Program.
            /// </summary>
            /// <param name="delta">The delta to be added</param>
            internal void AddGlobalStorageLineDelta(int delta)
            {
                if (this.AllGlobalStorageLineDelta == null)
                    this.AllGlobalStorageLineDelta = new List<int>();
                this.AllGlobalStorageLineDelta.Add(delta);
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
        /// <param name="clonedMapper">Linear mapper for cloned nodes</param>
        /// <returns>The Generated Source Document</returns>
        /// </summary>
        protected virtual SourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, IReadOnlyList<A> Input, LinearNodeSourceCodeMapper clonedMapper = null) where A : ITextLine
        {
            return LinearGeneration<A>(mapper, clonedMapper, Input, null, 0, mapper.LineData.Length);
        }

        /// <summary>
        /// /// Perform a linear Generation
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <param name="mapper">The linearization representation</param>
        /// <param name="clonedMapper">Linear mapper for cloned nodes</param>
        /// <param name="Input">Input source lines</param>
        /// <param name="startLine">The starting line (0 based)</param>
        /// <param name="endLine">The ending line (0- based and excluded)</param>
        /// <returns>The Generated Source Document</returns>
        protected SourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, LinearNodeSourceCodeMapper clonedMapper, IReadOnlyList<A> Input, LineMappingCtx lmCtx, int startLine, int endLine) where A : ITextLine
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

            //First of all compute any Global Storage Data
            if (clonedMapper != null && mapper.UseGlobalStorageSection && clonedMapper.ClonedGlobalStorageSection != null)
            {
                GetGlobalStorageData(clonedMapper, Input, lmCtx);
            }

            for (int i = startLine; i < endLine; i++)
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
                for (int j = i; j < endLine && mapper.CommentedLines[j]; j++)
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
                foreach (int node_index in line_nodes)
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
                        else foreach (var line in NodeLines(node, generated_node, curSourceText ?? targetSourceText, curSourceText == null))
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
                                int span = mapper.Nodes[node_index].Positions.Span;
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
                                CurrentLineMappinCtx = lmCtx;
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
                                CurrentLineMappinCtx = lmCtx;
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
            //5)//Generate stacked program for the global-storage section
            if (clonedMapper != null && mapper.UseGlobalStorageSection && clonedMapper.ClonedGlobalStorageSection != null)
            {
                string gsSrcPrg = GenerateGlobalStorageSectionStackedProgram(clonedMapper, Input, lmCtx);
                InsertLineMaybeSplit(targetSourceText, gsSrcPrg.ToString(), targetSourceText.Size, targetSourceText.Size, false);
            }

            //6)//Generate Line Exceed Diagnostics
            GenerateExceedLineDiagnostics();
            return targetSourceText;
        }

        /// <summary>
        /// Check if this is a token to skip while parsing Global-Storage section.
        /// Also handle scan state properties.
        /// </summary>
        /// <param name="t"></param>
        /// <returns></returns>
        private void AdvanceToNextStateAndAdjustTokenProperties(TokensLine tempTokensLine, Token t)
        {
            if (t != null && base.Layout == ColumnsLayout.CobolReferenceFormat && (t.Column >= 73 || t.Column <= 6))
                return;//Don't change the state for tokens inside unused columns if we are in cobol reference mode

            bool bSkip = t == null || t.TokenFamily == TokenFamily.Whitespace || t.TokenFamily == TokenFamily.Comments || (t.TokenType == TokenType.UserDefinedWord && t.Text.Trim().Length == 0);
            if (!bSkip)
            {
                tempTokensLine.ScanState.AdvanceToNextStateAndAdjustTokenProperties(t);
                if (t.TokenType == TokenType.PictureCharacterString)
                {//Strange case a Picture string can end with a '.' in this case reset last Significant token as if it was a PeriodSeparator.
                    if (t.Text.Trim().EndsWith("."))
                    {
                        tempTokensLine.ScanState.LastSignificantToken = null;
                        tempTokensLine.ScanState.BeforeLastSignificantToken = null;
                    }
                }
            }
        }
        /// <summary>
        /// Generates the Stacked program that declares all Global Storage Variables.
        /// </summary>
        /// <param name="clonedMapper">The Linear Source Code Mapper contains the GlobalStorage Section.</param>
        /// <param name="Input">The list of program input lines</param>
        /// <param name="lmCtx">Lime Mapping context</param>
        /// <returns>The Whole Staked Program source code</returns>
        private string GenerateGlobalStorageSectionStackedProgram<A>(LinearNodeSourceCodeMapper clonedMapper, IReadOnlyList<A> Input, LineMappingCtx lmCtx) where A : ITextLine
        {
            StringWriter sw = new StringWriter();

            sw.WriteLine("      *"); GSLineOffset += 1;
            sw.WriteLine("      * Global Storage Section variables"); GSLineOffset += 1;
            sw.WriteLine("      *_________________________________________________________________"); GSLineOffset += 1;
            sw.WriteLine("       IDENTIFICATION DIVISION."); GSLineOffset += 1;
            sw.WriteLine($"       PROGRAM-ID. {RootNode.MainProgram.Hash}."); GSLineOffset += 1;

            if (CompilationResults is CompilationUnit cu)
            {
                var environmentDiv = cu.ProgramClassDocumentSnapshot.Root.MainProgram.Children.OfType<EnvironmentDivision>().SingleOrDefault();
                if (environmentDiv != null)
                {
                    // header of EnvironmentDivision
                    var lines = environmentDiv.Lines;
                    // header and content of ConfigurationSection only.
                    lines = lines.Concat(environmentDiv.Children.OfType<ConfigurationSection>().SelectMany(c => c.SelfAndChildrenLines));

                    foreach (var line in lines)
                    {
                        sw.WriteLine(line.Text);
                        GSLineOffset++;
                    }
                }
            }

            sw.WriteLine("       DATA DIVISION."); GSLineOffset += 1;
            sw.WriteLine("       WORKING-STORAGE SECTION."); GSLineOffset += 1;
            sw.WriteLine("       01 PIC X(8) value ':TC:GBLS'."); GSLineOffset += 1;

            sw.WriteLine(GetGlobalStorageData(clonedMapper, Input, lmCtx));
            //So Here we can relocate Global Storage Line Mapping
            if (lmCtx != null)
            {
                for (int i = FirstGSLine; i < LastGSLine; i++)
                {
                    int firstLine = GSLmCtx.LineMapping[i].Item1;
                    int lastLine = GSLmCtx.LineMapping[i].Item2;
                    lmCtx.LineMapping[i] = new Tuple<int, int>(firstLine + lmCtx.startLineMapCounter + GSLineOffset - 1, lastLine + lmCtx.startLineMapCounter + GSLineOffset - 1);
                }

                //Create also the inverse line mapping for Global Storage generated in nested programs and procedures
                if (lmCtx.AllGlobalStorageLineDelta != null)
                {
                    lmCtx.InverseLineMapping = new List<Tuple<int, int>>();
                    foreach (int gsDelta in lmCtx.AllGlobalStorageLineDelta)
                    {
                        int baseLine = GSLmCtx.LineMapping[FirstGSLine].Item1 + lmCtx.startLineMapCounter + GSLineOffset - 1;
                        for (int i = FirstGSLine; i < LastGSLine; i++)
                        {
                            int firstLine = GSLmCtx.LineMapping[i].Item1 + lmCtx.startLineMapCounter + GSLineOffset - 1;
                            int lastLine = GSLmCtx.LineMapping[i].Item2 + lmCtx.startLineMapCounter + GSLineOffset - 1;
                            for (int j = firstLine; j <= lastLine; j++)
                            {
                                Tuple<int, int> inv = new Tuple<int, int>(-(j - baseLine + gsDelta + GSLineOffsetInverse), i + 1);
                                lmCtx.InverseLineMapping.Add(inv);
                            }
                        }
                    }
                }
            }

            sw.WriteLine("       LINKAGE SECTION.");
            sw.WriteLine("       01 GlobalPointer pointer.");
            sw.WriteLine("       PROCEDURE DIVISION USING BY REFERENCE GlobalPointer.");
            sw.WriteLine("           set GlobalPointer to address of TC-GlobalData");
            sw.WriteLine("           .");
            sw.WriteLine($"       END PROGRAM {RootNode.MainProgram.Hash}.");

            return sw.ToString();
        }

        public string GlobalStorageData = null;
        /// <summary>
        /// Global storage Line Mapping Context
        /// </summary>
        LineMappingCtx GSLmCtx = null;
        /// <summary>
        /// The first Global Storage Line
        /// </summary>
        int FirstGSLine;
        /// <summary>
        /// The Last Global Storage Line.
        /// </summary>
        int LastGSLine;
        /// <summary>
        /// The Offset to apply to relocate GlobalStore Line Mapping.
        /// </summary>
        int GSLineOffset;
        /// <summary>
        /// The Offset to apply to relocate GlobalStore Line Mapping for the inverse Line Mapping.
        /// </summary>
        int GSLineOffsetInverse;
        /// <summary>
        /// The Current Line Mapping context.
        /// </summary>
        public LineMappingCtx CurrentLineMappinCtx { get; protected set; }

        /// <summary>
        /// Get the content of the TC-GlobalData structure.
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <param name="clonedMapper"></param>
        /// <param name="Input"></param>
        /// <param name="lmCtx"></param>
        /// <returns></returns>
        public string GetGlobalStorageData<A>(LinearNodeSourceCodeMapper clonedMapper, IReadOnlyList<A> Input, LineMappingCtx lmCtx) where A : ITextLine
        {
            if (GlobalStorageData != null)
                return GlobalStorageData;

            GSLineOffset = 0;
            GSLineOffsetInverse = 0;
            StringWriter sw = new StringWriter();
            sw.WriteLine("       01 TC-GlobalData.");
            GSLineOffset += 1;//IMPORTANT Update GS relocation offset ==> one more line.
            GSLineOffsetInverse += 1;


            //Compute the last line of the Global Storage Node.
            int lastLine = -1;
            Node lastNode = null;
            clonedMapper.GetAfterLinearizationLastLine(clonedMapper.ClonedGlobalStorageSection, ref lastLine, ref lastNode);

            if (lmCtx != null)
            {
                GSLmCtx = new LineMappingCtx(new Tuple<int, int>[lmCtx.LineMapping.Length]);
            }
            SourceText gsSrcText = LinearGeneration(clonedMapper, null, Input, GSLmCtx, FirstGSLine = clonedMapper.ClonedGlobalStorageSection.CodeElement.Line - 1,
                LastGSLine = lastLine);
            //Take interessting scan state values from the original input
            TypeCobol.Compiler.Parser.CodeElementsLine cel = Input[FirstGSLine] as TypeCobol.Compiler.Parser.CodeElementsLine;
            //first true argument => we are in a DataDivision.
            System.Diagnostics.Debug.Assert(cel.ScanState.InsideDataDivision);
            string gsText = gsSrcText.GetTextAt(0, gsSrcText.Size);
            using (StringReader sr = new StringReader(gsText))
            {
                string line;
                while ((line = sr.ReadLine()) != null)
                {
                    //Allocate a scanner to reparse and change increments level
                    TokensLine tempTokensLine = new TokensLine(
                        new TextLineSnapshot(0, line, null),
                        base.Layout);

                    int startIndex = 0;
                    if (tempTokensLine.Type == CobolTextLineType.Debug && cel.ScanState.WithDebuggingMode)
                    {//Handling special debugging mode.
                        int minIndCol = base.Layout == ColumnsLayout.CobolReferenceFormat ? 6 : 0;
                        int maxIndCol = base.Layout == ColumnsLayout.CobolReferenceFormat ? 7 : 2;
                        System.Diagnostics.Debug.Assert(line.Length >= maxIndCol && (line[minIndCol] == 'D' || line[minIndCol] == 'd'));
                        if (line.Length >= maxIndCol && (line[minIndCol] == 'D' || line[minIndCol] == 'd'))
                        {
                            if ((base.Layout == ColumnsLayout.CobolReferenceFormat) || (line[minIndCol + 1] == ' '))
                            {
                                startIndex = maxIndCol;
                                sw.Write(line.Substring(0, maxIndCol));
                            }
                        }
                    }

                    tempTokensLine.InitializeScanState(cel.ScanState);
                    Scanner scanner = new Scanner(line, startIndex, line.Length - 1, tempTokensLine, CompilationResults.CompilerOptions, true);
                    Token t = null;

                    while ((t = scanner.GetNextToken()) != null)
                    {
                        if (t.TokenType == TokenType.GLOBAL_STORAGE)
                        {//Skip and remove GLOBAL-STORAGE SECTION. tokens by generating spaces instead.
                            sw.Write(new System.String(' ', t.Text.Length));
                            while ((t = scanner.GetNextToken()) != null && t.TokenType != TokenType.PeriodSeparator)
                            {
                                sw.Write(new System.String(' ', t.Text.Length));
                                AdvanceToNextStateAndAdjustTokenProperties(tempTokensLine, t);
                            }
                            if (t != null)
                                sw.Write(new System.String(' ', t.Text.Length));
                            AdvanceToNextStateAndAdjustTokenProperties(tempTokensLine, t);
                            continue;
                        }
                        else if (t.TokenType == TokenType.LevelNumber && base.Layout == ColumnsLayout.CobolReferenceFormat && t.Column > 6 && t.Column < 73)
                        {
                            TypeCobol.Compiler.Scanner.IntegerLiteralTokenValue intValue =
                                (TypeCobol.Compiler.Scanner.IntegerLiteralTokenValue)t.LiteralValue;
                            long level = (long)intValue.Number + 1;
                            if (level <= 49)
                            {
                                sw.Write(level.ToString("00"));
                            }
                            else
                            {
                                sw.Write(t.Text);
                            }
                        }
                        else
                        {
                            sw.Write(t.Text);
                        }
                        AdvanceToNextStateAndAdjustTokenProperties(tempTokensLine, t);
                    }
                    sw.WriteLine();
                }
            }

            GlobalStorageData = sw.ToString();
            return GlobalStorageData;
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
                    ICollection<ITextLine> lines = CobolTextLine.CreateCobolLines(this.Layout, CompilationResults.CompilerOptions, -1, ' ', "",
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
                    Diagnostic diag = new Diagnostic(MessageCode.GenerationErrorLineExceed, new Diagnostic.Position(lineNumber, 0, lineNumber, 0, null));
                    AddDiagnostic(diag);
                }
            }
        }

        /// <summary>
        /// Get the Lines gnerated for a Node.
        /// </summary>
        /// <param name="node">The node to get the lines</param>
        /// <param name="sourceText">The potential source text buffer in to which the line will be generated</param>
        /// <param name="isGlobalBuffer">Determine if yes or no the sourceText parameter refers a global source text buffer or a local source text buffer</param>
        /// <returns>The Node's lines</returns>
        IEnumerable<ITextLine> NodeLines(Node node, SourceText sourceText, bool isGlobalBuffer)
        {
            //Check for a node that need a IGeneratorContext data
            if (node is IGeneratorContext genCtx)
            {
                genCtx.Generator = this;
                genCtx.SourceTextBuffer = sourceText;
                genCtx.IsGlobalSourceTextBuffer = isGlobalBuffer;
            }

            node.Layout = Layout;
            return node.Lines;
        }

        /// <summary>
        /// Recursively get all lines of a node and its children.
        /// </summary>
        /// <param name="node">The node to get all line</param>
        /// <param name="all_lines">All line accumulator</param>
        /// <param name="sourceText">The potential source text buffer in to which the line will be generated</param>
        /// <param name="isGlobalBuffer">Determine if yes or no the sourceText parameter refers a global source text buffer or a local source text buffer</param>
        void RecursiveNodeLines(Node node, BitArray generated_node, List<ITextLine> all_lines, SourceText sourceText, bool isGlobalBuffer)
        {
            foreach (var l in NodeLines(node, sourceText, isGlobalBuffer))
                all_lines.Add(l);
            foreach (Node child in node.Children)
            {
                if (child.NodeIndex >= 0)
                    generated_node[child.NodeIndex] = true;
                RecursiveNodeLines(child, generated_node, all_lines, sourceText, isGlobalBuffer);
            }
        }
        /// <summary>
        /// Get all lines to be Generated by a Node. If the Node was inserted by a factory then
        /// all its children as also generated by a factory, they are generated with it.
        /// </summary>
        /// <param name="node"></param>
        /// <param name="generated_node"></param>
        /// <param name="sourceText">The potential source text buffer in to which the line will be generated</param>
        /// <param name="isGlobalBuffer">Determine if yes or no the sourceText parameter refers a global source text buffer or a local source text buffer</param>
        /// <returns></returns>
        public virtual IEnumerable<ITextLine> NodeLines(Node node, BitArray generated_node, SourceText sourceText, bool isGlobalBuffer)
        {
            if (node.IsFlagSet(Node.Flag.FullyGenerateRecursivelyFactoryGeneratedNode))
            {
                List<ITextLine> all_lines = new List<ITextLine>();
                RecursiveNodeLines(node, generated_node, all_lines, sourceText, isGlobalBuffer);
                foreach (var l in all_lines)
                    yield return l;
            }
            else
            {
                foreach (var l in NodeLines(node, sourceText, isGlobalBuffer))
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
                            foreach (var cl in NodeLines(child, sourceText, isGlobalBuffer))
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
                    results.Add(SetComment(new TextLineSnapshot(-1, cobol.SourceText, null), isComment));
                }
                else
                    throw new System.NotImplementedException("Unsuported columns layout: " + Layout);
            }
            else
            {
                if (Layout == ColumnsLayout.CobolReferenceFormat)
                {
                    var lines = CobolTextLine.Create(line.Text, Layout, CompilationResults.CompilerOptions, line.LineIndex);
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
        /// Produces a commented or an uncommented text line from a text line
        /// </summary>
        /// <param name="line">the line</param>
        /// <param name="isComment">if null then this is the identity function, if true a commented line is produced, otherwise an uncommented line is produced.</param>
        /// <returns>if isComment is null the same line is return, if true a commented line is returned otherwise an uncommented line</returns>
        private ITextLine SetComment(ITextLine line, bool? isComment)
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
        /// <param name="line">The text line to be produced a commented text line</param>
        /// <returns>The commented text line</returns>
        private ITextLine Comment(ITextLine line)
        {
            var cobol = line as CobolTextLine;
            if (cobol != null)
            {
                StringBuilder text = new StringBuilder(cobol.Text);
                if (text.Length > 6)
                    text[6] = '*';
                var lines = CobolTextLine.Create("*" + cobol.SourceText, cobol.ColumnsLayout, CompilationResults.CompilerOptions, cobol.LineIndex);
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
        private ITextLine Uncomment(ITextLine line)
        {
            var cobol = line as CobolTextLine;
            if (cobol != null)
            {
                StringBuilder text = new StringBuilder(cobol.Text);
                text[6] = ' ';
                var lines = CobolTextLine.Create(text.ToString(), cobol.ColumnsLayout, CompilationResults.CompilerOptions, cobol.LineIndex);
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
