using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Diagnostics;

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
        /// The Set of Exceed Lines over column 72
        /// </summary>
        private HashSet<int> ExceedLines;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public DefaultGenerator(TypeCobol.Compiler.CompilationDocument document, TextWriter destination, List<Skeleton> skeletons)
            : base(document, destination, skeletons)
        {
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
            Destination.Flush();
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
        private SourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, IReadOnlyList<A> Input) where A : ITextLine
        {            
            SourceText targetSourceText = new GapSourceText();
            //Stack Used to save current generation buffer when switching in a function declaration generation.
            //Beacuse a function declartion has its own buffer.
            Stack<SourceText> stackOuterBuffer = new Stack<SourceText>();
            Stack<SourceText> stackLocalBuffer = new Stack<SourceText>();
            //Bit Array of Generated Nodes.
            BitArray generated_node = new BitArray(mapper.NodeCount);
            //The previous line generation buffer 
            StringSourceText previousBuffer = null;
            for (int i = 0; i < mapper.LineData.Length; i++)
            {
                //--------------------------------------------------------------------------------------------------------------
                //1) A Non commented line with no Associated nodes is generated without any change.
                if (!mapper.CommentedLines[i] && mapper.LineData[i].LineNodes == null)
                {
                    //If there was a previous buffer ==> Flush it
                    if (previousBuffer != null)
                    {
                        if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                            AppendBufferContent(targetSourceText, previousBuffer);
                        previousBuffer = null;
                    }
                    string text = Input[i].Text;
                    if (mapper.LineData[i].Buffer != null)
                    {//This line has been assigned a target Buffer
                        mapper.LineData[i].Buffer.Insert(text, targetSourceText.Size, targetSourceText.Size);
                        mapper.LineData[i].Buffer.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
                    }
                    else
                    {
                        targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                        targetSourceText.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
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
                        AppendBufferContent(targetSourceText, previousBuffer);
                    previousBuffer = null;
                }
                for (int j = i; mapper.CommentedLines[j]; j++)
                {
                    List<int> current_nodes = mapper.LineData[j].LineNodes;
                    if (!LinearNodeSourceCodeMapper.HasIntersection(line_nodes, current_nodes))
                        break;//This commented line has no nodes which intersect with the previous line.
                    IEnumerable<ITextLine> lines = Indent(Input[j], true);
                    foreach (var line in lines)
                    {
                        string text = line.Text.TrimEnd();
                        targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                        targetSourceText.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
                        CheckFunctionDeclCommentedheader(mapper, current_nodes, text);
                    }
                    mapper.CommentedLines[j] = false;//This commented line has been generated now
                    line_nodes = current_nodes;
                }
                //--------------------------------------------------------------------------------------------------------------
                //3)For each node related to this line, and not already generated.
                line_nodes = mapper.LineData[i].LineNodes;
                foreach (int node_index in line_nodes)
                {
                    if (node_index == -1 || mapper.Nodes[node_index].node.IsFlagSet(Node.Flag.GeneratorCanIgnoreIt))
                    {//bad Node
                        continue;
                    }
                    if (generated_node[node_index])
                        continue;//Already Generated.
                    bool bFunctionBodyNode = mapper.Nodes[node_index].FunctionBodyNode != null;//Is this node in a function body ?
                    StringSourceText curSourceText = mapper.Nodes[node_index].Buffer;
                    if (curSourceText != previousBuffer && previousBuffer != null)
                    {//Flush previous buffer
                        if (!mapper.IsGeneratedEmptyBuffer(previousBuffer))
                            AppendBufferContent(targetSourceText, previousBuffer);
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
                            StringWriter sw = new StringWriter();
                            if (bFirst && !bIsFunctionDecl && curSourceText != null)
                            {//The first element don't ident it just insert it a the right position
                                sw.WriteLine(line.Text);
                                bFirst = false;
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
                                    funData.FunctionDeclBuffer.Insert(funData.CommentedHeader.ToString(), funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size);
                                }
                                //Insert the sequence
                                funData.FunctionDeclBuffer.Insert(text, funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size);
                            }
                            else
                            {
                                if (curSourceText == null)
                                    targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                                else
                                    curSourceText.Insert(text, Math.Min(from.Pos, curSourceText.Size), Math.Min(to.Pos, curSourceText.Size));
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
                                AppendBufferContent(targetSourceText, previousBuffer);
                            previousBuffer = null;
                        }
                        targetSourceText = stackOuterBuffer.Pop();
                        curSourceText = (StringSourceText)stackLocalBuffer.Pop();
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
                    AppendBufferContent(targetSourceText, previousBuffer);
                previousBuffer = null;
            }
            //--------------------------------------------------------------------------------------------------------------
            //4)//Flush of Function declation body
            foreach (int fun_index in mapper.FunctionDeclarationNodeIndices)
            {
                LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[fun_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                AppendBufferContent(targetSourceText, funData.FunctionDeclBuffer);
            }            
            //5)//Generate Line Exceed Diagnostics
            GenerateExceedLineDiagnostics();
            return targetSourceText;
        }

        /// <summary>
        /// Append a source buffer at the end of a target buffer
        /// </summary>
        /// <param name="dstBuffer"></param>
        /// <param name="srcBuffer"></param>
        private static void AppendBufferContent(SourceText dstBuffer, SourceText srcBuffer)
        {
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
        {
            int lineLen = -1;
            int lineStartOffset = -1;
            int lineEndOffset = -1;
            int start = Math.Min(from.Pos, buffer.Size);
            int end = Math.Min(to.Pos, buffer.Size);
            if (ExceedLines != null)
            {
                ExceedLines.Remove(lineNumber);
            }
            lineLen = buffer.GetLineInfo(start, out lineStartOffset, out lineEndOffset);
            buffer.Insert(code, start, end);

            int delta = -(end - start) + code.Length;
            int newLineLen = lineLen + delta;
            bool newHas73Chars = false;
            if (newLineLen > LEGAL_COBOL_LINE_LENGTH)
            {
                for (int k = LEGAL_COBOL_LINE_LENGTH; k < newLineLen & !newHas73Chars; k++)
                    newHas73Chars = !Char.IsWhiteSpace(buffer[lineStartOffset + k]);
                //Error
                //Emit an error.
                if ((newLineLen > MAX_COBOL_LINE_LENGTH) || newHas73Chars)
                {
                    if (ExceedLines == null)
                    {
                        ExceedLines = new HashSet<int>();
                    }
                    ExceedLines.Add(lineNumber);
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
                foreach (int lineNumber in ExceedLines)
                {
                    Diagnostic diag = new Diagnostic(MessageCode.GenerationErrorLineExceed, 0, 0, lineNumber);
                    AddDiagnostic(diag);
                }
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
            foreach (var l in node.Lines)
                yield return l;
            if (node.IsFlagSet(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex))
            {
                //All its Children that are not inserted at a specific index are generated with it
                foreach(Node child in node.Children)
                {
                    if (child.IsFlagSet(Node.Flag.FactoryGeneratedNode) && 
                        !child.IsFlagSet(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex))
                    {
                        generated_node[child.NodeIndex] = true;
                        foreach (var cl in child.Lines)
                            yield return cl;
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
            int length = to - from;
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
                    var lines = CobolTextLine.Create(line.Text, Layout, line.InitialLineIndex);
                    foreach (var l in lines) results.Add(SetComment(l, isComment));
                }
                else
                    if (Layout == ColumnsLayout.FreeTextFormat)
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
                text[6] = '*';
                var lines = CobolTextLine.Create("*" + cobol.SourceText, cobol.ColumnsLayout, cobol.InitialLineIndex);
                foreach (var l in lines) return l;// there's only one in the collection
                throw new System.NotImplementedException("I should have at least one item!");
            }
            else
            {
                return new TextLineSnapshot(line.InitialLineIndex, "*" + line.Text, null);
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
                var lines = CobolTextLine.Create(text.ToString(), cobol.ColumnsLayout, cobol.InitialLineIndex);
                foreach (var l in lines) 
                    return l;// there's only one in the collection
                throw new System.NotImplementedException("I should have at least one item!");
            }
            else
            {
                StringBuilder text = new StringBuilder(line.Text);
                int index = line.Text.IndexOf('*');
                text[index] = ' ';
                return new TextLineSnapshot(line.InitialLineIndex, text.ToString(), null);
            }
        }
    }
}
