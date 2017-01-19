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

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Default Generator
    /// </summary>
    public class DefaultGenerator : Generator
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parser"> The Parser which contains parse results </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public DefaultGenerator(Parser parser, TextWriter destination, List<Skeleton> skeletons)
            : base(parser, destination, skeletons)
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
            SourceText targetSourceText = LinearGeneration(mapper, Parser.Results.TokensLines);
            // Step 3: Write target document
            targetSourceText.Write(Destination);
            Destination.Flush();
        }

        /// <summary>
        /// Perform a linear Generation
        /// //1) A Non commented line with no Associated nodes is generated without any change.
        /// //2) If the line is commented then first comment all following lines that have the same intersection with the corresponding target Nodes.
        /// //3) For each node related to a line, and not already generated the corresponding code.
        /// //4) Flush of Function declations.
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
                            targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
                        previousBuffer = null;
                    }
                    IEnumerable<ITextLine> lines = Indent(Input[i], null);
                    foreach (var line in lines)
                    {
                        string text = line.Text;
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
                        targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
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
                        string text = line.Text;
                        targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                        targetSourceText.Insert(Environment.NewLine, targetSourceText.Size, targetSourceText.Size);
                    }
                    mapper.CommentedLines[j] = false;//This commented line has been generated now
                    line_nodes = current_nodes;
                }
                //--------------------------------------------------------------------------------------------------------------
                //3)For each node related to this line, and not already generated.
                line_nodes = mapper.LineData[i].LineNodes;
                foreach (int node_index in line_nodes)
                {
                    if (node_index == -1)
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
                            targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
                        previousBuffer = null;
                    }
                    Node node = mapper.Nodes[node_index].node;
                    bool bGenerated = node is Generated;
                    if (!bGenerated)
                    {   //This Node is not Generated: If it removed then remove its source code otherwise do Nothing it is already in the source buffer.
                        if (mapper.Nodes[node_index].Removed)
                        {//If this node is removed
                            //var sourceLine = TargetDocument[i];
                            Position from = mapper.Nodes[node_index].From;
                            Position to = mapper.Nodes[node_index].To;
                            //Delete <==> Replace with blanks
                            ReplaceByBlanks(curSourceText, from.Pos, to.Pos);
                        }
                    }
                    else
                    {
                        bool bIsFunctionDecl = mapper.Nodes[node_index] is LinearNodeSourceCodeMapper.NodeFunctionData;
                        bool bFirst = true;
                        Position from = mapper.Nodes[node_index].From;
                        Position to = mapper.Nodes[node_index].To;
                        bool bIsGenerateAndReplace = node is GeneratedAndReplace;
                        if (bIsGenerateAndReplace)
                        {//The node has a source code that must be replace
                            string code = (node as GeneratedAndReplace).ReplaceCode;
                            curSourceText.Insert(code, Math.Min(from.Pos, curSourceText.Size), Math.Min(to.Pos, curSourceText.Size));
                        }
                        else foreach (var line in node.Lines)
                            {
                                StringWriter sw = new StringWriter();
                                if (bFirst && !bIsFunctionDecl && curSourceText != null)
                                {//The first element don't ident it just insert it a the right position
                                    sw.WriteLine(line.Text);
                                    bFirst = false;
                                }
                                else foreach (var l in Indent(line, null))
                                {
                                    sw.WriteLine(l.Text);
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
                                        //--------------------------------------------------
                                        //Create the commented header of the function.
                                        //--------------------------------------------------
                                        List<ITextLine> funHeader = CreateCommentedSequence(curSourceText, f, t);
                                        //Create a the erase string to erase in the original source code
                                        //The Function header.
                                        //Erase in the original source code the Function header?
                                        ReplaceByBlanks(curSourceText, f, t);
                                        string crlf = "";
                                        foreach (var h in funHeader)
                                        {//Output commented function header in t he Function Declaration buffer                                       
                                            foreach (var hl in Indent(h, null))
                                            {
                                                funData.FunctionDeclBuffer.Insert(crlf, funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size);
                                                funData.FunctionDeclBuffer.Insert(hl.Text, funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size);
                                                crlf = Environment.NewLine;
                                            }
                                        }
                                        //Insert NewLine characters.
                                        funData.FunctionDeclBuffer.Insert(crlf, funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size);
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
                                targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
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
                    targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
                previousBuffer = null;
            }
            //--------------------------------------------------------------------------------------------------------------
            //4)//Flush of Function declation body
            foreach (int fun_index in mapper.FunctionDeclarationNodeIndices)
            {
                LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[fun_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                targetSourceText.Insert(funData.FunctionDeclBuffer, targetSourceText.Size, targetSourceText.Size);
            }
            return targetSourceText;
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
        /// Create a commented Sequence of a Portion of the text.
        /// </summary>
        /// <param name="sourceText"></param>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <returns></returns>
        protected List<ITextLine> CreateCommentedSequence(StringSourceText sourceText, int from, int to)
        {
            List<ITextLine>  sequence = new List<ITextLine>(); // TCRFUN_CODEGEN_AS_NESTED_PROGRAM
            sequence.Add(new TextLineSnapshot(-1, "*", null));
            int startPos = from;
            int i = from;
            for (i = from; i < to; i++)
            {
                char c = sourceText[i];
                if (c == '\n')
                {
                    int endPos = i;
                    while (sourceText[endPos] == '\n' || sourceText[endPos] == '\r')
                        endPos--;
                    string text = sourceText.GetTextAt(startPos, endPos + 1);
                    ITextLine line = new TextLineSnapshot(-1, "*" + text, null);
                    sequence.Add(line);
                    startPos = i + 1;
                }
            }
            if (from != to)
            {
                string text = sourceText.GetTextAt(startPos, i);
                ITextLine line = new TextLineSnapshot(-1, "*" + text, null);
                sequence.Add(line);
            }

            return sequence;
        }

        protected override bool Process(Compiler.Nodes.Node node)
        {
            return false;
        }

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
