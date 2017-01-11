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
            GapSourceText targetSourceText = LinearGeneration(mapper, Parser.Results.TokensLines);
            // Step 3: Write target document
            targetSourceText.Write(Destination);
            Destination.Flush();
        }

        /// <summary>
        /// Perform a linear Generation
        /// </summary>
        private GapSourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, IReadOnlyList<A> Input) where A : ITextLine
        {            
            GapSourceText targetSourceText = new GapSourceText();
            //Bit Array of Generated Nodes.
            BitArray generated_node = new BitArray(mapper.NodeCount);
            //The previous line generation buffer 
            StringSourceText previousBuffer = null;
            bool bBufferWasInFunctionBody = false;
            for (int i = 0; i < mapper.LineData.Length; i++)
            {
                //--------------------------------------------------------------------------------------------------------------
                //1) A Non commented line with no Associated nodes is generated without any change.
                if (!mapper.CommentedLines[i] && mapper.LineData[i].LineNodes == null)
                {
                    //If there was a previous buffer ==> Flush it
                    if (previousBuffer != null)
                    {
                        if (!bBufferWasInFunctionBody && !mapper.IsGeneratedEmptyBuffer(previousBuffer) )
                            targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
                        previousBuffer = null;
                    }
                    IEnumerable<ITextLine> lines = Indent(Input[i], null);
                    foreach (var line in lines)
                    {
                        string text = line.Text;
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
                    if (!bBufferWasInFunctionBody && !mapper.IsGeneratedEmptyBuffer(previousBuffer))
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
                foreach(int node_index in line_nodes)
                {
                    if (node_index == -1)
                    {//bad Node
                        continue;
                    }
                    if (generated_node[node_index])
                        continue;//Already Generated.
                    bool bFunctionBodyNode = mapper.Nodes[node_index].FunctionBodyNode != null;
                    StringSourceText curSourceText = mapper.Nodes[node_index].Buffer;
                    if (curSourceText != previousBuffer && previousBuffer != null)
                    {//Flush previous buffer
                        if (!bBufferWasInFunctionBody && !mapper.IsGeneratedEmptyBuffer(previousBuffer))
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
                            curSourceText.Delete(from.Pos, to.Pos);
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
                            if (bFirst && !bIsFunctionDecl)
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
                            {
                                int f = Math.Min(from.Pos, curSourceText.Size);
                                int t = Math.Min(to.Pos, curSourceText.Size);
                                if (f != t)
                                    curSourceText.Delete(f, t);
                                LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[node_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                                funData.FunctionDeclBuffer.Insert(text, funData.FunctionDeclBuffer.Size, funData.FunctionDeclBuffer.Size);
                            }
                            else
                            {
                                curSourceText.Insert(text, Math.Min(from.Pos, curSourceText.Size), Math.Min(to.Pos, curSourceText.Size));
                            }
                            from = to;
                            sw.Close();
                        }
                        if (!bIsGenerateAndReplace)
                        {
                            //Pad a splitted segment
                            int span = mapper.Nodes[node_index].Positions.Item3;
                            string pad = new string(' ', span);
                            curSourceText.Insert(pad, to.Pos, to.Pos);
                        }
                    }
                    //This node is now generated.
                    generated_node[node_index] = true;
                    previousBuffer = curSourceText;
                    bBufferWasInFunctionBody = bFunctionBodyNode;
                }
                //--------------------------------------------------------------------------------------------------------------
            }
            //If there was a previous buffer ==> Flush it
            if (previousBuffer != null)
            {
                if (!bBufferWasInFunctionBody && !mapper.IsGeneratedEmptyBuffer(previousBuffer))
                    targetSourceText.Insert(previousBuffer, targetSourceText.Size, targetSourceText.Size);
                previousBuffer = null;
            }
            //--------------------------------------------------------------------------------------------------------------
            //4)//Flush of Function declation body
            foreach (int fun_index in mapper.FunctionDeclarationNodeIndices)
            {
                StringSourceText prevBuffer = null;
                LinearNodeSourceCodeMapper.NodeFunctionData funData = mapper.Nodes[fun_index] as LinearNodeSourceCodeMapper.NodeFunctionData;
                targetSourceText.Insert(funData.FunctionDeclBuffer, targetSourceText.Size, targetSourceText.Size);
                foreach (int node_index in funData.FunctionDeclNodes)
                {
                    if (node_index == -1)
                        continue;
                    Node node = mapper.Nodes[node_index].node;
                    if (mapper.Nodes[node_index].Buffer != null)
                    {
                        if (prevBuffer != null && prevBuffer != mapper.Nodes[node_index].Buffer)
                        {
                            targetSourceText.Insert(prevBuffer, targetSourceText.Size, targetSourceText.Size);                            
                        }
                        prevBuffer = mapper.Nodes[node_index].Buffer;
                    }
                    else
                    {//Generate the code
                        if (prevBuffer != null)
                        {
                            targetSourceText.Insert(prevBuffer, targetSourceText.Size, targetSourceText.Size);
                            prevBuffer = null;
                        }
                        StringWriter sw = new StringWriter();
                        foreach (var line in node.Lines)
                        {
                            foreach (var l in Indent(line, null))
                            {
                                sw.WriteLine(l.Text);
                            }
                        }
                        sw.Flush();
                        string text = sw.ToString();
                        targetSourceText.Insert(text, targetSourceText.Size, targetSourceText.Size);
                    }
                }
                if (prevBuffer != null)
                {
                    targetSourceText.Insert(prevBuffer, targetSourceText.Size, targetSourceText.Size);
                    prevBuffer = null;
                }
            }
            return targetSourceText;
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
