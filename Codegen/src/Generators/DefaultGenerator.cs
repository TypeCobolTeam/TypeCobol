using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Default Generator
    /// </summary>
    public class DefaultGenerator : Generator2
    {
        /// <summary>Index in Input of the next line to write</summary>
        private int offset = 0;
        /// <summary>Last line written</summary>
        private TypeCobol.Compiler.Text.ITextLine lastline = null;

        /// <summary>
        /// The Directive to CodeElement Mapper
        /// </summary>
        public DirectiveCodeElementMapper DirectiveCodeElementMapper
        {
            get;
            private set;
        }

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
            DirectiveCodeElementMapper = new DirectiveCodeElementMapper(Parser.Results.TokensLines, this);            
            RootNode.Accept(DirectiveCodeElementMapper);
            //TargetDocument.Dump();
            base.TreeToCode();
            //Flush all Node SourceTextBuffer
            FlushNodeSourceTextBuffers();
        }

        /// <summary>
        /// Flushes all Node's Source Text Buffers.
        /// </summary>
        protected virtual void FlushNodeSourceTextBuffers()
        {
            //For each Duplicate line that are associated to a Sooyrce Text Buffer
            foreach (var line in DirectiveCodeElementMapper.DuplicatedLineSourceTextMap.Keys)
            {                
                StringSourceText sourceText = DirectiveCodeElementMapper.DuplicatedLineSourceTextMap[line];
                //Pad all splitted segment based on associated node consumed tokens positions
                List<Compiler.Nodes.Node> nodes = DirectiveCodeElementMapper.SourceTexNodestMap[sourceText];
                foreach (var node in nodes)
                {
                    Tuple<int, int> from_to = node.FromToPositions;
                    if (from_to != null)
                    {                        
                        //Position in the source text
                        Tuple<Position, Position> positions = DirectiveCodeElementMapper.NodeFromToPositionMap[node];
                        if (positions != null)
                        {
                            int span = from_to.Item2;
                            string pad = new string(' ', span);
                            sourceText.Insert(pad, positions.Item2.Pos, positions.Item2.Pos);
                        }
                    }
                }

                if (nodes.Count != 0)
                {// Only if associated nodes exists
                    //Insert the Source Text
                    var Input = Parser.Results.TokensLines;
                    SourceDocument.SourceLine src_line = line < Input.Count
                     ? base.SourceLineMap[Input[line] as TypeCobol.Compiler.Scanner.ITokensLine]
                     : null;
                    int to = src_line != null ? src_line.To : this.TargetDocument.To;
                    TargetDocument.Source.Insert(sourceText, to, to);
                }
            }
        }

        protected override bool Process(Compiler.Nodes.Node node)
        {
            //TODO If Node inside a copy are not useful for step 1 of Generator, we can move this to step1.
            if (node.IsInsideCopy())
            {
                return false;
            }
            Tuple<Position, Position> from_to_generated = null;
            if (this.DirectiveCodeElementMapper.NodeFromToPositionMap.ContainsKey(node))
            {   //A node with no possition shall be ignored.
                from_to_generated = DirectiveCodeElementMapper.NodeFromToPositionMap[node];
                if (from_to_generated == null)
                {
                    return false;
                }
            }
            var generated = node as Generated;
            //Are we generating a Node which have a separate buffer ?
            StringSourceText sourceText = null;
            if (this.DirectiveCodeElementMapper.NodeSourceTextMap.ContainsKey(node))
            {
                sourceText = DirectiveCodeElementMapper.NodeSourceTextMap[node];
            }
            if (generated == null || sourceText == null)
            {//Non generated node or no custom source text ==> ignore custom buffer position
                from_to_generated = null;
                sourceText = null;
            }
            
            bool bFirst = true;
            foreach(var line in node.Lines)
            {
                if (generated != null)
                {
                    // if we write generated code, we INSERT one line of code between Input lines;
                    // thus, we must decrease offset as it'll be re-increased by Write(line) and
                    // we don't want to fuck up next iteration
                    offset--;
                }
                else
                {
                    // before we copy an original line of code, we must still write non-source
                    // lines (eg. comments or empty lines) so they are preserved in Output
                    WriteInputLinesUpTo(line);
                }
                from_to_generated = Write(line, node.Comment, sourceText != null ? sourceText : TargetDocument.Source, from_to_generated, bFirst);
                bFirst = false;
                //this.TargetDocument.Dump();
            }
            return generated == null || !generated.IsLeaf;
        }

        /// <summary>
        /// Write all lines between the last written line (ie. Input[offset-1]) and a given line.
        /// If line is contained in Input but before offset, all remaining Input will be written.
        ///	In other words: don't fall in this case.
        /// </summary>
        /// <param name="line"></param>
        /// <returns>Number of lines written during this method call.</returns>
        private int WriteInputLinesUpTo(ITextLine line)
        {
            if (!IsInInput(line)) 
                return 0;
            int lines = 0;
            var Input = Parser.Results.TokensLines;
            while (offset < Input.Count)
            {
                var l = Input[offset];
                if (l == line) 
                    break;
                if (!ShouldCopy(l))
                {//JCM Remove  this line from the Input ???? This is the way which is used to remove line from the source code
                    SourceDocument.SourceLine src_line = SourceLineMap[l as TypeCobol.Compiler.Scanner.ITokensLine];
                    String text = base.TargetDocument.Source.GetTextAt(src_line.From, src_line.To);
                    base.TargetDocument.Source.Delete(src_line.From, src_line.To);
                }
                offset++;
                lines++;
            }
            return lines;
        }
        /// <summary>
        /// Only copy from input to output lines that are comment or blank.
        /// Everything else is either:
        ///  - COBOL source, written by original AST nodes
        ///  - TypeCobol source, written by AST generated nodes
        ///  - invalid lines (wtf?)
        /// Source lines are of type Source, Debug or Continuation in CobolTextLineType enum.
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private bool ShouldCopy(ICobolTextLine line)
        {
            //Check for a Compiler Directive and consider that it should be copied.
            if (line.Type == CobolTextLineType.Source && line is TypeCobol.Compiler.Preprocessor.ProcessedTokensLine)
            {//Fix for tests CASE9, CASE10
                TypeCobol.Compiler.Preprocessor.ProcessedTokensLine ptl = (TypeCobol.Compiler.Preprocessor.ProcessedTokensLine)line;
                if (ptl.HasCompilerDirectives)
                    return true;
            }

            return line.Type == CobolTextLineType.Comment || line.Type == CobolTextLineType.Blank
               || line.Type == CobolTextLineType.Debug; // #267: Debug lines are copied "AS IS", even if they are invalid in COBOL85!
        }

        private bool IsInInput(ITextLine line)
        {
            if (line is TypeCobol.Compiler.Scanner.ITokensLine)
            {
                int c = offset;
                if (c < base.SourceLineMap.Count)
                {
                    return base.SourceLineMap.ContainsKey(line as TypeCobol.Compiler.Scanner.ITokensLine);
                }
            }
            return false;
        }

        /// <summary>
        /// Writes one line of Input as one or more lines in Output.
        ///	A single line, once indented, can output as many lines, especially on 80 colons.
        ///	The value of offset is increased once as part of the Write operation.
        /// </summary>
        /// <param name="line">Input[offset]</param>
        /// <param name="isComment">Must line be commented ?</param>
        /// <param name="from_to_generated"> is this line from generated code, this the interval where to insert the line</param>
        private Tuple<Position, Position> Write(ITextLine line, bool? isComment, SourceText sourceText, Tuple<Position, Position> from_to_generated, bool bFirst)
        {
            if (line == lastline)
                return from_to_generated;
            StringWriter sw = null;
            string text = null;
            if (IsInInput(line))
            {
                if (isComment == true)
                {//If a Line is already in the source code only regenerate those that must be commented.
                    SourceDocument.SourceLine comment_line = base.SourceLineMap[line as TypeCobol.Compiler.Scanner.ITokensLine];
                    sw = new System.IO.StringWriter();
                    foreach (var l in Indent(line, isComment))
                    {
                        sw.WriteLine(l.Text);
                    }
                    //Replace the current line
                    sw.Flush();
                    text = sw.ToString();
                    sourceText.Insert(text, comment_line.From, comment_line.To);
                    if (from_to_generated != null)
                        from_to_generated = new Tuple<Position, Position>(from_to_generated.Item2, from_to_generated.Item2);
                }
                offset++;
                lastline = line;
                return from_to_generated;
            }
            sw = new System.IO.StringWriter();
            var Input = Parser.Results.TokensLines;
            SourceDocument.SourceLine src_line = offset < Input.Count
             ? base.SourceLineMap[Input[offset] as TypeCobol.Compiler.Scanner.ITokensLine]
             : null;
            if (from_to_generated != null && bFirst && isComment != true)
            {//The first element don't ident it just insert it a the right position
                sw.WriteLine(line.Text);
            }
            else foreach (var l in Indent(line, isComment))
            {
                sw.WriteLine(l.Text);
            }
            sw.Flush();
            text = sw.ToString();
            if (from_to_generated != null)
            {
                sourceText.Insert(text, from_to_generated.Item1.Pos, from_to_generated.Item2.Pos);
                from_to_generated = new Tuple<Position, Position>(from_to_generated.Item2, from_to_generated.Item2);
            }
            else
            {
                int to = src_line != null ? src_line.To : sourceText.Size;                
                sourceText.Insert(text, to, to);
            }
            sw.Close();
            offset++;
            lastline = line;
            return from_to_generated;
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
