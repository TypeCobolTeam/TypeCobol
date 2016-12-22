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
        /// Constructor
        /// </summary>
        /// <param name="parser"> The Parser which contains parse results </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public DefaultGenerator(Parser parser, TextWriter destination, List<Skeleton> skeletons)
            : base(parser, destination, skeletons)
        {
        }

        protected override bool Process(Compiler.Nodes.Node node)
        {
            //TODO If Node inside a copy are not useful for step 1 of Generator, we can move this to step1.
            if (node.IsInsideCopy())
            {
                return false;
            }
            var generated = node as Generated;
            foreach (var line in node.Lines)
            {
                if (generated != null)
                    // if we write generated code, we INSERT one line of code between Input lines;
                    // thus, we must decrease offset as it'll be re-increased by Write(line) and
                    // we don't want to fuck up next iteration
                    offset--;
                else
                    // before we copy an original line of code, we must still write non-source
                    // lines (eg. comments or empty lines) so they are preserved in Output
                    WriteInputLinesUpTo(line);
                Write(line, node.Comment);
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
                {//JCM Remove  this line from the Input ???? This is the way use to remove line from the source code
                    SourceDocument.SourceLine src_line = SourceLineMap[l as TypeCobol.Compiler.Scanner.ITokensLine];
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
            return line.Type == CobolTextLineType.Comment || line.Type == CobolTextLineType.Blank
               || line.Type == CobolTextLineType.Debug // #267: Debug lines are copied "AS IS", even if they are invalid in COBOL85!
               || (line.Type == CobolTextLineType.Source && line.SourceText.Trim().StartsWith("COPY"));
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
        private void Write(ITextLine line, bool? isComment)
        {
            if (line == lastline) 
                return;
            StringWriter sw = null;
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
                    base.TargetDocument.Source.Insert(sw.ToString(), comment_line.From, comment_line.To);
                }
                offset++;
                lastline = line;
                return;
            }
            sw = new System.IO.StringWriter();
            var Input = Parser.Results.TokensLines;
            SourceDocument.SourceLine src_line = offset < Input.Count
             ? base.SourceLineMap[Input[offset] as TypeCobol.Compiler.Scanner.ITokensLine]
             : null;
            foreach (var l in Indent(line, isComment))
            {
                //Insert the line after the current offset                
                sw.WriteLine(l.Text);                
            }
            sw.Flush();
            int to = src_line != null ? src_line.To : base.TargetDocument.To;
            base.TargetDocument.Source.Insert(sw.ToString(), to, to);
            sw.Close();
            offset++;
            lastline = line;
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
