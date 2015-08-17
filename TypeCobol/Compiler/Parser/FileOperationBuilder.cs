using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    internal class FileOperationBuilder
    {
        internal OpenStatement CreateOpenStatement(CobolCodeElementsParser.OpenStatementContext context)
        {
            var elements = new List<OpenElement>();
            if (context.openInput() != null)
            {
                foreach (var c in context.openInput())
                {
                    OpenElement e = CreateOpenElement(c);
                    if (e != null) elements.Add(e);
                }
            }
            if (context.openOutput() != null)
            {
                foreach (var c in context.openOutput())
                {
                    OpenElement e = CreateOpenElement(c);
                    if (e != null) elements.Add(e);
                }
            }
            if (context.openIO() != null)
            {
                foreach (var c in context.openIO())
                {
                    OpenElement e = CreateOpenElement(c);
                    if (e != null) elements.Add(e);
                }
            }
            if (context.openExtend() != null)
            {
                foreach (var c in context.openExtend())
                {
                    OpenElement e = CreateOpenElement(c);
                    if (e != null) elements.Add(e);
                }
            }
            return new OpenStatement(elements);
        }

        private OpenElement CreateOpenElement(CobolCodeElementsParser.OpenInputContext context)
        {
            if (context.fileNameWithNoRewindOrReversed() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameWithNoRewindOrReversed())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename.fileName());
                bool norewind = filename.NO() != null;
                bool reversed = filename.REVERSED() != null;
                if (f != null) filenames.Add(new OpenFileName(f, norewind, reversed));
            }
            return new OpenElement(OpenMode.INPUT, filenames);
        }

        private OpenElement CreateOpenElement(CobolCodeElementsParser.OpenOutputContext context)
        {
            if (context.fileNameWithNoRewind() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameWithNoRewind())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename.fileName());
                bool norewind = filename.NO() != null;
                if (f != null) filenames.Add(new OpenFileName(f, norewind));
            }
            return new OpenElement(OpenMode.OUTPUT, filenames);
        }

        private OpenElement CreateOpenElement(CobolCodeElementsParser.OpenIOContext context)
        {
            if (context.fileName() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileName())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename);
                if (f != null) filenames.Add(new OpenFileName(f));
            }
            return new OpenElement(OpenMode.IO, filenames);
        }

        private OpenElement CreateOpenElement(CobolCodeElementsParser.OpenExtendContext context)
        {
            if (context.fileName() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileName())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename);
                if (f != null) filenames.Add(new OpenFileName(f));
            }
            return new OpenElement(OpenMode.EXTEND, filenames);
        }
    }

}
