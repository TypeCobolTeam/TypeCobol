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
            var filenames = new Dictionary<OpenMode, IList<OpenFileName>>();
            var list = new List<OpenFileName>();
            if (context.openInput() != null)
            {
                foreach (var c in context.openInput())
                {
                    IList<OpenFileName> l = CreateOpenFileNames(c);
                    if (l != null) list.AddRange(l);
                }
            }
            filenames.Add(OpenMode.INPUT, list);
            list = new List<OpenFileName>();
            if (context.openOutput() != null)
            {
                foreach (var c in context.openOutput())
                {
                    IList<OpenFileName> l = CreateOpenFileNames(c);
                    if (l != null) list.AddRange(l);
                }
            }
            filenames.Add(OpenMode.OUTPUT, list);
            list = new List<OpenFileName>();
            if (context.openIO() != null)
            {
                foreach (var c in context.openIO())
                {
                    IList<OpenFileName> l = CreateOpenFileNames(c);
                    if (l != null) list.AddRange(l);
                }
            }
            filenames.Add(OpenMode.IO, list);
            list = new List<OpenFileName>();
            if (context.openExtend() != null)
            {
                foreach (var c in context.openExtend())
                {
                    IList<OpenFileName> l = CreateOpenFileNames(c);
                    if (l != null) list.AddRange(l);
                }
            }
            filenames.Add(OpenMode.EXTEND, list);
            return new OpenStatement(filenames);
        }

        private IList<OpenFileName> CreateOpenFileNames(CobolCodeElementsParser.OpenInputContext context)
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
            return filenames;
        }

        private IList<OpenFileName> CreateOpenFileNames(CobolCodeElementsParser.OpenOutputContext context)
        {
            if (context.fileNameWithNoRewind() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameWithNoRewind())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename.fileName());
                bool norewind = filename.NO() != null;
                if (f != null) filenames.Add(new OpenFileName(f, norewind));
            }
            return filenames;
        }

        private IList<OpenFileName> CreateOpenFileNames(CobolCodeElementsParser.OpenIOContext context)
        {
            if (context.fileName() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileName())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename);
                if (f != null) filenames.Add(new OpenFileName(f));
            }
            return filenames;
        }

        private IList<OpenFileName> CreateOpenFileNames(CobolCodeElementsParser.OpenExtendContext context)
        {
            if (context.fileName() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileName())
            {
                var f = SyntaxElementBuilder.CreateFileName(filename);
                if (f != null) filenames.Add(new OpenFileName(f));
            }
            return filenames;
        }

        internal CloseStatement CreateCloseStatement(CobolCodeElementsParser.CloseStatementContext context)
        {
            if (context.closeFileName() == null) return null;
            var filenames = new List<CloseFileName>();
            foreach (var filename in context.closeFileName())
            {
                CloseFileName f = CreateCloseFileName(filename);
                if (f != null) filenames.Add(f);
            }
            return new CloseStatement(filenames);
        }



        private CloseFileName CreateCloseFileName(CobolCodeElementsParser.CloseFileNameContext context)
        {
            if (context == null) return null;
            var filename = SyntaxElementBuilder.CreateFileName(context.fileName());
            return new CloseFileName(filename, context.REEL() != null, context.UNIT() != null, context.REMOVAL() != null, context.NO() != null, context.LOCK() != null);
        }
    }

}
