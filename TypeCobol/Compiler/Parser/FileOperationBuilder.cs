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
        internal OpenStatement CreateOpenStatement(CodeElementsParser.OpenStatementContext context)
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

        private IList<OpenFileName> CreateOpenFileNames(CodeElementsParser.OpenInputContext context)
        {
            if (context.fileNameWithNoRewindOrReversed() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameWithNoRewindOrReversed())
            {
                var f = CobolWordsBuilder.CreateFileName(filename.fileNameReference());
                bool norewind = filename.NO() != null;
                bool reversed = filename.REVERSED() != null;
                if (f != null) filenames.Add(new OpenFileName(f, norewind, reversed));
            }
            return filenames;
        }

        private IList<OpenFileName> CreateOpenFileNames(CodeElementsParser.OpenOutputContext context)
        {
            if (context.fileNameWithNoRewind() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameWithNoRewind())
            {
                var f = CobolWordsBuilder.CreateFileName(filename.fileNameReference());
                bool norewind = filename.NO() != null;
                if (f != null) filenames.Add(new OpenFileName(f, norewind));
            }
            return filenames;
        }

        private IList<OpenFileName> CreateOpenFileNames(CodeElementsParser.OpenIOContext context)
        {
            if (context.fileNameReference() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameReference())
            {
                var f = CobolWordsBuilder.CreateFileName(filename);
                if (f != null) filenames.Add(new OpenFileName(f));
            }
            return filenames;
        }

        private IList<OpenFileName> CreateOpenFileNames(CodeElementsParser.OpenExtendContext context)
        {
            if (context.fileNameReference() == null) return null;
            var filenames = new List<OpenFileName>();
            foreach (var filename in context.fileNameReference())
            {
                var f = CobolWordsBuilder.CreateFileName(filename);
                if (f != null) filenames.Add(new OpenFileName(f));
            }
            return filenames;
        }



        internal CloseStatement CreateCloseStatement(CodeElementsParser.CloseStatementContext context)
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

        private CloseFileName CreateCloseFileName(CodeElementsParser.CloseFileNameContext context)
        {
            if (context == null) return null;
            var filename = CobolWordsBuilder.CreateFileName(context.fileNameReference());
            return new CloseFileName(filename, context.REEL() != null || context.UNIT() != null, context.REMOVAL() != null, context.NO() != null, context.LOCK() != null);
        }



        internal ReadStatement CreateReadStatement(CodeElementsParser.ReadStatementContext context)
        {
            if (context == null) return null;
            return new ReadStatement(
                CobolWordsBuilder.CreateFileName(context.fileNameReference()),
                CobolWordsBuilder.CreateIdentifier(context.identifier()),
                CobolWordsBuilder.CreateQualifiedName(context.qualifiedDataName()),
                context.NEXT() != null,
                context.RECORD() != null
                );
        }

        internal WriteStatement CreateWriteStatement(CodeElementsParser.WriteStatementContext context)
        {
            if (context == null) return null;
            return new WriteStatement(
                CobolWordsBuilder.CreateQualifiedName(context.qualifiedDataName()),
                CobolWordsBuilder.CreateIdentifier(context.identifier()),
                context.BEFORE() != null,
                context.AFTER() != null,
                new ArithmeticExpressionBuilder().CreateNumberOrIdentifier(context.identifierOrInteger()),
                CobolWordsBuilder.CreateMnemonic(context.mnemonicForEnvironmentNameReference()),
                context.PAGE() != null
                );
        }

        internal RewriteStatement CreateRewriteStatement(CodeElementsParser.RewriteStatementContext context)
        {
            if (context == null) return null;
            return new RewriteStatement(
                CobolWordsBuilder.CreateQualifiedName(context.qualifiedDataName()),
                CobolWordsBuilder.CreateIdentifier(context.identifier())
                );
        }
    }

}
