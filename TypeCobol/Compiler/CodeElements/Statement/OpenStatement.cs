using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public enum OpenMode { INPUT, OUTPUT, IO, EXTEND };

    public class OpenStatement : CodeElement
    {
        IList<OpenElement> Objects;

        public OpenStatement(IList<OpenElement> objects)
            : base(CodeElementType.OpenStatement)
        {
            this.Objects = (objects != null ? objects : new List<OpenElement>());
        }
    }

    public class OpenElement
    {
        OpenMode Mode;
        IList<OpenFileName> FileNames;

        public OpenElement(OpenMode mode, IList<OpenFileName> filenames)
        {
            this.Mode = mode;
            this.FileNames = (filenames != null ? filenames : new List<OpenFileName>());
        }
    }

    public class OpenFileName
    {
        SymbolReference<FileName> FileName;
        bool IsNoRewind;
        bool IsReversed;

        public OpenFileName(SymbolReference<FileName> filename, bool norewind = false, bool reversed = false)
        {
            this.FileName = filename;
            this.IsNoRewind = norewind;
            this.IsReversed = reversed;
        }
    }
}
