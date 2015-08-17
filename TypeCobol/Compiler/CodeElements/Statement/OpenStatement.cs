using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public enum OpenMode { INPUT, OUTPUT, IO, EXTEND };

    public class OpenStatement : CodeElement
    {
        Dictionary<OpenMode, IList<OpenFileName>> FileNames;

        public OpenStatement(Dictionary<OpenMode, IList<OpenFileName>> filenames)
            : base(CodeElementType.OpenStatement)
        {
            this.FileNames = (filenames != null ? filenames : new Dictionary<OpenMode, IList<OpenFileName>>());
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
