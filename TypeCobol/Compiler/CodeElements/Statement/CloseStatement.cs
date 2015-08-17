using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    public class CloseStatement : CodeElement
    {
        IList<CloseFileName> FileNames;

        public CloseStatement(IList<CloseFileName> filenames)
            : base(CodeElementType.CloseStatement)
        {
            this.FileNames = (filenames != null ? filenames : new List<CloseFileName>());
        }
    }

    public class CloseFileName
    {
        SymbolReference<FileName> FileName;
        bool IsReel;
        bool IsUnit;
        bool IsForRemoval;
        bool IsNoRewind;
        bool IsLock;

        public CloseFileName(SymbolReference<FileName> filename, bool reel, bool unit, bool forremoval, bool norewind, bool lock_)
        {
            this.FileName = filename;
            this.IsReel = reel;
            this.IsUnit = unit;
            this.IsForRemoval = forremoval;
            this.IsNoRewind = norewind;
            this.IsLock = lock_;
        }
    }
}
