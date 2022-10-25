#if EUROINFO_RULES

using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    public class RemarksData
    {
        public string[] usedCPYs;
        public int insertionLine;
    }
}

#endif
