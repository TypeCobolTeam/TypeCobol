#if EUROINFO_RULES

using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class ExtractRemarksDataRequest
    {
        public static readonly RequestType Type = new RequestType("typecobol/euroinformation/ExtractRemarksData", typeof(ExtractRemarksDataParams), typeof(RemarksData), null);
    }
}

#endif
