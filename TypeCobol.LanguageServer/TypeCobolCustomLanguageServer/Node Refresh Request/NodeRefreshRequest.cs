using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    class NodeRefreshRequest
    {
        public static readonly RequestType Type = new RequestType("typecobol/refreshNodesRequest", typeof(NodeRefreshParams), typeof(bool), null);
    }
}
