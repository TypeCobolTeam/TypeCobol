using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// The GetDataLayout request is sent by the client to retrieve the elements declared in a program or a copy
    /// and some additional information (especially their memory size).
    /// </summary>
    class GetDataLayoutRequest
    {
        public static readonly RequestType Type = new RequestType("typecobol/getDataLayoutRequest", typeof(GetDataLayoutParams), typeof(GetDataLayoutResult), null);
    }
}
