using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    internal class WorkspaceApplyEditRequest
    {
        public static readonly RequestType Type = new RequestType("workspace/applyEdit", typeof(ApplyWorkspaceEditParams), typeof(ApplyWorkspaceEditResponse), null);
    }
}
