using System;

namespace TypeCobol.LanguageServer.JsonRPC
{
    // Signatures for methods which implement Remote Procedure Calls
    
    /// <summary>
    /// Handle a notification received from a remote client
    /// </summary>
    public delegate void NotificationHandler(NotificationType notificationType, object parameters, LSPProfiling lspProfiling);

    /// <summary>
    /// Execute a request received from a remote client and return a ressult or error
    /// </summary>
    public delegate ResponseResultOrError RequestHandler(RequestType requestType, object parameters, LSPProfiling lspProfiling);
}
