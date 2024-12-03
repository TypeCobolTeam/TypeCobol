using System;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.JsonRPC
{
    public interface IRPCServer
    {
        /// <summary>
        /// Register a description of all the notification methods supported by the RPC server
        /// </summary>
        void RegisterNotificationMethod(NotificationType notificationType, NotificationHandler notificationHandler);

        /// <summary>
        /// Register a description of all the request methods supported by the RPC server
        /// </summary>
        void RegisterRequestMethod(RequestType requestType, RequestHandler requestHandler);

        /// <summary>
        /// Send a notification to the client
        /// </summary>
        void SendNotification(NotificationType notificationType, object parameters);

        /// <summary>
        /// Send an async request to the client and await for the response or error
        /// </summary>
        Task<ResponseResultOrError> SendRequest(RequestType requestType, object parameters, out string requestId);

        /// <summary>
        /// Write a trace in the server log file
        /// </summary>
        void WriteServerLog(string trace);

        /// <summary>
        /// An Unhandled Exception handler
        /// </summary>
        UnhandledExceptionEventHandler Handler { get; set; }
    }
}
