using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol.Telemetry_Notification
{
    /// <summary>
    /// The telemetry notification is sent from the server to the client to ask the client
    /// to log a telemetry event. The protocol doesn't specify the payload since no interpretation
    /// of the data happens in the protocol.
    /// 
    /// Most clients even don’t handle the event directly but forward them to the extensions
    /// owing the corresponding server issuing the event.
    /// </summary>
    class TelemetryNotification
    {
        public static readonly NotificationType Type = new NotificationType("telemetry/event", typeof(object));
    }
}
