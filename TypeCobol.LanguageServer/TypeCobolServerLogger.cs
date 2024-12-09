using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.VsCodeProtocol;
using TypeCobol.LanguageServer.VsCodeProtocol.Telemetry_Notification;
using TypeCobol.Logging;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Custom logger used to dispatch server exceptions as telemetry events to the client
    /// </summary>
    internal class TypeCobolServerLogger : ILogger
    {
        private readonly IRPCServer _rpcServer;

        public TypeCobolServerLogger(IRPCServer rpcServer)
        {
            _rpcServer = rpcServer;
        }

        public void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData)
        {
            // Do nothing
        }

        public void LogException(Exception exception, IDictionary<string, object> contextData)
        {
            // Send exception as telemetry/event, ignore context data as the event is supposed to be lightweight.
            // Context data will still be sent through the other loggers
            var telemetryEvent = TelemetryEvent.CreateFrom(exception);
            _rpcServer.SendNotification(TelemetryNotification.Type, telemetryEvent);
        }

        public void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData)
        {
            // Do nothing
        }
    }
}
