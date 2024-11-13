using System.Text;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Class used to describe an event that occured on this server.
    /// </summary>
    public class TelemetryEvent
    {
        /// <summary>
        /// Extract data from an exception to produce a meaningful TelemetryEvent.
        /// The event will contain :
        /// - the exception's .NET type
        /// - the exception's message
        /// - the exception's target site (which is the method that threw the exception, top of the StackTrace)
        /// </summary>
        /// <param name="exception">Non-null exception instance to describe.</param>
        /// <returns>Non-null instance of TelemetryEvent.</returns>
        public static TelemetryEvent CreateFrom(Exception exception)
        {
            string exceptionType = exception.GetType().FullName;
            string exceptionMessage = exception.Message;
            string exceptionSource = exception.Source;
            var exceptionTargetSite = new StringBuilder();
            if (exception.TargetSite != null)
            {
                var targetSite = exception.TargetSite;
                exceptionTargetSite.Append(targetSite.DeclaringType?.FullName); // Fullname of the type declaring the method
                exceptionTargetSite.Append('.');
                exceptionTargetSite.Append(targetSite.Name);
                exceptionTargetSite.Append('(');
                exceptionTargetSite.AppendJoin(", ", targetSite.GetParameters().Select(p => p.ParameterType.Name)); // For each param, simple name of its type
                exceptionTargetSite.Append(')');
            }

            return new TelemetryEvent()
            {
                Type = "exception",
                Data = new Dictionary<string, string>()
                {
                    { "Type", exceptionType },
                    { nameof(Exception.Message), exceptionMessage },
                    { nameof(Exception.Source), exceptionSource },
                    { nameof(Exception.TargetSite), exceptionTargetSite.ToString() }
                }
            };
        }

        /// <summary>
        /// String identifier for the type of the event.
        /// </summary>
        public string Type { get; set; }

        /// <summary>
        /// Name-value collection to hold descriptive data of the event.
        /// </summary>
        public Dictionary<string, string> Data { get; set; }
    }
}
