using System.Diagnostics;
using System.Reflection;
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
        /// - the exception's target site (which is the first user-code method in the StackTrace)
        /// </summary>
        /// <param name="exception">Non-null exception instance to describe.</param>
        /// <returns>Non-null instance of TelemetryEvent.</returns>
        public static TelemetryEvent CreateFrom(Exception exception)
        {
            // Explore StackTrace to get first user-code method
            MethodBase targetSite = exception.TargetSite;
            try
            {
                var stackTrace = new StackTrace(exception);
                foreach (var stackFrame in stackTrace.GetFrames())
                {
                    var method = stackFrame.GetMethod();

                    // Get module name
                    string methodModule = method?.Module.Name;
                    if (methodModule == null) continue;

                    // Keep method if it belongs to one of our assemblies (i.e. name of declaring assembly starts with 'TypeCobol')
                    if (methodModule.StartsWith(nameof(TypeCobol)))
                    {
                        targetSite = method;
                        break;
                    }
                }
            }
            catch
            {
                // Something went wrong when reading the StackTrace...
                // Keep logging the exception with its original target site.
            }

            var exceptionTargetSite = new StringBuilder();
            if (targetSite != null)
            {
                exceptionTargetSite.Append(targetSite.DeclaringType?.FullName); // Fullname of the type declaring the method
                exceptionTargetSite.Append('.');
                exceptionTargetSite.Append(targetSite.Name);
                exceptionTargetSite.Append('(');
                exceptionTargetSite.AppendJoin(", ", targetSite.GetParameters().Select(p => p.ParameterType.Name)); // For each param, simple name of its type
                exceptionTargetSite.Append(')');
            }

            return new TelemetryEvent()
            {
                type = "exception",
                data = new Dictionary<string, string>()
                {
                    { "Type", exception.GetType().FullName },
                    { nameof(Exception.Message), exception.Message },
                    { nameof(Exception.Source), exception.Source },
                    { nameof(Exception.TargetSite), exceptionTargetSite.ToString() }
                }
            };
        }

        /// <summary>
        /// String identifier for the type of the event.
        /// </summary>
        public string type { get; set; }

        /// <summary>
        /// Name-value collection to hold descriptive data of the event.
        /// </summary>
        public Dictionary<string, string> data { get; set; }
    }
}
