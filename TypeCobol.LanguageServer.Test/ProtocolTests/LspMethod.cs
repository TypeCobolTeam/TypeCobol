using System.Diagnostics;
using System.Reflection;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal class LspMethod
    {
        public static Dictionary<string, LspMethod> DiscoverLspMethods()
        {
            var result = new Dictionary<string, LspMethod>();
            var assembly = typeof(TypeCobolLanguageServer).Assembly;
            foreach (var type in assembly.GetTypes())
            {
                foreach (var field in type.GetFields(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    if (field.FieldType == typeof(NotificationType))
                    {
                        AddNotificationType(field);
                    }
                    else if (field.FieldType == typeof(RequestType))
                    {
                        AddRequestType(field);
                    }
                }
            }

            return result;

            void AddNotificationType(FieldInfo field)
            {
                var notificationType = (NotificationType)field.GetValue(null);
                Debug.Assert(notificationType != null);
                if (notificationType.ParamsType == null)
                {
                    // Nothing to test
                    Console.WriteLine($"'{notificationType.Method}' notification skipped, no data to test.");
                    return;
                }

                result.Add(notificationType.Method, new LspMethod(notificationType));
            }

            void AddRequestType(FieldInfo field)
            {
                var requestType = (RequestType)field.GetValue(null);
                Debug.Assert(requestType != null);
                if (requestType.ParamsType == null && requestType.ResultType == null)
                {
                    // Nothing to test
                    Console.WriteLine($"'{requestType.Method}' request skipped, no data to test.");
                    return;
                }

                result.Add(requestType.Method, new LspMethod(requestType));
            }
        }

        public NotificationType NotificationType { get; }
        public RequestType RequestType { get; }

        public LspMethod(NotificationType notificationType)
        {
            NotificationType = notificationType;
            RequestType = null;
        }

        public LspMethod(RequestType requestType)
        {
            NotificationType = null;
            RequestType = requestType;
        }

        public bool IsNotification => NotificationType != null;
    }
}
