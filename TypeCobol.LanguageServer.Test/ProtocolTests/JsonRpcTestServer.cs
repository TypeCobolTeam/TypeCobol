using System.Diagnostics;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    /// <summary>
    /// This class is responsible for testing messages. The actual class tested is JsonRPCServer.
    /// By inheriting from it, we can control the message before and after they are sent or conversely
    /// before and after they are received.
    /// </summary>
    internal class JsonRpcTestServer : JsonRPCServer
    {
        private static readonly LSPProfiling _DefaultProfilingData = new LSPProfiling(TimeSpan.Zero, 0);

        private static readonly JsonSerializer _JsonSerializer = new JsonSerializer()
        {
            // Use strict deserialization, to ensure each field from the test data is mapped to a field in our classes
            // This flag does not apply to the deserializer used by the real server
            MissingMemberHandling = MissingMemberHandling.Error
        };

        // This is a little trick to instantiate the TestMessageServer while calling the base constructor
        private static IMessageServer CreateMessageServer(out TestMessageServer testMessageServer)
        {
            testMessageServer = new TestMessageServer();
            return testMessageServer;
        }

        private readonly TestMessageServer _testMessageServer;
        private readonly Dictionary<string, NotificationType> _notificationTypes;
        private readonly Dictionary<string, RequestType> _requestTypes;
        private object _expected;

        public JsonRpcTestServer(IEnumerable<LspMethodDefinition> supportedMethods)
            : base(CreateMessageServer(out var testMessageServer))
        {
            // Unhandled exceptions handler: for testing we want to see every error so just rethrow
            Handler = (_, args) => throw (Exception)args.ExceptionObject;

            _testMessageServer = testMessageServer;
            _notificationTypes = new Dictionary<string, NotificationType>();
            _requestTypes = new Dictionary<string, RequestType>();
            _expected = null;

            // Dispatch LSP methods into notifications and requests dictionaries + register our own generic handlers
            foreach (var supportedMethod in supportedMethods)
            {
                if (supportedMethod.Kind == MethodKind.NotificationMethod)
                {
                    var notificationType = (NotificationType)supportedMethod;
                    _notificationTypes.Add(notificationType.Method, notificationType);
                    RegisterNotificationMethod(notificationType, HandleNotification);
                }
                else
                {
                    var requestType = (RequestType)supportedMethod;
                    _requestTypes.Add(requestType.Method, requestType);
                    RegisterRequestMethod(requestType, HandleRequest);
                }
            }
        }

        private void HandleNotification(NotificationType notificationType, object parameters, LSPProfiling lspProfiling)
        {
            /*
             * The server received a notification, we must compare the received params with the ones we are
             * expecting from the test data.
             */
            DeepEquals.AssertAreEqual(_expected, parameters);
        }

        private ResponseResultOrError HandleRequest(RequestType requestType, object parameters, LSPProfiling lspProfiling)
        {
            /*
             * The server received a request, we must compare the received params of the request with the params
             * we are expecting from the test data.
             */
            DeepEquals.AssertAreEqual(_expected, parameters);

            // Replying a dummy response, this response will be ignored by the TestMessageServer
            return new ResponseResultOrError() { result = new object() };
        }

        /// <summary>
        /// Test one message.
        /// </summary>
        /// <param name="testMessage">The non-null TestMessage to check.</param>
        public void Test(TestMessage testMessage)
        {
            ResetExpected();
            if (testMessage.Action == MessageAction.Send)
            {
                /*
                 * Testing a send operation: the goal of a send is to serialize a given protocol object into a JSON string
                 * and send the string to the client. So here we capture the expected JSON and give it to the TestMessageServer
                 * then perform the send. The TestMessageServer will be responsible for comparing the expected JSON with the
                 * actual JSON produced by the JsonRPCServer.
                 */
                var json = JToken.Parse(testMessage.Content);
                _testMessageServer.Expected = new Expected(json, testMessage.Type == MessageType.Response);
                SendMessage(testMessage.Method, testMessage.Type, json);
            }
            else
            {
                /*
                 * Testing a receive operation: the goal of a receive is to read a given JSON string and deserialize it
                 * into a protocol object. So here we deserialize the test data to get the expected object then we let
                 * the server handle the string message. The server will call us back through the registered handlers.
                 * The notification and request handler are responsible for comparing the expected object with the actual
                 * object deserialized by the server.
                 * For response messages, the flow is a bit different. We have to send a fake request so that the server
                 * will associate the response message to an id it knows. When handling the response, the server will
                 * set the actual response object as the result of the fake request, allowing us to get it back.
                 */
                Debug.Assert(testMessage.Action == MessageAction.Receive);
                _expected = JToken.Parse(testMessage.Content).ToObject(GetTargetType(testMessage), _JsonSerializer);

                Task<ResponseResultOrError> task = null;
                string requestId = "received-request-id";
                if (testMessage.Type == MessageType.Response)
                {
                    // Fake request
                    var requestType = _requestTypes[testMessage.Method];
                    task = SendRequest(requestType, new object(), out requestId);
                }

                // Let the server handle the message
                string message = testMessage.WrapContent(requestId);
                HandleMessage(message, null, _DefaultProfilingData);

                if (task != null)
                {
                    // The message was a response, read the result from the task
                    DeepEquals.AssertAreEqual(_expected, task.Result.result);
                }
            }
        }

        private void ResetExpected()
        {
            _expected = null;
            _testMessageServer.Expected = null;
        }

        private void SendMessage(string method, MessageType messageType, JToken json)
        {
            if (messageType == MessageType.Notification)
            {
                var notificationType = _notificationTypes[method];
                object parameter = json.ToObject(notificationType.ParamsType, _JsonSerializer);
                SendNotification(notificationType, parameter);
            }
            else
            {
                var requestType = _requestTypes[method];
                if (messageType == MessageType.Request)
                {
                    object parameter = json.ToObject(requestType.ParamsType, _JsonSerializer);
                    SendRequest(requestType, parameter, out _).ConfigureAwait(false);
                }
                else
                {
                    Debug.Assert(messageType == MessageType.Response);
                    const string requestId = "sent-request-id";
                    object result = json.ToObject(requestType.ResultType, _JsonSerializer);
                    Reply(requestId, new ResponseResultOrError() { result = result });
                }
            }
        }

        private Type GetTargetType(TestMessage testMessage)
        {
            if (testMessage.Type == MessageType.Notification)
            {
                var notificationType = _notificationTypes[testMessage.Method];
                return notificationType.ParamsType;
            }

            var requestType = _requestTypes[testMessage.Method];
            if (testMessage.Type == MessageType.Request)
            {
                return requestType.ParamsType;
            }

            Debug.Assert(testMessage.Type == MessageType.Response);
            return requestType.ResultType;
        }
    }
}
