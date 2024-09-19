using System.Diagnostics;
using Newtonsoft.Json.Linq;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal class JsonRpcTestServer : JsonRPCServer
    {
        private static readonly LSPProfiling DefaultProfilingData = new LSPProfiling(TimeSpan.Zero, 0);

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
            Handler = (_, args) => throw (Exception)args.ExceptionObject;
            _testMessageServer = testMessageServer;
            _notificationTypes = new Dictionary<string, NotificationType>();
            _requestTypes = new Dictionary<string, RequestType>();
            _expected = null;

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
            DeepEquals.AssertAreEqual(_expected, parameters);
        }

        private ResponseResultOrError HandleRequest(RequestType requestType, object parameters, LSPProfiling lspProfiling)
        {
            DeepEquals.AssertAreEqual(_expected, parameters);
            return new ResponseResultOrError() { result = new object() };
        }

        public void Test(TestMessage testMessage)
        {
            ResetExpected();
            if (testMessage.Action == MessageAction.Send)
            {
                var json = JToken.Parse(testMessage.Content);
                _testMessageServer.Expected = new Expected(json, testMessage.Type == MessageType.Response);
                SendMessage(testMessage.Method, testMessage.Type, json);
            }
            else
            {
                Debug.Assert(testMessage.Action == MessageAction.Receive);
                _expected = JToken.Parse(testMessage.Content).ToObject(GetTargetType(testMessage));

                Task<ResponseResultOrError> task = null;
                string requestId = "received-request-id";
                if (testMessage.Type == MessageType.Response)
                {
                    var requestType = _requestTypes[testMessage.Method];
                    task = SendRequest(requestType, new object(), out requestId);
                }

                string message = testMessage.WrapContent(requestId);
                HandleMessage(message, null, DefaultProfilingData);

                if (task != null)
                {
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
                object parameter = json.ToObject(notificationType.ParamsType);
                SendNotification(notificationType, parameter);
            }
            else
            {
                var requestType = _requestTypes[method];
                if (messageType == MessageType.Request)
                {
                    object parameter = json.ToObject(requestType.ParamsType);
                    SendRequest(requestType, parameter, out _).ConfigureAwait(false);
                }
                else
                {
                    Debug.Assert(messageType == MessageType.Response);
                    const string requestId = "sent-request-id";
                    object result = json.ToObject(requestType.ResultType);
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
