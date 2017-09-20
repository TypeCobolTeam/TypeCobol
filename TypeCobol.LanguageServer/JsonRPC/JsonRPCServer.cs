using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using TypeCobol.LanguageServer.StdioHttp;
using Analytics;

namespace TypeCobol.LanguageServer.JsonRPC
{
    /// <summary>
    /// Implementation of a JsonRPC 2.0 message handler
    /// </summary>
    public class JsonRPCServer : IMessageHandler, IRPCServer
    {
        public JsonRPCServer(IMessageServer messageServer)
        {
            this.messageServer = messageServer;

            JsonConvert.DefaultSettings = () => new JsonSerializerSettings
            {
                Formatting = Formatting.None,
                NullValueHandling = NullValueHandling.Ignore
            };
        }

        // Message server used to send Remote Procedure Calls to the client
        private IMessageServer messageServer;
        
        // Notification methods supported by this RPC server
        private class NotificationMethod { public NotificationType Type; public NotificationHandler HandleNotification; }
        private IDictionary<string, NotificationMethod> notificationMethods = new Dictionary<string, NotificationMethod>();
        // Request methods supported by this RPC server
        private class RequestMethod { public RequestType Type; public RequestHandler HandleRequest; }
        private IDictionary<string, RequestMethod> requestMethods = new Dictionary<string, RequestMethod>();

        /// <summary>
        /// Register a description of all the notification methods supported by the RPC server
        /// </summary>
        public void RegisterNotificationMethod(NotificationType notificationType, NotificationHandler notificationHandler)
        {
            notificationMethods.Add(notificationType.Method, new NotificationMethod() { Type = notificationType, HandleNotification = notificationHandler });
        }

        /// <summary>
        /// Register a description of all the request methods supported by the RPC server
        /// </summary>
        public void RegisterRequestMethod(RequestType requestType, RequestHandler requestHandler)
        {
            requestMethods.Add(requestType.Method, new RequestMethod() { Type = requestType, HandleRequest = requestHandler });
        }

        /// <summary>
        /// Send a notification to the client
        /// </summary>
        public void SendNotification(NotificationType notificationType, object parameters)
        {
            JObject jsonMessage = new JObject();
            PrepareJsonPRCMessage(jsonMessage);

            jsonMessage["method"] = notificationType.Method;
            if (parameters != null)
            {
                jsonMessage["params"] = JToken.FromObject(parameters);
            }

            // Send text message
            messageServer.SendMessage(jsonMessage.ToString(Formatting.None));
        }

        // Add Json RPC standard property
        private void PrepareJsonPRCMessage(JObject jsonMessage)
        {
            jsonMessage["jsonrpc"] = "2.0";
        }

        // Sequence number used to generate unique identifiers for the requests and responses
        private int sequenceNumber;

        // Remeber all requests sent and still waiting for a response 
        private IDictionary<string, ResponseWaitState> responsesExpected = new Dictionary<string, ResponseWaitState>();

        /// <summary>
        /// Send an async request to the client and await later for the response or error
        /// </summary>
        public Task<ResponseResultOrError> SendRequest(RequestType requestType, object parameters)
        {
            JObject jsonMessage = new JObject();
            PrepareJsonPRCMessage(jsonMessage);

            // Generate a unique id for the request
            int id = Interlocked.Increment(ref sequenceNumber);
            string requestId = id.ToString();
            jsonMessage["id"] = requestId;

            jsonMessage["method"] = requestType.Method;
            if (parameters != null)
            {
                jsonMessage["params"] = JToken.FromObject(parameters);
            }

            //  Send text message
            messageServer.SendMessage(jsonMessage.ToString(Formatting.None));

            // Remember all elements which will be needed to handle correctly the response to the request
            TaskCompletionSource<ResponseResultOrError> taskCompletionSource = new TaskCompletionSource<ResponseResultOrError>();
            ResponseWaitState responseWaitState = new ResponseWaitState(requestType, requestId, taskCompletionSource);
            responsesExpected.Add(requestId, responseWaitState);

            // The completion of the task will be signaled later, when the response arrives
            return taskCompletionSource.Task;
        }

        /// <summary>
        /// Implementation of IMessageHandler
        /// </summary>
        public void HandleMessage(string message, IMessageServer server)
        {
            JObject jsonObject = JObject.Parse(message);

            // Try to read the JsonRPC message properties
            string requestId = (string)jsonObject["id"];
            string method = (string)jsonObject["method"];
            JToken parameters = jsonObject["params"];
            JToken result = jsonObject["result"];
            JToken error = jsonObject["error"];

            // Check message type
            // -- Notification --
            if (requestId == null && method != null)
            {
                HandleNotification(method, parameters);
            }
            // -- Request --
            else if (requestId != null && method != null)
            {
                HandleRequest(method, requestId, parameters);
            }
            // -- Response --
            else if (requestId != null && (result != null || error != null))
            {
                HandleResponse(requestId, result, error);
            }
        }
        
        private void HandleNotification(string method, JToken parameters)
        {
            NotificationMethod notificationMethod = null;
            notificationMethods.TryGetValue(method, out notificationMethod);
            if(notificationMethod == null)
            {
                WriteServerLog(String.Format("No notification handler was registered for method \"{0}\"", method));
            }
            else
            {
                NotificationType notificationType = notificationMethod.Type;
                object objParams = null;
                if(parameters != null)
                {
                    objParams = parameters.ToObject(notificationType.ParamsType);
                }
                try
                {
                    notificationMethod.HandleNotification(notificationType, objParams);
                }
                catch(Exception e)
                {
                    WriteServerLog(String.Format("Notification handler for {0} failed : {1}", notificationType.GetType().Name, e.Message));
                    ResponseResultOrError error = new ResponseResultOrError() { code = ErrorCodes.InternalError, message = e.Message , data = parameters.ToString() };
                    Reply(method, error);
                }
            }
        }

        private void HandleRequest(string method, string requestId, JToken parameters)
        {
            RequestMethod requestMethod = null;
            requestMethods.TryGetValue(method, out requestMethod);
            if (requestMethod == null)
            {
                WriteServerLog(String.Format("No request handler was registered for method \"{0}\"", method));
            }
            else
            {
                RequestType requestType = requestMethod.Type;
                object objParams = null;
                if (parameters != null)
                {
                    objParams = parameters.ToObject(requestType.ParamsType);
                }
                try
                {
                    ResponseResultOrError resultOrError = requestMethod.HandleRequest(requestType, objParams);
                    Reply(requestId, resultOrError);
                }
                catch(Exception e)
                {
                    ResponseResultOrError error = new ResponseResultOrError() { code = ErrorCodes.InternalError, message = e.Message };
                    Reply(requestId, error);
                }
            }
        }

        private void Reply(string requestId, ResponseResultOrError resultOrError)
        {
            JObject jsonMessage = new JObject();
            PrepareJsonPRCMessage(jsonMessage);

            // Response properties
            jsonMessage["id"] = requestId;
            if (resultOrError.result != null)
            {
                jsonMessage["result"] = JToken.FromObject(resultOrError.result);
            }
            else if (resultOrError.code != null)
            {
                jsonMessage["error"] = JToken.FromObject(resultOrError);
            }

            //  Send text message
            messageServer.SendMessage(jsonMessage.ToString(Formatting.None));
        }

        private void HandleResponse(string requestId, JToken result, JToken error)
        {
            ResponseWaitState responseWaitState = null;
            responsesExpected.TryGetValue(requestId, out responseWaitState);
            if (responseWaitState == null)
            {
                WriteServerLog(String.Format("No response was expected for request id \"{0}\"", requestId));
            }
            else
            {
                RequestType requestType = responseWaitState.RequestType;
                object objResult = null;
                if (result != null && requestType.ResultType != null)
                {
                    objResult = result.ToObject(requestType.ResultType);
                }
                object objErrorData = null;
                if (error != null && error["data"] != null && requestType.ErrorDataType != null)
                {
                    objErrorData = error["data"].ToObject(requestType.ErrorDataType);
                }

                ResponseResultOrError resultOrError = new ResponseResultOrError();
                resultOrError.result = objResult;
                if (error != null && error["code"] != null)
                {
                    resultOrError.code = (int)error["code"];
                    resultOrError.message = (string)error["message"];
                    resultOrError.data = objErrorData;
                }
                
                try
                {
                    responseWaitState.TaskCompletionSource.SetResult(resultOrError);
                }
                catch (Exception e)
                {
                    WriteServerLog(String.Format("Task completion for the response expected by request {0} of type {1} failed : {1}", requestId, requestType.GetType().Name, e.Message));
                }
            }
        }

        /// <summary>
        /// Write a trace in the server log file
        /// </summary>
        public void WriteServerLog(string trace)
        {
            messageServer.WriteServerLog(trace);
            AnalyticsWrapper.Telemetry.TrackTrace(trace);
        }       
    }
}
