using System;
using System.Diagnostics;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer
{
    public class MessageActionWrapper
    {
        private MessageActionWrapper()
        {
            InQueueDuration = new Stopwatch();
            InQueueDuration.Start();
        }
        /// <summary>
        /// Constructor for JSonMessage Queue Element
        /// </summary>
        /// <param name="message">JSonRPC message</param>
        /// <param name="messageHandler">A class object that inherit from interface IMessageHandler</param>
        public MessageActionWrapper(string message, IMessageServer messageHandler) : this()
        {
            MessageKind = MessageKind.JSonMessage;
            Message = message;
            MessageServer = messageHandler;
        }

        /// <summary>
        /// Constructor for Action Queue Element
        /// </summary>
        /// <param name="action">Delegated Action to be executed</param>
        public MessageActionWrapper(Action action) : this()
        {
            MessageKind = MessageKind.Action;
            Action = action;
        }

        public MessageKind MessageKind { get; private set; }
        public Action Action { get; private set; }
        public string Message { get; private set; }
        public IMessageServer MessageServer { get; private set; } 
        public Stopwatch InQueueDuration { get; }

        /// <summary>
        /// Call this method when this message is dequeue and will be processed
        /// </summary>
        public void BeginProcess()
        {
            InQueueDuration.Stop();
        }
    }

    public enum MessageKind
    {
        JSonMessage,
        Action
    }
}
