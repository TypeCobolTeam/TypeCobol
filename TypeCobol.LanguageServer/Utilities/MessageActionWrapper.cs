using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer
{
    public class MessageActionWrapper
    {
        /// <summary>
        /// Constructor for JSonMessage Queue Element
        /// </summary>
        /// <param name="message">JSonRPC message</param>
        /// <param name="messageHandler">A class object that inherit from interface IMessageHandler</param>
        public MessageActionWrapper(string message, IMessageServer messageHandler)
        {
            MessageKind = MessageKind.JSonMessage;
            Message = message;
            MessageServer = messageHandler;
        }

        /// <summary>
        /// Constructor for Action Queue Element
        /// </summary>
        /// <param name="action">Delegated Action to be executed</param>
        public MessageActionWrapper(Action action)
        {
            MessageKind = MessageKind.Action;
            Action = action;
        }

        public MessageKind MessageKind { get; private set; }
        public Action Action { get; private set; }
        public string Message { get; private set; }
        public IMessageServer MessageServer { get; private set; } 
    }

    public enum MessageKind
    {
        JSonMessage,
        Action
    }
}
