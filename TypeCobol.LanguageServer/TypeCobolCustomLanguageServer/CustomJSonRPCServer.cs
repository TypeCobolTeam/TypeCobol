using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer
{
    public class CustomJSonRPCServer : JsonRPCServer
    {
        /// <summary>
        /// This action queue will allow other Thread to store action to be done right after any handled message by JSonRPCServer. 
        /// Any thread can store an Action to execute and the main Thread will run it. 
        /// </summary>
        public Stack<Action> ActionQueue { get; set; }

        public CustomJSonRPCServer(IMessageServer messageServer) : base(messageServer)
        {
            ActionQueue = new Stack<Action>();
        }

        public override void HandleMessage(string message, IMessageServer server)
        {
            //Check if queued actions needs to be done
            lock (ActionQueue)
            {
                while (ActionQueue.Any())
                {
                    var action = ActionQueue.Pop();
                    action(); //Execute acion on the main Thread
                }
            }

            base.HandleMessage(message, server);
        }
    }
}

