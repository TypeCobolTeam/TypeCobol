using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.StdioHttp;
using TypeCobol.LanguageServer.Utilities;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Main executable hosting the language server
    /// </summary>
    class TypeCobolServerHost
    {
        public static Queue<MessageActionWrapper> MessagesActionQueue { get; set; }
        /// <summary>
        /// Main Entry point of the Server.
        /// </summary>
        /// <param name="args">Arguments: arg[0] the LogLevel (0=Lifecycle,1=Message,2=Protocol) - args[1] a Log File</param>
        /// <see cref="TypeCobol.LanguageServer.StdioHttp.ServerLogLevel"/>
        static void Main(string[] args)
        {
            ServerLogLevel logLevel;
            TextWriter logWriter;
            //Queue storing messages coming from client, this queue is read by readingThread
            MessagesActionQueue = new Queue<MessageActionWrapper>();

            GetArguments(args, out logLevel, out logWriter);
            // Open log file
            try
            {
                // Configure the protocols stack
                var httpServer = new StdioHttpServer(Encoding.UTF8, logLevel, logWriter, MessagesActionQueue);
                var jsonRPCServer = new JsonRPCServer(httpServer);
                var typeCobolServer = new TypeCobolServer(jsonRPCServer, MessagesActionQueue);

                //Creating the thread that will read mesages and handle them 
                var backgroundExecutionThread = new Thread(() => { MessageHandler(jsonRPCServer, typeCobolServer); }) {IsBackground = true};
                backgroundExecutionThread.Start();

                // Start listening to incoming request (block, infinite loop)
                httpServer.StartReceivingMessagesFor(jsonRPCServer);
            }
            finally
            {
                if (logWriter != System.Console.Error)
                {
                    logWriter.Close();
                }
            }
        }

        /// <summary>
        /// Method of backgroundExecutionThread. It will Loop until the end of main Thread.
        /// This method is going to read the MessagesActionQueue and do associated action.
        /// In case of JsonRPC message, it will be pass to a MessageHandler.
        /// In case of an action, it will execute it. 
        /// </summary>
        /// <param name="messageHandler"></param>
        /// <param name="typeCobolServer"></param>
        static void MessageHandler(IMessageHandler messageHandler, TypeCobolServer typeCobolServer)
        {
            while (true)
            {
                Thread.Sleep(1); //To preserve processor use

                MessageActionWrapper messageActionWrapper = null;
                lock (MessagesActionQueue)
                {
                    if (MessagesActionQueue.Any())
                    {
                        messageActionWrapper = MessagesActionQueue.Dequeue(); //Pop out message from queue
                    }
                }
                if (messageActionWrapper == null)
                    continue;

                if (messageActionWrapper.MessageKind == MessageKind.JSonMessage)
                    messageHandler.HandleMessage(messageActionWrapper.Message, messageActionWrapper.MessageServer); //Give this mesage to the real handler
                else if (messageActionWrapper.MessageKind == MessageKind.Action)
                {
                    try
                    {
                        messageActionWrapper.Action(); //Execute queued action
                    }
                    catch (Exception e)
                    {
                        typeCobolServer.NotifyException(e);
                    }
                }
                    
            }
        }

        /// <summary>
        /// Collect Arguments
        /// </summary>
        /// <param name="args">The arguments</param>
        /// <param name="logLevel">Output: The LogLevel from the arguments</param>
        /// <param name="logWriter">Output: The Log Writer from the arguments</param>
        static void GetArguments(String[] args, out ServerLogLevel logLevel, out TextWriter logWriter)
        {
            logLevel = ServerLogLevel.Lifecycle;
            logWriter = null;
            try
            {
                if (args != null && args.Length != 0)
                {//Standard output
                    try
                    {
                        // args[0] : Trace level
                        logLevel = (ServerLogLevel)Int32.Parse(args[0]);
                        if (!System.Enum.IsDefined(typeof(ServerLogLevel), (Int32)logLevel)) ;
                        {
                            logLevel = ServerLogLevel.Protocol;
                        }
                    }
                    catch (Exception e)
                    {
                        System.Console.Error.WriteLine(e.Message);
                    }

                    if (args.Length > 1)
                    {
                        // Open log file
                        // args[1] : Log file name
                        string logFile = args[1];
                        try
                        {
                            StreamWriter sw = new StreamWriter(logFile);
                            sw.AutoFlush = true;
                            logWriter = sw;
                        }
                        catch (Exception e)
                        {
                            System.Console.Error.WriteLine(e.Message);
                        }
                    }
                }
            }
            finally
            {
                if (logWriter == null)
                {
                    logWriter = new DebugTextWriter();
                }
            }
        }
    }
}
