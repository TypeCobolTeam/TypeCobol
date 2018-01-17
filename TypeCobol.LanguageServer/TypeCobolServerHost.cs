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
        public static Queue<Tuple<string, IMessageServer>> MessagesQueue { get; set; }
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
            MessagesQueue = new Queue<Tuple<string, IMessageServer>>();

            GetArguments(args, out logLevel, out logWriter);
            // Open log file
            try
            {
                // Configure the protocols stack
                var httpServer = new StdioHttpServer(Encoding.UTF8, logLevel, logWriter, MessagesQueue);
                var jsonRPCServer = new CustomJSonRPCServer(httpServer);
                new TypeCobolServer(jsonRPCServer);

                //Creating the thread that will read mesages and handle them 
                var readingThread = new Thread(() => { MessageHandler(jsonRPCServer); }) {IsBackground = true};
                readingThread.Start();

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

        static void MessageHandler(IMessageHandler messageHandler)
        {
            while (true)
            {
                Tuple<string, IMessageServer> message = null;
                lock (MessagesQueue)
                {
                    if (MessagesQueue.Any())
                    {
                        message = MessagesQueue.Dequeue(); //Pop out message from queue
                    }
                }
                if (message != null)
                    messageHandler.HandleMessage(message.Item1, message.Item2); //Give this mesage to the real handler

                Thread.Sleep(1); //To preserve processor use
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
