using System;
using System.IO;
using System.Text;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.StdioHttp;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Main executable hosting the language server
    /// </summary>
    class TypeCobolServerHost
    {
        static void Main(string[] args)
        {
            // args[0] : Trace level
            ServerLogLevel logLevel = (ServerLogLevel)Int32.Parse(args[0]);
            // args[1] : Log file name
            string logFile = args[1];
            
            // Open log file
            using (StreamWriter sw = new StreamWriter(logFile))
            {
                sw.AutoFlush = true;

                // Configure the protocols stack
                var httpServer = new StdioHttpServer(Encoding.UTF8, logLevel, sw);
                var jsonRPCServer = new JsonRPCServer(httpServer);
                var languageServer = new TypeCobolServer(jsonRPCServer);

                // Start listening to incoming request (block, infinite loop)
                httpServer.StartReceivingMessagesFor(jsonRPCServer);
            }
        }
    }
}
