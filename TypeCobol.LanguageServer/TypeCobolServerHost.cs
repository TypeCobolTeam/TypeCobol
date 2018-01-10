using System;
using System.IO;
using System.Text;
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
        /// <summary>
        /// Main Entry point of the Server.
        /// </summary>
        /// <param name="args">Arguments: arg[0] the LogLevel (0=Lifecycle,1=Message,2=Protocol) - args[1] a Log File</param>
        /// <see cref="TypeCobol.LanguageServer.StdioHttp.ServerLogLevel"/>
        static void Main(string[] args)
        {
            ServerLogLevel logLevel;
            TextWriter logWriter;

            GetArguments(args, out logLevel, out logWriter);
            // Open log file
            try
            {
                // Configure the protocols stack
                var httpServer = new StdioHttpServer(Encoding.UTF8, logLevel, logWriter);
                var jsonRPCServer = new CustomJSonRPCServer(httpServer);
                var languageServer = new TypeCobolServer(jsonRPCServer);

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
