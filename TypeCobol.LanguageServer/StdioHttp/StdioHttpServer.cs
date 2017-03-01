using System;
using System.IO;
using System.Text;

namespace TypeCobol.LanguageServer.StdioHttp
{
    /// <summary>
    /// Implementation of a simple Http server using the standard input and output streams
    /// </summary>
    public class StdioHttpServer : IMessageServer
    {
        // Configuration of the Http server
        private const int BUFFER_SIZE = 8192;
        private Encoding messageEncoding;
        private ServerLogLevel logLevel;
        private TextWriter logWriter;

        /// <summary>
        /// Configure the Http server
        /// </summary>
        /// <param name="messageEncoding">Encoding used for the body of the Http messages</param>
        /// <param name="logLevel">Verbosity of the logs written by the Http server</param>
        /// <param name="logWriter">Text Writer used to write all the logs of the Http server</param>
        public StdioHttpServer(Encoding messageEncoding, ServerLogLevel logLevel, TextWriter logWriter)
        {
            this.messageEncoding = messageEncoding;
            this.logLevel = logLevel;
            this.logWriter = logWriter;
            if (logWriter is StreamWriter)
                (this.logWriter as StreamWriter).AutoFlush = true;
        }

        // Shutdown request
        private bool shutdownAfterNextMessage = false;

        /// <summary>
        /// Infinite message loop : start listening for incoming Http messages on stdin.
        /// Each message received is passed to an IMessageHandler on the main thread of the loop.
        /// This method blocks the current thread until ShutdownAfterNextMessage() is called. 
        /// </summary>
        public void StartReceivingMessagesFor(IMessageHandler messageHandler)
        {
            logWriter.WriteLine(String.Format("{0} -- Server startup", DateTime.Now));

            // Infinite message loop
            char[] buffer = new char[BUFFER_SIZE];            
            for (;;)
            {
                // Receive and handle one message
                try
                {
                    // Read Http message headers                  
                    int contentLength = 0;
                    string headerLine = Console.ReadLine();
                    while (!String.IsNullOrEmpty(headerLine))
                    {
                        int headerSeparatorIndex = headerLine.IndexOf(':');
                        if (headerSeparatorIndex > 0)
                        {
                            // Ignore all headers but Content-Length
                            string headerName = headerLine.Substring(0, headerSeparatorIndex);
                            if (headerName == "Content-Length" && headerSeparatorIndex < (headerLine.Length - 2))
                            {
                                // Try to parse Content-Length
                                string headerValue = headerLine.Substring(headerSeparatorIndex + 2);
                                Int32.TryParse(headerValue, out contentLength);
                            }
                        }
                        headerLine = Console.ReadLine();
                    }

                    // If the server could not find the content length of the message
                    // it is impossible to detect where the message ends : write a fatal 
                    // error message and exit the loop
                    if (contentLength == 0)
                    {
                        logWriter.WriteLine(String.Format("{0} !! Fatal error : message without Content-Length header", DateTime.Now));
                        break;
                    }
                    else
                    {
                        // Log the size of the message received
                        if (logLevel >= ServerLogLevel.Message)
                        {
                            logWriter.WriteLine(String.Format("{0} >> Message received : Content-Length={1}", DateTime.Now, contentLength));
                        }
                    }

                    // Read Http message body
                    StringBuilder sbMessage = new StringBuilder();
                    while (contentLength > 0)
                    {
                        int nbCharsToRead = contentLength > buffer.Length ? buffer.Length : contentLength;
                        int nbCharsRead = Console.In.Read(buffer, 0, nbCharsToRead);
                        sbMessage.Append(buffer, 0, nbCharsRead);
                        contentLength -= nbCharsRead;
                    }
                    string message = sbMessage.ToString();
                    if(logLevel == ServerLogLevel.Protocol)
                    {
                        logWriter.WriteLine(message);
                        logWriter.WriteLine("----------");
                    }

                    // Handle incoming message and optionnaly send reply
                    messageHandler.HandleMessage(message, this);
                }
                catch (Exception e)
                {
                    logWriter.WriteLine(String.Format("{0} !! Exception : {1}", DateTime.Now, e.Message));
                }

                // Exit the loop after message handling if a shutdown of the server has been requested
                if (shutdownAfterNextMessage)
                {
                    break;
                }
            }

            logWriter.WriteLine(String.Format("{0} -- Server shutdown", DateTime.Now));
        }

        /// <summary>
        /// Request a shutdown of the server after handling the current or the next message
        /// </summary>
        public void ShutdownAfterNextMessage()
        {
            shutdownAfterNextMessage = true;
        }

        // Synchronized access
        private readonly object _lock = new object();

        /// <summary>
        /// Send a message to the Http client
        /// </summary>
        public void SendMessage(string message)
        {
            lock(_lock)
            {
                // Write only the Content-Length header
                int contentLength = Console.Out.Encoding.GetByteCount(message);
                Console.Out.WriteLine("Content-Length: " + contentLength);
                Console.Out.WriteLine();
                if (logLevel >= ServerLogLevel.Message)
                {
                    logWriter.WriteLine(String.Format("{0} << Message sent : Content-Length={1}", DateTime.Now, contentLength));
                }

                // Write the message body
                Console.Out.Write(message);
                if (logLevel == ServerLogLevel.Protocol)
                {
                    logWriter.WriteLine(message);
                    logWriter.WriteLine("----------");
                }
            }
        }

        /// <summary>
        /// Write a timestamped trace in the server log file
        /// </summary>
        public void WriteServerLog(string trace)
        {
            logWriter.WriteLine(String.Format("{0} -- Trace : {1}", DateTime.Now, trace));
        }

        /*/// <summary>
        /// Wrapper over a binary stream which limits the number of bytes read to Content-Length
        /// </summary>
        private class ContentLengthStream : Stream
        {
            private Stream stream;
            private int bytesToRead;

            public ContentLengthStream(Stream stream)
            {
                this.stream = stream;
                ResetSetContentLength();
            }

            public void SetContentLength(int contentLength)
            {
                this.bytesToRead = contentLength;
            }

            public void ResetSetContentLength()
            {
                this.bytesToRead = -1;
            }

            public override bool CanRead { get { return stream.CanRead; } }
            public override bool CanSeek { get { return stream.CanSeek; } }
            public override bool CanWrite { get { return stream.CanWrite; } }
            public override long Length { get { return stream.Length; } }
            public override long Position { get { return stream.Position; } set { stream.Position = value; } }
            public override void Flush() { stream.Flush(); }
            public override long Seek(long offset, SeekOrigin origin) { return stream.Seek(offset, origin); }
            public override void SetLength(long value) { stream.SetLength(value); }

            public override int Read(byte[] buffer, int offset, int count)
            {
                if(bytesToRead < 0)
                {
                    return Read(buffer, offset, count);
                }
                else if (bytesToRead == 0)
                {
                    return -1;
                }
                else
                {
                    int maxBytesCount = count <= bytesToRead ? count : bytesToRead;
                    int bytesRead = Read(buffer, offset, maxBytesCount);
                    bytesToRead -= bytesRead;
                    return bytesRead;
                }
            }

            public override void Write(byte[] buffer, int offset, int count) { stream.Write(buffer, offset, count); }
        }*/
    }
}
