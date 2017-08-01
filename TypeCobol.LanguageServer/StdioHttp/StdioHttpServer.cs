using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;

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
        public const string CONTENT_LENGTH_HEADER = "Content-Length";
        public const string CONTENT_TYPE_HEADER = "Content-Type";


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
            Console.OutputEncoding = messageEncoding ?? Encoding.UTF8;
            Console.InputEncoding = messageEncoding ?? Encoding.UTF8;
            if (logWriter is StreamWriter)
                (this.logWriter as StreamWriter).AutoFlush = true;
        }

        // Shutdown request
        private bool shutdownAfterNextMessage = false;

        /// <summary>
        /// Structure of Messages Headers
        /// </summary>
        protected class Headers
        {
            public int contentLength;
            public string charset;
            public Headers()
            {
                Reset();
            }

            public void Reset()
            {
                this.contentLength = -1;
                this.charset = Encoding.UTF8.BodyName;
            }
        }

        /// <summary>
        /// Parse the Message Header Line
        /// </summary>
        /// <param name="line"></param>
        /// <param name="headers"></param>
        /// <returns></returns>

        protected void ParseHeader(String line, Headers headers)
        {
            int sepIndex = line.IndexOf(':');
            if (sepIndex >= 0)
            {
                String key = line.Substring(0, sepIndex).Trim();
                switch (key)
                {
                    case CONTENT_LENGTH_HEADER:
                            Int32.TryParse(line.Substring(sepIndex + 1).Trim(), out headers.contentLength);
                        break;
                    case CONTENT_TYPE_HEADER:
                        {
                            int charsetIndex = line.IndexOf("charset=");
                            if (charsetIndex >= 0)
                                headers.charset = line.Substring(charsetIndex + 8).Trim();
                            break;
                        }
                }
            }
        }

        /// <summary>
        /// Handle a Receive message for Stdin
        /// </summary>
        /// <param name="messageHandler">The Message Handler</param>
        /// <param name="stdin">The Stdin instance</param>
        /// <param name="headers">The Message Header</param>
        /// <param name="buffer">The read buffer</param>
        /// <returns>true if the message has been handled, false otherwise</returns>
        protected bool HandleMessage(IMessageHandler messageHandler, Stream stdin, Headers headers, byte[] buffer)
        {
            // If the server could not find the content length of the message
            // it is impossible to detect where the message ends : write a fatal 
            // error message and exit the loop
            if (headers.contentLength == 0)
            {
                logWriter.WriteLine(String.Format("{0} !! Fatal error : message without Content-Length header", DateTime.Now));
                return false;
            }
            else
            {
                // Log the size of the message received
                if (logLevel >= ServerLogLevel.Message)
                {
                    logWriter.WriteLine(String.Format("{0} >> Message received : Content-Length={1}", DateTime.Now, headers.contentLength));
                }
            }

            // Read Http message body
            using (MemoryStream stream = new MemoryStream(headers.contentLength))
            {
                while (headers.contentLength > 0)
                {
                    int nbCharsToRead = headers.contentLength > buffer.Length ? buffer.Length : headers.contentLength;
                    int nbCharsRead = stdin.Read(buffer, 0, nbCharsToRead);
                    stream.Write(buffer, 0, nbCharsRead);
                    headers.contentLength -= nbCharsRead;
                }
                Encoding encoding = headers.charset == Encoding.UTF8.BodyName ? Encoding.UTF8 : Encoding.GetEncoding(headers.charset);
                if (encoding == null) {
                    logWriter.WriteLine(String.Format("{0} >> Fail to get encoding : {1} --> using default encoding UTF-8", DateTime.Now, headers.charset));
                    encoding = Encoding.UTF8;
                }
                string message = encoding.GetString(stream.ToArray()); ;
                if (logLevel == ServerLogLevel.Protocol)
                {
                    logWriter.WriteLine(message);
                    logWriter.WriteLine("----------");
                }

                // Handle incoming message and optionnaly send reply
                messageHandler.HandleMessage(message, this);
            }
            return true;
        }
        /// <summary>
        /// Infinite message loop : start listening for incoming Http messages on stdin.
        /// Each message received is passed to an IMessageHandler on the main thread of the loop.
        /// This method blocks the current thread until ShutdownAfterNextMessage() is called. 
        /// </summary>
        public void StartReceivingMessagesFor(IMessageHandler messageHandler)
        {
            logWriter.WriteLine(String.Format("{0} -- Server startup", DateTime.Now));

            StringBuilder headerBuilder = null;
            bool newLine = false;
            Headers headers = new Headers();
            // Infinite message loop
            using (Stream stdin = Console.OpenStandardInput())
            {
                byte[] buffer = new byte[BUFFER_SIZE];
                for (;;)
                {
                    // Receive and handle one message
                    try
                    {
                        // Read Http message headers                  
                        int c = stdin.ReadByte();
                        if (c == -1)
                            break;//End of input stream has been reached???
                        else
                        {
                            if (c == '\n')
                            {
                                if (newLine)
                                {
                                    // Two consecutive newlines have been read, which signals the start of the message content
                                    if (headers.contentLength < 0)
                                    {
                                        logWriter.WriteLine(String.Format("{0} !! Fatal error : message without Content-Length header", DateTime.Now));
                                        break;
                                    }
                                    else
                                    {
                                        bool result = HandleMessage(messageHandler, stdin, headers, buffer);
                                        if (!result)
                                            break;
                                        newLine = false;
                                    }
                                    headers.Reset();
                                }
                                else if (headerBuilder != null)
                                {
                                    // A single newline ends a header line
                                    ParseHeader(headerBuilder.ToString(), headers);
                                    headerBuilder = null;
                                }
                                newLine = true;
                            }
                            else if (c != '\r')
                            {
                                // Add the character to the current header line
                                if (headerBuilder == null)
                                    headerBuilder = new StringBuilder();
                                headerBuilder.Append((char)c);
                                newLine = false;
                            }
                        }
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
            Action<object> action = (object msg) =>
            {
                lock (_lock)
                {
                    string the_message = (string)msg;
                    // Write only the Content-Length header
                    int contentLength = Console.Out.Encoding.GetByteCount(the_message);
                    Console.Out.WriteLine("Content-Length: " + contentLength);
                    Console.Out.WriteLine();
                    if (logLevel >= ServerLogLevel.Message)
                    {
                        logWriter.WriteLine(String.Format("{0} << Message sent : Content-Length={1}", DateTime.Now, contentLength));
                    }

                    // Write the message body
                    Console.Out.Write(the_message);
                    if (logLevel == ServerLogLevel.Protocol)
                    {
                        logWriter.WriteLine(the_message);
                        logWriter.WriteLine("----------");
                    }
                }
            };
            Task tsend = new Task(action, message);
            tsend.Start();
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
