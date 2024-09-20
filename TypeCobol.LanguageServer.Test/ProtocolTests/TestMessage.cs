using System.Text;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    /// <summary>
    /// Test data needed to test one single message. It can be either a request,
    /// a notification or a response. The message can be either sent to the client or received from the client.
    /// </summary>
    internal class TestMessage
    {
        public string FileName { get; }

        public string Method { get; }

        public MessageAction Action { get; }

        public MessageType Type { get; }

        public string Content { get; }

        public TestMessage(string messageFilePath)
        {
            FileName = Path.GetFileName(messageFilePath);

            // Parse file name
            string[] parts = FileName.Split('.');
            Method = parts[0].Replace('_', '/');
            Action = Parse<MessageAction>(parts[1]);
            Type = Parse<MessageType>(parts[2]);

            // Read content
            Content = File.ReadAllText(messageFilePath, Encoding.UTF8);

            static TEnum Parse<TEnum>(string text) where TEnum : struct, Enum // .NET oddity: a type constrained to be an Enum is apparently not necessarily a struct for the compiler so using both constraints here
            {
                if (Enum.TryParse(text, true, out TEnum result))
                {
                    return result;
                }

                throw new ArgumentException($"Cannot parse '{text}', as '{nameof(TEnum)}'.", nameof(messageFilePath));
            }
        }

        /// <summary>
        /// Wrap the content of the message either as a param or as a result depending on message nature.
        /// </summary>
        /// <param name="requestId">In case of a request (or a response), the id to use for this message.</param>
        /// <returns>Message content wrapped, ready to be sent to client.</returns>
        public string WrapContent(string requestId)
        {
            return Type switch
            {
                MessageType.Notification => @$"{{""jsonrpc"":""2.0"",""method"":""{Method}"",""params"":{Content}}}",
                MessageType.Request => @$"{{""jsonrpc"":""2.0"",""id"":""{requestId}"",""method"":""{Method}"",""params"":{Content}}}",
                MessageType.Response => @$"{{""jsonrpc"":""2.0"",""id"":""{requestId}"",""result"":{Content}}}",
                _ => Content
            };
        }
    }
}
