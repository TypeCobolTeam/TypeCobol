using System.Text;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
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
            string[] parts = FileName.Split('.');
            Method = parts[0].Replace('_', '/');
            Action = Parse<MessageAction>(parts[1]);
            Type = Parse<MessageType>(parts[2]);
            Content = File.ReadAllText(messageFilePath, Encoding.UTF8);

            static TEnum Parse<TEnum>(string text) where TEnum : struct, Enum
            {
                if (Enum.TryParse(text, true, out TEnum result))
                {
                    return result;
                }

                throw new ArgumentException($"Cannot parse '{text}', as '{nameof(TEnum)}'.", nameof(messageFilePath));
            }
        }

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
