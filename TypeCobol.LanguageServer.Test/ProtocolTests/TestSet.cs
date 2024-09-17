using System.Collections;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    internal class TestSet : IEnumerable<TestMessage>
    {
        public static TestSet Build(string messageFilesDirectory)
        {
            var testMessages = Directory.EnumerateFiles(messageFilesDirectory)
                .Select(messageFilePath => new TestMessage(messageFilePath))
                .ToList();
            return new TestSet(testMessages);
        }

        private readonly List<TestMessage> _testMessages;

        private TestSet(List<TestMessage> testMessages)
        {
            _testMessages = testMessages;
        }

        public IEnumerator<TestMessage> GetEnumerator() => _testMessages.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public void Validate(Dictionary<string, LspMethodDefinition> methods)
        {
            var untested = new Dictionary<string, LspMethodDefinition>(methods);
            var unknown = new List<string>();
            var messagesByMethod = this.ToLookup(testMessage => testMessage.Method);
            foreach (var messageGroup in messagesByMethod)
            {
                string method = messageGroup.Key;
                if (!untested.Remove(method))
                {
                    unknown.Add(method);
                }
            }

            AssertEmptyCollection(nameof(untested), untested.Keys.ToList());
            AssertEmptyCollection(nameof(unknown), unknown);

            static void AssertEmptyCollection(string category, List<string> collection)
            {
                if (collection.Count == 0) return;

                var error = new StringBuilder();
                error.AppendLine($"Found {category} methods:");
                foreach (var method in collection)
                {
                    error.Append("'");
                    error.Append(method);
                    error.AppendLine("'");
                }

                Assert.Fail(error.ToString());
            }
        }
    }
}
