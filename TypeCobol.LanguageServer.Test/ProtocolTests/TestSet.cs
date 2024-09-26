using System.Collections;
using System.Text;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.Test.ProtocolTests
{
    /// <summary>
    /// Wrap a list of TestMessage and provide minimal consistency check
    /// </summary>
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

#if !EUROINFO_RULES
            // In non-EI code ExtractRemarksData is not defined but the test messages for this method are still there, so we have to remove them
            _testMessages.RemoveAll(testMessage => testMessage.Method == "typecobol/euroinformation/ExtractRemarksData");
#endif
        }

        public IEnumerator<TestMessage> GetEnumerator() => _testMessages.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public void Validate(Dictionary<string, LspMethodDefinition> methods)
        {
            // Detect untested methods: methods that are known by this server but without any associated test file
            // and unknown methods: methods having a test file but not known by this server
            var untested = new Dictionary<string, LspMethodDefinition>(methods);
            var unknown = new List<string>();
            var testedMethods = this.Select(testMessage => testMessage.Method).ToHashSet();
            foreach (var testedMethod in testedMethods)
            {
                if (!untested.Remove(testedMethod))
                {
                    unknown.Add(testedMethod);
                }
            }

            // Both should be empty
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

                throw new Exception(error.ToString());
            }
        }
    }
}
