using System.Collections;
using System.Collections.Generic;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Wraps a Dictionary to expose analyzer results.
    /// </summary>
    public class AnalyzerResults : IReadOnlyDictionary<string, object>
    {
        private readonly Dictionary<string, object> _data;

        internal AnalyzerResults(Dictionary<string, object> data)
        {
            _data = data;
        }

        public IEnumerator<KeyValuePair<string, object>> GetEnumerator() => _data.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public int Count => _data.Count;

        public bool ContainsKey(string key) => _data.ContainsKey(key);

        public bool TryGetValue(string key, out object value) => _data.TryGetValue(key, out value);

        /// <summary>
        /// Attempt to retrieve the result for a specific analyzer identified by its id.
        /// </summary>
        /// <typeparam name="TResult">Desired type of result.</typeparam>
        /// <param name="analyzerId">Unique string identifier of the analyzer.</param>
        /// <param name="result">The result of the analyzer if found, default(TResult) otherwise.</param>
        /// <returns>True if the result has been found, False otherwise.</returns>
        public bool TryGetResult<TResult>(string analyzerId, out TResult result)
        {
            if (_data.TryGetValue(analyzerId, out var untypedResult) && untypedResult is TResult typedResult)
            {
                result = typedResult;
                return true;
            }

            result = default;
            return false;
        }

        public object this[string key] => _data[key];

        public IEnumerable<string> Keys => _data.Keys;

        public IEnumerable<object> Values => _data.Values;
    }
}
