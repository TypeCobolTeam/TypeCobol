using System.Diagnostics;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Generate index variables required when iterating arrays.
    /// </summary>
    internal class IndexGenerator : ICobolCodeProvider
    {
        private readonly string _hash;
        private readonly List<string> _indices;

        public IndexGenerator(string hash)
        {
            _hash = hash;
            _indices = new List<string>();
        }

        public bool HasContent => _indices.Count > 0;

        /// <summary>
        /// Get the index name for the given OCCURS dimension.
        /// If no index exists yet for this dimension, a new index is created.
        /// </summary>
        /// <param name="occursDimension">OCCURS dimension, starts at 1.</param>
        /// <returns>Name of the index to use for this dimension.</returns>
        public string GetOrCreateIndex(int occursDimension)
        {
            Debug.Assert(occursDimension >= 1);

            string index;
            if (occursDimension > _indices.Count)
            {
                // Create new index
                Debug.Assert(occursDimension == _indices.Count + 1); // Check no gap
                index = $"Idx-{_hash}-{occursDimension}";
                _indices.Add(index);
            }
            else
            {
                // Reuse index
                index = _indices[occursDimension - 1];
            }

            return index;
        }

        public void WriteCobolCode(CobolStringBuilder builder)
        {
            // Each index is generated as a 77 level, PIC 9(4) COMP-5. This allows to iterate arrays having less than 65535 items.
            foreach (var index in _indices)
            {
                // As index name is no longer than 15 chars, the whole declaration fit on one line
                builder.AppendWord($"77 {index} PIC 9(4) COMP-5.");
                builder.AppendLine();
            }
        }
    }
}
