using System.Diagnostics;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Generate index variables required when iterating arrays.
    /// </summary>
    internal class IndexGenerator : ICobolCodeProvider
    {
        /// <summary>
        /// An index is defined by a name and its size, which is the number of digits in its PICTURE.
        /// </summary>
        /// <param name="Name">Name of the index when created.</param>
        private record Index(string Name)
        {
            public int Size { get; set; }
        }

        private readonly string _hash;
        private readonly List<Index> _indices;

        public IndexGenerator(string hash)
        {
            _hash = hash;
            _indices = new List<Index>();
        }

        public bool HasContent => _indices.Count > 0;

        /// <summary>
        /// Get the index name for the given OCCURS dimension.
        /// If no index exists yet for this dimension, a new index is created.
        /// </summary>
        /// <param name="occursDimension">OCCURS dimension, starts at 1.</param>
        /// <param name="size">Number of digits required to accomodate max index value.</param>
        /// <returns>Name of the index to use for this dimension.</returns>
        public string GetOrCreateIndex(int occursDimension, int size)
        {
            Debug.Assert(occursDimension >= 1);
            Debug.Assert(size >= 1);

            Index index;
            if (occursDimension > _indices.Count)
            {
                // Create new index
                Debug.Assert(occursDimension == _indices.Count + 1); // Check no gap
                index = new Index($"Idx-{_hash}-{occursDimension}") { Size = size };
                _indices.Add(index);
            }
            else
            {
                // Reuse index and update size
                index = _indices[occursDimension - 1];
                index.Size = Math.Max(index.Size, size);
            }

            return index.Name;
        }

        public void WriteCobolCode(CobolStringBuilder builder)
        {
            // Each index is generated as a 77 level, PIC 9(<size>) and usage COMP.
            foreach (var index in _indices)
            {
                builder.AppendWord("77");
                builder.AppendWord(index.Name);
                builder.AppendWord("PIC");
                builder.AppendWord($"9({index.Size})");
                builder.AppendWord("COMP.");
                builder.AppendLine();
            }
        }
    }
}
