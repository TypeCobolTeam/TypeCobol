using System.Diagnostics;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class IndexGenerator
    {
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

        public string GetOrCreateIndex(int occursDimension, int size)
        {
            Debug.Assert(occursDimension >= 1);
            Debug.Assert(size >= 1);

            Index index;
            if (occursDimension > _indices.Count)
            {
                Debug.Assert(occursDimension == _indices.Count + 1);
                index = new Index($"Idx-{_hash}-{occursDimension}") { Size = size };
                _indices.Add(index);
            }
            else
            {
                index = _indices[occursDimension - 1];
                index.Size = Math.Max(index.Size, size);
            }

            return index.Name;
        }

        public void WriteCobolCode(CobolStringBuilder builder)
        {
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
