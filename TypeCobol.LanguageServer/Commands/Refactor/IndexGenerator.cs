namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class IndexGenerator
    {
        private record struct Index(string Name, int Size);

        private readonly string _hash;
        private readonly List<Index> _indices;

        public IndexGenerator(string hash)
        {
            _hash = hash;
            _indices = new List<Index>();
        }

        public string GenerateNextIndex(int size)
        {
            var newIndex = new Index($"Idx-{_hash}-{_indices.Count + 1}", size);
            _indices.Add(newIndex);
            return newIndex.Name;
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
