using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal class DataDefinitionToDisplayVisitor : SelectedNodeVisitor
    {
        private readonly string _hash;
        private int _dataLogicalLevel;
        private readonly Stack<string> _indices;
        private GeneratedStatement _currentStatement;

        public DataDefinitionToDisplayVisitor(string hash, Selection rootSelection)
            : base(rootSelection)
        {
            _hash = hash;
            _dataLogicalLevel = -1;
            _indices = new Stack<string>();
            GeneratedStatements = new GeneratedRoot();
            _currentStatement = GeneratedStatements;
        }

        public GeneratedRoot GeneratedStatements { get; }

        protected override IEnumerable<Node> SelectChildren(Node parent)
        {
            // Auto select mode: all data descriptions, except anonymous
            foreach (var child in parent.Children.OfType<DataDescription>())
            {
                if (string.IsNullOrEmpty(child.Name)) continue;
                yield return child;
            }
        }

        protected override void EnterNode(Node node)
        {
            if (node is DataDefinition dataDefinition)
            {
                EnterDataDefinition(dataDefinition);
            }
        }

        protected override void ProcessNode(Node node, bool nodeHasSelectedChildren)
        {
            if (node is DataDefinition dataDefinition)
            {
                ProcessDataDefinition(dataDefinition, nodeHasSelectedChildren);
            }
        }

        protected override void ExitNode(Node node)
        {
            if (node is DataDefinition dataDefinition)
            {
                ExitDataDefinition(dataDefinition);
            }
        }

        private void EnterDataDefinition(DataDefinition dataDefinition)
        {
            _dataLogicalLevel++;

            // Generate PERFORM if need be
            if (dataDefinition.IsTableOccurence)
            {
                string index = $"Idx-{_hash}-{_indices.Count + 1}";
                _indices.Push(index);

                GeneratedPerform perform;

                // Generate IF IS NUMERIC if need be
                if (dataDefinition.OccursDependingOn != null)
                {
                    string max = dataDefinition.OccursDependingOn.MainSymbolReference.Name;
                    var @if = new GeneratedIf($"{max} IS NUMERIC"); // TODO Else ?
                    _currentStatement.AddChild(@if);
                    _currentStatement = @if;
                    perform = new GeneratedPerform(index, max);

                }
                else
                {
                    string max = dataDefinition.MaxOccurencesCount.ToString();
                    perform = new GeneratedPerform(index, max);
                }

                _currentStatement.AddChild(perform);
                _currentStatement = perform;
            }
        }

        private void ProcessDataDefinition(DataDefinition dataDefinition, bool dataDefinitionHasSelectedChildren)
        {
            var indices = _indices.Reverse().ToArray();
            var accessor = DataDefinitionHelper.GetClosestAccessor(dataDefinition, indices);
            if (accessor.Name == null)
            {
                // Cannot generate DISPLAY statement
                return;
            }

            GeneratedDisplay display;
            if (accessor.ReferenceModifier != null)
            {
                // FILLER / Anonymous data
                display = new GeneratedDisplay(_dataLogicalLevel, accessor.Name, indices, accessor.ReferenceModifier);
            }
            else
            {
                // Named data
                bool withValue = !DataDefinitionHelper.IsGroup(dataDefinition) || !dataDefinitionHasSelectedChildren;
                display = new GeneratedDisplay(_dataLogicalLevel, dataDefinition.Name, indices, withValue);
            }

            _currentStatement.AddChild(display);
        }

        private void ExitDataDefinition(DataDefinition dataDefinition)
        {
            if (dataDefinition.IsTableOccurence)
            {
                // Exit PERFORM
                _currentStatement = _currentStatement.Parent;

                if (dataDefinition.OccursDependingOn != null)
                {
                    // Exit IF IS NUMERIC
                    _currentStatement = _currentStatement.Parent;
                }

                _indices.Pop();
            }

            _dataLogicalLevel--;
        }
    }
}
