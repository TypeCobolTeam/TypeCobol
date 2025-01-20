using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Extends SelectedNodeVisitor to build a series of DISPLAY statements,
    /// one for each selected node.
    /// </summary>
    internal class DataDefinitionToDisplayVisitor : SelectedNodeVisitor
    {
        private readonly IndexGenerator _indexGenerator;
        private int _dataLogicalLevel;
        private int _occursDimension;
        private readonly Stack<string> _indices;
        private GeneratedStatement _currentStatement;

        public DataDefinitionToDisplayVisitor(Selection rootSelection, IndexGenerator indexGenerator)
            : base(rootSelection)
        {
            _indexGenerator = indexGenerator;
            _dataLogicalLevel = -1; // Starts at -1 so the 01 levels have a logical level of 0 after being entered
            _occursDimension = 0;
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
                _occursDimension++;

                // Compute index size
                string max = dataDefinition.MaxOccurencesCount.ToString();
                int indexSize = max.Length;

                // Generate IF IS NUMERIC AND IN RANGE if need be
                if (dataDefinition.OccursDependingOn != null)
                {
                    // Depending on has to be checked, and it becomes the max for the PERFORM
                    string maxOccurs = max;
                    max = dataDefinition.OccursDependingOn.MainSymbolReference.Name;

                    // Check depending on type: we should not check class when the depending on is binary
                    var dependingOn = dataDefinition.GetDataDefinitionFromStorageAreaDictionary(dataDefinition.OccursDependingOn.StorageArea, true);
                    var dependingOnUsage = dependingOn?.SemanticData?.Type?.Usage;
                    bool checkNumeric = dependingOnUsage != Compiler.Types.Type.UsageFormat.Comp && dependingOnUsage != Compiler.Types.Type.UsageFormat.Comp5;
                    string errorMessage = $"Cannot DISPLAY \"{dataDefinition.Name}\" because its DEPENDING ON \"{max}\" is {(checkNumeric ? "either not numeric or" : "")} out of range.";

                    // Create IF statement and continue generation inside it
                    var ifNumericAndInRange = new GeneratedIfIsNumericAndInRangeElseDisplayMessage(max, checkNumeric, maxOccurs, errorMessage);
                    _currentStatement.AddChild(ifNumericAndInRange);
                    _currentStatement = ifNumericAndInRange;
                }

                // Reuse or generate new index
                string index = _indexGenerator.GetOrCreateIndex(_occursDimension, indexSize);
                _indices.Push(index);

                // Generate PERFORM and continue generation inside it
                var perform = new GeneratedPerform(index, max);
                _currentStatement.AddChild(perform);
                _currentStatement = perform;
            }
        }

        private void ProcessDataDefinition(DataDefinition dataDefinition, bool dataDefinitionHasSelectedChildren)
        {
            var indices = _indices.Reverse().ToArray();
            var accessor = DataDefinitionHelper.GetAccessor(dataDefinition, indices);
            if (accessor.Data == null)
            {
                // Cannot generate DISPLAY statement
                return;
            }

            // Generate DISPLAY
            bool withValue = !IsGroup(dataDefinition) || !dataDefinitionHasSelectedChildren;
            var display = new GeneratedDisplayVariable(_dataLogicalLevel, dataDefinition, accessor, indices, withValue);
            _currentStatement.AddChild(display);
        }

        // TODO factorize with #2697
        private static bool IsGroup(DataDefinition dataDefinition) => dataDefinition.ChildrenCount > 0;

        private void ExitDataDefinition(DataDefinition dataDefinition)
        {
            // Mirror the EnterDataDefinition method
            if (dataDefinition.IsTableOccurence)
            {
                // Exit PERFORM
                _currentStatement = _currentStatement.Parent;

                if (dataDefinition.OccursDependingOn != null)
                {
                    // Exit IF IS NUMERIC
                    _currentStatement = _currentStatement.Parent;
                }

                // Pop innermost index as we have exited the PERFORM
                _indices.Pop();

                _occursDimension--;
            }

            _dataLogicalLevel--;
        }
    }
}
