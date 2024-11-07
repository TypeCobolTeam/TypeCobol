using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    internal enum NodeVisitMode
    {
        Default,
        PassThrough,
        Automatic
    }

    internal abstract class Selection
    {
        public NodeVisitMode VisitMode { get; }

        public List<Selection> SubSelections { get; }

        protected Selection(NodeVisitMode visitMode, List<Selection> subSelections)
        {
            VisitMode = visitMode;
            SubSelections = subSelections;
        }

        public abstract Node SelectChild(Node parent);
    }

    internal class SelectionByName : Selection
    {
        public string Name { get; }

        public SelectionByName(string name, NodeVisitMode visitMode, List<Selection> subSelections)
            : base(visitMode, subSelections)
        {
            Name = name;
        }

        public override Node SelectChild(Node parent)
        {
            return parent.Children.FirstOrDefault(child => child.Name == Name);
        }
    }

    internal class SelectionByIndex : Selection
    {
        public int Index { get; }

        public SelectionByIndex(int index, NodeVisitMode visitMode, List<Selection> subSelections)
            : base(visitMode, subSelections)
        {
            Index = index;
        }

        public override Node SelectChild(Node parent)
        {
            return Index < parent.ChildrenCount ? parent.Children[Index] : null;
        }
    }

    internal class AutoSelection : Selection
    {
        public static AutoSelection Instance = new();

        private AutoSelection()
            : base(NodeVisitMode.Automatic, null)
        {

        }

        public override Node SelectChild(Node parent)
        {
            throw new InvalidOperationException();
        }
    }

    internal abstract class SelectedNodeVisitor
    {
        private readonly Selection _rootSelection;

        protected SelectedNodeVisitor(Selection rootSelection)
        {
            _rootSelection = rootSelection;
        }

        public void Visit(Node root)
        {
            VisitChildren(_rootSelection, root);
        }

        private void VisitChildren(Selection selection, Node node)
        {
            if (selection.VisitMode == NodeVisitMode.Automatic)
            {
                foreach (var child in SelectChildren(node))
                {
                    VisitNode(AutoSelection.Instance, child);
                }
            }
            else
            {
                foreach (var subSelection in selection.SubSelections)
                {
                    var child = subSelection.SelectChild(node);
                    if (child != null)
                    {
                        VisitNode(subSelection, child);
                    }
                    // else: inconsistent selection from client ?
                }
            }
        }

        protected abstract IEnumerable<Node> SelectChildren(Node parent);

        private void VisitNode(Selection selection, Node node)
        {
            EnterNode(node);
            if (selection.VisitMode != NodeVisitMode.PassThrough)
            {
                ProcessNode(node);
            }
            VisitChildren(selection, node);
            ExitNode(node);
        }

        protected abstract void EnterNode(Node node);

        protected abstract void ProcessNode(Node node);

        protected abstract void ExitNode(Node node);
    }

    internal class DataDefinitionToDisplayVisitor : SelectedNodeVisitor
    {
        private record State();

        private readonly Stack<State> _state;

        public DataDefinitionToDisplayVisitor(Selection rootSelection)
            : base(rootSelection)
        {
            _state = new Stack<State>();
            _state.Push(new State()); // Push initial state
        }

        protected override IEnumerable<Node> SelectChildren(Node parent)
        {
            // Auto select mode: all data definitions, except anonymous and REDEFINES
            foreach (var child in parent.Children.OfType<DataDefinition>())
            {
                if (child.Type == CodeElementType.DataRedefinesEntry) continue;
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

        protected override void ProcessNode(Node node)
        {
            if (node is DataDefinition dataDefinition)
            {
                ProcessDataDefinition(dataDefinition);
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
            // Update state
            var currentState = _state.Peek();
            _state.Push(currentState with { });

            // Generate PERFORM if need be
            if (dataDefinition.IsTableOccurence && dataDefinition.MaxOccurencesCount > 1)
            {
                // TODO GeneratePerform();
            }
        }

        private void ProcessDataDefinition(DataDefinition dataDefinition)
        {
            // Generate DISPLAY
            // TODO GenerateDisplay();
        }

        private void ExitDataDefinition(DataDefinition dataDefinition)
        {
            // Restore state
            _state.Pop();
        }
    }
}
