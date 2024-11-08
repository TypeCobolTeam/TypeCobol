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
        public static Selection Root(params Selection[] selections)
        {
            var rootSelection = new SelectionByIndex(0, NodeVisitMode.PassThrough);
            rootSelection.SubSelections.AddRange(selections);
            return rootSelection;
        }

        public NodeVisitMode VisitMode { get; }

        public List<Selection> SubSelections { get; }

        protected Selection(NodeVisitMode visitMode)
        {
            VisitMode = visitMode;
            SubSelections = new List<Selection>();
        }

        public abstract Node SelectChild(Node parent);
    }

    internal class SelectionByName : Selection
    {
        public string Name { get; }

        public SelectionByName(string name, NodeVisitMode visitMode)
            : base(visitMode)
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

        public SelectionByIndex(int index, NodeVisitMode visitMode)
            : base(visitMode)
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
            : base(NodeVisitMode.Automatic)
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
                bool nodeHasSelectedChildren = selection.SubSelections.Count > 0 || selection.VisitMode == NodeVisitMode.Automatic;
                ProcessNode(node, nodeHasSelectedChildren);
            }
            VisitChildren(selection, node);
            ExitNode(node);
        }

        protected abstract void EnterNode(Node node);

        protected abstract void ProcessNode(Node node, bool nodeHasSelectedChildren);

        protected abstract void ExitNode(Node node);
    }
}
