using Newtonsoft.Json;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Commands
{
    /// <summary>
    /// Serialization class for Selection and its derived types.
    /// To avoid type management in JSON string, all properties from all classes are defined
    /// in this surrogate class. The client is responsible for sending consistent sets of
    /// properties allowing the server to decide which concrete class to instantiate.
    /// </summary>
    public class JsonSelection
    {
        [JsonProperty("vm")]
        public NodeVisitMode VisitMode { get; set; }

        [JsonProperty("ch")]
        public JsonSelection[] SubSelections { get; set; }

        [JsonProperty("name")]
        public string Name { get; set; }

        [JsonProperty("idx")]
        public int? Index { get; set; }

        /// <summary>
        /// Instantiate actual Selection from transferred data.
        /// </summary>
        /// <returns>Non-null instance of Selection based on criteria set in this instance.</returns>
        /// <exception cref="InvalidDataException">When no valid criteria combination could be found to select the actual Selection type.</exception>
        internal Selection Convert()
        {
            Selection instance = null;

            if (Name != null)
            {
                // Prioritize selection by name
                instance = new SelectionByName(Name, VisitMode);
            }
            else if (Index.HasValue)
            {
                // Then by index
                instance = new SelectionByIndex(Index.Value, VisitMode);
            }
            
            if (instance == null)
            {
                // No name, no index -> Exception
                throw new InvalidDataException("Could not find actual Selection type, no selection criterion was provided.");
            }

            if (SubSelections != null)
            {
                // Recursively convert children
                var subSelections = SubSelections.Select(subSelection => subSelection.Convert());
                instance.SubSelections.AddRange(subSelections);
            }

            return instance;
        }
    }

    /// <summary>
    /// Visit mode for a Node
    /// </summary>
    public enum NodeVisitMode
    {
        /// <summary>
        /// Visit the node and process it.
        /// Continue visit according to the SubSelections.
        /// </summary>
        Default,

        /// <summary>
        /// Visit the node but do not process it.
        /// Let the SubSelections decide whether there are children nodes to be processed or not.
        /// </summary>
        PassThrough,

        /// <summary>
        /// Visit the node and process it, ignore the SubSelections.
        /// The visitor itself is solely responsible for the rest of the visit, independently of the SubSelections.
        /// </summary>
        Automatic
    }

    /// <summary>
    /// Abstract base class for a Node selection.
    /// </summary>
    internal abstract class Selection
    {
        /// <summary>
        /// Defines what to do on the selected Node when reached.
        /// </summary>
        public NodeVisitMode VisitMode { get; }

        /// <summary>
        /// Defines which nodes to select among the children of the current selected Node.
        /// </summary>
        public List<Selection> SubSelections { get; }

        protected Selection(NodeVisitMode visitMode)
        {
            VisitMode = visitMode;
            SubSelections = new List<Selection>();
        }

        /// <summary>
        /// Implements the selection mechanism defined for this instance
        /// among the children of the given node.
        /// </summary>
        /// <param name="parent">Parent node.</param>
        /// <returns>Node matching the selection criterion if any, null otherwise.</returns>
        public abstract Node SelectChild(Node parent);
    }

    internal class SelectionByName : Selection
    {
        /// <summary>
        /// Name of Node to select.
        /// </summary>
        public string Name { get; }

        public SelectionByName(string name, NodeVisitMode visitMode)
            : base(visitMode)
        {
            Name = name;
        }

        public override Node SelectChild(Node parent)
        {
            // Optimization: use SymbolTable and filter on parent instead of iterating Children and filter by name
            if (parent.SymbolTable.DataEntries.TryGetValue(Name, out var candidates))
            {
                return candidates.FirstOrDefault(candidate => candidate.Parent == parent);
            }

            return null;
        }
    }

    internal class SelectionByIndex : Selection
    {
        /// <summary>
        /// Index of Node to select.
        /// </summary>
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

    /// <summary>
    /// Singleton instance of Selection to represent the automatic mode.
    /// </summary>
    internal class AutoSelection : Selection
    {
        public static AutoSelection Instance = new();

        private AutoSelection()
            : base(NodeVisitMode.Automatic)
        {

        }

        public override Node SelectChild(Node parent)
        {
            // Should not be called as the selection has to be done by visitor itself in Automatic mode.
            throw new InvalidOperationException("AutoSelection has no selection mechanism proper.");
        }
    }

    /// <summary>
    /// Base class for AST visitor with a selection of nodes to visit.
    /// </summary>
    internal abstract class SelectedNodeVisitor
    {
        private readonly Selection _rootSelection;

        protected SelectedNodeVisitor(Selection rootSelection)
        {
            _rootSelection = rootSelection;
        }

        /// <summary>
        /// Starts the visit on the supplied root node, using the selection
        /// provided at construction time.
        /// </summary>
        /// <param name="root">Root node to start the visit on.</param>
        public void Visit(Node root)
        {
            // Validate initial selection
            bool startVisit = root.Parent == null || _rootSelection.SelectChild(root.Parent) == root;
            if (startVisit)
            {
                VisitNode(_rootSelection, root);
            }
        }

        private void VisitChildren(Selection selection, Node parentNode)
        {
            if (selection.VisitMode == NodeVisitMode.Automatic)
            {
                // We are in automatic mode: use the selection mechanism from this visitor
                foreach (var child in SelectChildren(parentNode))
                {
                    // Keep the automatic mode active
                    VisitNode(AutoSelection.Instance, child);
                }
            }
            else
            {
                // Use selection mechanism from sub-selections
                foreach (var subSelection in selection.SubSelections)
                {
                    var child = subSelection.SelectChild(parentNode);
                    if (child != null)
                    {
                        VisitNode(subSelection, child);
                    }
                    // else: inconsistent selection from client ? Ignore.
                }
            }
        }

        /// <summary>
        /// Custom selection mechanism for this visitor, used when in Automatic mode.
        /// </summary>
        /// <param name="parent">Parent node from which a set of children must be selected.</param>
        /// <returns>Non-null enumeration of selected children.</returns>
        protected abstract IEnumerable<Node> SelectChildren(Node parent);

        private void VisitNode(Selection selection, Node node)
        {
            EnterNode(node);
            if (selection.VisitMode != NodeVisitMode.PassThrough)
            {
                // Default or Automatic mode -> the node must be processed.
                bool nodeHasSelectedChildren = selection.SubSelections.Count > 0 || selection.VisitMode == NodeVisitMode.Automatic;
                ProcessNode(node, nodeHasSelectedChildren);
            }
            VisitChildren(selection, node);
            ExitNode(node);
        }

        /// <summary>
        /// Called when entering a new node during the visit.
        /// This method is called no matter the current visit mode.
        /// </summary>
        /// <param name="node">Newly entered node.</param>
        protected abstract void EnterNode(Node node);

        /// <summary>
        /// Called when a Node has been selected for processing.
        /// </summary>
        /// <param name="node">The node to process.</param>
        /// <param name="nodeHasSelectedChildren">Boolean indicating whether the node has selected children or not.</param>
        protected abstract void ProcessNode(Node node, bool nodeHasSelectedChildren);

        /// <summary>
        /// Called when exiting a Node.
        /// This method is called on every node previously entered, no matter the current visit mode.
        /// </summary>
        /// <param name="node">Exited node.</param>
        protected abstract void ExitNode(Node node);
    }
}
