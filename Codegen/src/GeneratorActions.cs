using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Actions;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// Class that Manages Generator Action.
    /// </summary>
    public class GeneratorActions : List<TypeCobol.Codegen.Actions.Action>, NodeVisitor
    {
        /// <summary>
        /// Skeletons patterns
        /// </summary>
        public List<Skeleton> Skeletons
        {
            get;
            private set;
        }

        /// <summary>
        /// Event Before executing an Action.
        /// </summary>
        public event EventHandler BeforeAction;
        /// <summary>
        /// Event After executing an action
        /// </summary>
        public event EventHandler AfterAction;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Skeletons">Skeletons pattern for actions</param>
        public GeneratorActions(List<Skeleton> skeletons)
        {
            Skeletons = skeletons ?? new List<Skeleton>();
        }

        public ICollection<TypeCobol.Codegen.Actions.Action> GetActions(Node node)
        {
            var actions = new List<TypeCobol.Codegen.Actions.Action>();
            var skeleton = GetActiveSkeleton(node);
            if (skeleton != null)
            {
                var properties = GetProperties(node, skeleton.Properties);
                foreach (var pattern in skeleton)
                {
                    var action = GetAction(node, properties, pattern);
                    if (action != null) 
                        actions.Add(action);
                }
            }
            return actions;
        }

        /// <summary>
        /// Visitor on a Node
        /// </summary>
        /// <param name="node"></param>
        public void Visit(Node node)
        {
            var actions = GetActions(node);
            AddRange(actions);
            foreach (var child in new List<Node>(node.Children)) 
                child.Accept(this);
        }

        /// <summary>
        /// Performs all actions
        /// <param name="tree">The tree on which to operate</param>
        /// </summary>
        public void Perform(Root tree)
        {
            tree.Accept(this);
            var groups = new List<string>();
            foreach (var action in this)
            {
                if (action.Group != null && groups.Contains(action.Group)) 
                    continue;
                if (BeforeAction != null)
                    BeforeAction(this, (EventArgs)action);
                action.Execute();
                if (AfterAction != null)
                    AfterAction(this, (EventArgs)action);
                if (action.Group != null) 
                    groups.Add(action.Group);
            }
        }

        public Skeleton GetActiveSkeleton(Node node)
        {
            foreach (var skeleton in Skeletons)
            {
                bool active = false;
                foreach (var condition in skeleton.Conditions)
                {
                    active = active || condition.Verify(node); // OR
                }
                if (active) 
                    return skeleton;//TODO: what if more than 1 skel activates?
            }
            return null;
        }

        public Dictionary<string, object> GetProperties(Node node, IEnumerable<string> properties)
        {
            var result = new Dictionary<string, object>();
            var errors = new System.Text.StringBuilder();
            foreach (var pname in properties)
            {
                var property = node[pname];
                if (property != null) result[pname] = property;
                else errors.Append(pname).Append(", ");
            }
            if (errors.Length > 0)
            {
                errors.Length -= 2;
                errors.Insert(0, "Undefined properties for " + node.GetType().Name + ": ");
                errors.Append(". (line:\"").Append(node.CodeElement.InputStream).Append("\")");
                throw new System.ArgumentException(errors.ToString());
            }
            return result;
        }

        public TypeCobol.Codegen.Actions.Action GetAction(Node source, Dictionary<string, object> properties, Pattern pattern)
        {
            int? index;
            var destination = GetLocation(source, pattern.Location, out index);
            if ("create".Equals(pattern.Action))
            {
                return new Create(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter, index);
            }
            if ("replace".Equals(pattern.Action))
            {
                return new Replace(destination, pattern.Template, properties, pattern.Group, pattern.Delimiter);
            }
            if ("comment".Equals(pattern.Action))
            {
                return new Comment(destination);
            }
            if ("expand".Equals(pattern.Action))
            {
                return new Expand(source, destination, pattern.Location);
            }
            if ("erase".Equals(pattern.Action))
            {
                return new Erase(destination, pattern.Template);
            }
            System.Console.WriteLine("Unknown action: \"" + pattern.Action + "\"");
            return null;
        }

        public Node GetLocation(Node node, string location, out int? index)
        {
            index = null;
            if (location.EndsWith(".begin"))
            {
                location = location.Substring(0, location.Length - ".begin".Length);
                index = 0;
            }
            else
                if (location.EndsWith(".end"))
                {
                    location = location.Substring(0, location.Length - ".end".Length);
                }

            if (location == null || location.ToLower().Equals("node")) return node;
            var root = node.Root;
            var result = root.Get(location);
            if (result != null) return result;
            result = Create(root, location);
            if (result != null) return result;
            throw new System.ArgumentException("Undefined URI: " + location);
        }
        public Node Create(Node node, string location)
        {
            var factory = new Codegen.Nodes.Factory();
            var parts = location.Split(new char[] { '.' });
            var path = new System.Text.StringBuilder();
            Node result = null;
            foreach (var part in parts)
            {
                path.Append(part);
                var current = node.Get(path.ToString());
                if (current == null)
                {
                    string nextsibling;
                    current = factory.Create(part, out nextsibling);
                    if (current == null) return null;
                    //All these nodes have been generated by a factory.
                    //Mark them so that the generator can associate them to the first
                    //parent having a location in the source file.
                    current.SetFlag(Node.Flag.FactoryGeneratedNode, true, true);
                    int index = 0;
                    if (nextsibling != null)
                    {
                        var sibling = result.Get(nextsibling);
                        index = sibling.Parent.IndexOf(sibling);
                    }
                    result.Add(current, index);
                }
                result = current;
                path.Append('.');
            }
            return result;
        }  
    }
}
