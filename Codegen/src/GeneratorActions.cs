using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Actions;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
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
        /// The Group Prefix of the current Program node.
        /// </summary>
        private string ProgramGroupPrefix = null;
        /// <summary>
        /// The Current Program Node.
        /// </summary>
        private Program CurrentProgram = null;

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
            string saveProgramGroupPrefix = ProgramGroupPrefix;
            Program saveCurrentProgram = CurrentProgram;
            if (node is Program)
            {
                CurrentProgram = node as Program;
                ProgramGroupPrefix = CurrentProgram.Identification.Text;
            }

            var actions = GetActions(node);
            AddRange(actions);
            foreach (var child in new List<Node>(node.Children)) 
                child.Accept(this);

            if (node is Program)
            {
                ProgramGroupPrefix = saveProgramGroupPrefix;
                CurrentProgram = saveCurrentProgram;
            }
        }

        /// <summary>
        /// Performs all actions
        /// <param name="tree">The tree on which to operate</param>
        /// </summary>
        public void Perform(Root tree)
        {
            if (tree == null)
                return;
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
            //Evaluate Any Proprty
            if (!pattern.EvalBooleanProperty(properties))
            {
                return null;
            }
            int? index;
            string group = pattern.Group;
            if (group != null && ProgramGroupPrefix != null)
            {                
                group = ProgramGroupPrefix + group;
            }
            var destination = GetLocation(source, pattern.Location, out index);
            if ("create".Equals(pattern.Action))
            {
                return new Create(destination, pattern, properties, group, pattern.Delimiter, index);
            }
            if ("replace".Equals(pattern.Action))
            {
                return new Replace(destination, pattern.Template, properties, group, pattern.Delimiter);
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

        /// <summary>
        /// Get the Program Node corresponding to a Child
        /// </summary>
        /// <param name="child">The Child Node</param>
        /// <returns>The Program Node</returns>
        public static Program GetProgramNode(Node child)
        {
            if (child == null)
                return null;
            while (child != null && !(child is Program))
                child = child.Parent;
            return (Program)child;
        }

        /// <summary>
        /// Normalize of location
        /// </summary>
        /// <param name="node">The parent node of the location</param>
        /// <param name="location">The location to normalize</param>
        /// <param name="index">The index of the location if any</param>
        /// <returns>The normalized location string</returns>
        private static string NormalizeLocation(Node node, string location, out int? index)
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
            return location;
        }

        /// <summary>
        /// Determines if the given location exists
        /// </summary>
        /// <param name="node">The parent node of the location</param>
        /// <param name="location">The loctaion to test.</param>
        /// <returns>true if the location exists, fals eotherwise</returns>
        public bool IsLocationExists(Node node, string location, out int? index)
        {
            index = null;
            location = NormalizeLocation(node, location, out index);
            if (location == null || location.ToLower().Equals("node")) 
                return true;
            var root = CurrentProgram ?? (node.GetProgramNode() ?? node.Root);
            var result = root.Get(location);
            return result != null;
        }

        /// <summary>
        /// Get a Single Location Node
        /// </summary>
        /// <param name="node">The parent node of the location</param>
        /// <param name="location">The location to get</param>
        /// <param name="index">Output Index of the location in the parent node</param>
        /// <returns>The location's node</returns>
        public Node GetSingleLocation(Node node, string location, out int? index)
        {
            index = null;
            location = NormalizeLocation(node, location, out index);
            if (location == null || location.ToLower().Equals("node")) return node;
            var root = CurrentProgram ?? (node.GetProgramNode() ?? node.Root);
            var result = root.Get(location);
            if (result != null) return result;
            result = Create(root, location);
            if (result != null) return result;
            throw new System.ArgumentException("Undefined URI: " + location);
        }

        public Node GetLocation(Node node, string location, out int? index)
        {
            string[] locations = location.Split(new char[] { '|' });
            for(int i = 0; i < locations.Length; i++)
            {
                if (IsLocationExists(node, locations[i], out index))
                {
                    return GetSingleLocation(node, locations[i], out index);
                }
            }
            return GetSingleLocation(node, locations[locations.Length - 1], out index);
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
                    //Keep The insertion sequence ??
                    current.SetFlag(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex, true, false);
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
