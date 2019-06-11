﻿using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Codegen.Actions;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
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
        /// CompilationDocument that contains all the code parsing results
        /// </summary>
        public CompilationDocument CompilationDocument { get; set; }
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

        public Generator Generator { get; private set; }

        public IActionsProvider ActionsProvider {get; private set;}

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Skeletons">Skeletons pattern for actions</param>
        public GeneratorActions(Generator generator, List<Skeleton> skeletons, CompilationDocument compilationDocument, IActionsProvider actionsProvider = null) {
            Generator = generator;
            Skeletons = skeletons ?? new List<Skeleton>();
            CompilationDocument = compilationDocument;
            ActionsProvider = actionsProvider;
        }


        /// <summary>
        /// Get all actions created for a node.
        /// </summary>
        /// <param name="node">The node to get all actions.</param>
        /// <returns>The collection of actiosn.</returns>
        public ICollection<TypeCobol.Codegen.Actions.Action> GetActions(Node node)
        {
            if (ActionsProvider != null)
            {
                return ActionsProvider.GetActions(node, this);
            }
            else
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
            if (actions != null)
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
        public void Perform(Node tree)
        {
            if (tree == null)
                return;
            tree.Accept(this);
            var groups = new List<string>();
            for(int i = 0; i < this.Count; i++)
            {
                var action = this[i];
                try
                {
                    if (action.Group != null && groups.Contains(action.Group)) 
                    continue;
                    if (BeforeAction != null)
                        BeforeAction(this, (EventArgs)action);
                    var subActions = action.Execute();
                    if (subActions != null)
                    {
                        this.AddRange(subActions);
                    }
                    if (AfterAction != null)
                        AfterAction(this, (EventArgs)action);
                    if (action.Group != null)
                        groups.Add(action.Group);
                } catch (Exception e) {
                    Diagnostic diag = new Diagnostic(MessageCode.ImplementationError, 0, 0, 0, e.Message + "\n" + e.StackTrace);
                    Generator.AddDiagnostic(diag);
                }
            }
        }

        /// <summary>
        /// Give the first skeleton that match a node.
        /// </summary>
        /// <param name="node">The node to get the active skeleton</param>
        /// <returns>The first matching skeleton if any, null otherwise.</returns>
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

        /// <summary>
        /// Compute all dynamic values corresponding to a set of properties.
        /// </summary>
        /// <param name="node">The node to get properties values</param>
        /// <param name="properties">All properties value</param>
        /// <returns>A dictionary of properties values Dictionary<property:string, value:object></returns>
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

        /// <summary>
        /// Create Codegen Action
        /// </summary>
        /// <param name="souce">The source node</param>
        /// <param name="pattern">The pattern's name</param>
        /// <param name="code">The code of the action</param>
        /// <param name="action">The actions' name</param>
        /// <param name="group">The action's group</param>
        /// <param name="location">The node's location</param>
        /// <param name="position">The node's insertion position</param>
        /// <param name="newline">Shall we first emit a newline</param>
        /// <returns>The action if any, null otherwise</returns>
        public TypeCobol.Codegen.Actions.Action CreateAction(Node source, string pattern, string code, string action, string group, string location, string position, bool newline)
        {
            if (code != null)
            {
                code = code.TrimStart().Replace("\r\n", "\n");
            }
            int? index;
            if (group != null && ProgramGroupPrefix != null)
            {
                group = ProgramGroupPrefix + group;
            }
            var destination = GetLocation(source, location, out index);
            if ("create".Equals(action))
            {
                return new CreateCode(destination, pattern, code, group, index, newline);
            }
            if ("replace".Equals(action))
            {
                return new ReplaceCode(destination, pattern, code, group);
            }
            if ("comment".Equals(action))
            {
                return new Comment(destination);
            }
            if ("expand".Equals(action))
            {
                return new Expand(source, destination, location);
            }
            if ("erase".Equals(action))
            {
                return new Erase(destination, code);
            }
            if ("remarks".Equals(action))
            {
                return new Remarks(source, destination, location, CompilationDocument);
            }
            if ("clone".Equals(action))
            {
                return new Clone(destination, this);
            }
                System.Console.WriteLine("Unknown action: \"" + action + "\"");
            return null;

        }

        /// <summary>
        /// Create the action associated to a node using a pattern.
        /// </summary>
        /// <param name="source">The source node</param>
        /// <param name="properties">The dictonary of property values</param>
        /// <param name="pattern">The action pattern</param>
        /// <returns>The create action if any, null otherwise</returns>
        public TypeCobol.Codegen.Actions.Action GetAction(Node source, Dictionary<string, object> properties, Pattern pattern)
        {
            //Ignore any deprecated pattern.
            if (pattern.IsDeprecated)
                return null;
            
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
                return new Create(destination, pattern, properties, group, index);
            }
            if ("replace".Equals(pattern.Action))
            {
                return new Replace(destination, pattern, properties, group);
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
            if ("remarks".Equals(pattern.Action))
            {
                return new Remarks(source, destination, pattern.Location, CompilationDocument);
            }
            System.Console.WriteLine("Unknown action: \"" + pattern.Action + "\"");
            return null;
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
            Node root = CurrentProgram ?? node.GetProgramNode();
            root = root ?? node.Root;
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
        public Node GetSingleLocation(Node node, string location, out int? index, string[] subPaths)
        {
            index = null;
            location = NormalizeLocation(node, subPaths != null ? subPaths[0] : location, out index);
            if (location == null || location.ToLower().Equals("node"))
            {
                return node;
            }
            Node root = (CurrentProgram ?? node.GetProgramNode());
            root = root ?? node.Root;
            var result = root.Get(subPaths != null ? subPaths[0] : location);
            if (result != null)
            {                
                if (subPaths != null)
                {//Check if a subpath matches                                        
                    for (int i = 1; i < subPaths.Length; i++)
                    {
                        int nbParent = subPaths[i].LastIndexOf('/') + 1;
                        Node parent = result;
                        int matchIndex = 0;
                        for (int j = 0; j < nbParent; j++)
                        {
                            matchIndex = parent.Parent.IndexOf(parent);
                            parent = parent.Parent;
                        }
                        int? index2 = null;
                        var subResult = GetSingleSubLocation(parent, subPaths[i].Substring(nbParent), out index2, matchIndex + 1);
                        if (subResult != null)
                        {
                            index = index2;
                            if (subResult.CodeElement == null)
                            {
                                if (subResult.Parent == parent)
                                {                                    
                                    index = null;
                                }
                                else
                                {
                                    result = subResult;
                                }
                            }
                            else
                            {
                                result = subResult;
                            }
                            break;
                        }
                    }
                }
                return result;
            }
            result = Create(root, subPaths != null ? subPaths[0] : location);
            if (result != null)
            {
                return result;
            }
            throw new System.ArgumentException("Undefined URI: " + location);
        }

        /// <summary>
        /// Get a Sub Location node
        /// </summary>
        /// <param name="parent">The parent of the the sub location</param>
        /// <param name="location">The sub location</param>
        /// <param name="index">Output Index of the location in the parent node</param>
        /// <param name="subIndex">Index of the starting sublocation in the parent</param>
        /// <returns></returns>
        public Node GetSingleSubLocation(Node parent, string location, out int? index, int subIndex)
        {            
            index = null;
            location = NormalizeLocation(parent, location, out index);
            if (location == null || location.ToLower().Equals("node"))
            {
                return parent;
            }
            var result = parent.Get(location, subIndex);
            return result;
        }

        /// <summary>
        /// Get the node corresponding to a set of locations.
        /// </summary>
        /// <param name="parent">The parent node</param>
        /// <param name="location">The location access path, this can be a set of access path using the character '|' as separator.
        /// The lookup semantic is as follow: each access path is tested for existence, if it exists then a corresponding node is searched for it and returned.
        /// Finally the last access path will be used to locate the node, if none of the previous path exists.
        /// </param>
        /// <param name="index">[output] the position index of the resulting node within its parent children.</param>
        /// <returns>The </returns>
        public Node GetLocation(Node parent, string location, out int? index)
        {
            string[] locations = location.Split(new char[] { '|' });
            string[] paths = null;
            if (locations.Length > 1)
            {
                for (int i = 0; i < locations.Length; i++)
                {
                    locations[i] = locations[i].Trim();
                    string mainLocation = locations[i];
                    paths = null;              
                    if (mainLocation.IndexOf(',') > 0)
                    {
                        paths = mainLocation.Split(new char[] { ',' });
                        mainLocation = paths[0];
                    }
                    if (IsLocationExists(parent, mainLocation, out index))
                    {
                        return GetSingleLocation(parent, locations[i], out index, paths);
                    }
                }
            }
            paths = null;
            if (locations[locations.Length - 1].IndexOf(',') > 0)
            {
                paths = locations[locations.Length - 1].Split(new char[] { ',' });                
            }
            return GetSingleLocation(parent, locations[locations.Length - 1], out index, paths);
        }

        /// <summary>
        /// Create a node at the given location.
        /// </summary>
        /// <param name="parent">The parent root node</param>
        /// <param name="location">The location as access path.</param>
        /// <returns>The create node</returns>
        public Node Create(Node parent, string location)
        {
            var factory = new Codegen.Nodes.Factory();
            var parts = location.Split(new char[] { '.' });
            var path = new System.Text.StringBuilder();
            Node result = null;
            foreach (var part in parts)
            {
                path.Append(part);
                var current = parent.Get(path.ToString());
                if (current == null)
                {
                    string nextsibling;
                    List<string> previoussibling;
                    current = factory.Create(part, out nextsibling, out previoussibling);
                    if (current == null) return null;
                    //All these nodes have been generated by a factory.
                    //Mark them so that the generator can associate them to the first
                    //parent having a location in the source file.
                    current.SetFlag(Node.Flag.FactoryGeneratedNode, true, true);
                    //Keep The insertion sequence ??
                    current.SetFlag(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex, true, false);
                    int index = -1;
                    if (nextsibling != null)
                    {
                        var sibling = result?.Get(nextsibling);
                        if (sibling != null)
                            index = sibling.Parent.IndexOf(sibling);
                    }
                    if (index >= 0)
                        result?.Add(current, index);
                    else
                    {
                        index = 0;
                        //Look for a next
                        if (previoussibling != null)
                        {
                            foreach (string p in previoussibling)
                            {
                                var previous = result?.Get(p);
                                if (previous != null)
                                {
                                    int pindex = previous.Parent.IndexOf(previous);
                                    if (pindex >= 0)
                                    {
                                        index = pindex + 1;
                                        break;
                                    }
                                }
                            }
                        }
                        result?.Add(current, index);
                    }
                }
                result = current;
                path.Append('.');
            }
            return result;
        }  
    }
}
