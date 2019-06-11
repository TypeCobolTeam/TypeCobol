﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Analytics;
using TypeCobol.Codegen.Actions;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;
using TypeCobol.CustomExceptions;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;
using System.Text;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// The Second Code Generator which can Handle correctly all preprocessor directives
    /// during the Code Generation Phase.
    /// </summary>
    public abstract class Generator : IGenerator, NodeVisitor
    {
        /// <summary>
        /// The Compilation Document.
        /// </summary>
        public TypeCobol.Compiler.CompilationDocument CompilationResults
        {
            get;
            private set;
        }

        /// <summary>
        /// The Destination.
        /// </summary>
        public StringBuilder Destination
        {
            get;
            private set;
        }

        /// <summary>
        /// The Target Generated Document
        /// </summary>
        public TypeCobol.Compiler.Source.SourceDocument TargetDocument
        {
            get;
            private set;
        }

        /// <summary>
        /// Generator Actions
        /// </summary>
        public GeneratorActions Actions
        {
            get;
            private set;
        }

        /// <summary>
        /// The Generator Current SourceFile Node
        /// </summary>
        public SourceFile RootNode
        {
            get;
            private set;
        }

        /// <summary>
        /// Tghe Generator current symbol table
        /// </summary>
        public SymbolTable SymTable
        {
            get;
            private set;
        }

        /// <summary>
        /// The Generator current Columns Layout
        /// </summary>
        public ColumnsLayout Layout
        {
            get;
            private set;
        }

        /// <summary>
        /// Lines of Erased Nodes.
        /// </summary>
        public List<Node> ErasedNodes
        {
            get;
            private set;
        }

        /// <summary>
        /// Lines of Cloned Nodes.
        /// </summary>
        public List<Node> ClonedNodes
        {
            get;
            private set;
        }

        public List<Diagnostic> Diagnostics
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public Generator(TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, List<Skeleton> skeletons, string typeCobolVersion)
        {
            this.CompilationResults = document;
            this.TypeCobolVersion = typeCobolVersion;
            Destination = destination;

            //Add version to output file
            if (!string.IsNullOrEmpty(TypeCobolVersion))
                Destination.AppendLine("      *TypeCobol_Version:" + TypeCobolVersion);

            Actions = new GeneratorActions(this, skeletons, document, skeletons != null ? null : new TypeCobol.Codegen.Actions.Skeletons());
            //To Store Erased Nodes by the Erase Action.
            ErasedNodes = new List<Node>();
            //To Store Cloned Nodes by the Clone Action.
            ClonedNodes = new List<Node>();
            //The After Action Listener
            Actions.AfterAction += OnAfterAction;
        }

        /// <summary>
        /// Get the From and To Positions of this Node based on the consumed Token, if no ConsumedTokens the return value is NULL.
        /// In the consumed tokens span over several lines then the size of the newline sequence is included for each line.
        /// The method also calculate the ending span offset from the beginning of the last line.
        /// It also get the list of Line numbers occupated by this node, and the offset of each line.
        /// </summary>
        public Tuple<int, int, int, List<int>, List<int>> FromToPositions(Node node)
        {
            if (node.CodeElement == null || node.CodeElement.ConsumedTokens == null || node is ParameterEntry)
                return null;
            if (node.CodeElement.ConsumedTokens.Count > 0)
            {
                int ln_size = System.Environment.NewLine.Length;
                int from = -1;
                int to = 0;
                int i = 0;
                int span = 0;
                List<int> lineNumbers = new List<int>();
                List<int> lineOffsets = new List<int>();
                SourceDocument.SourceLine srcFirstLine = null;
                do
                {
                    var token = node.CodeElement.ConsumedTokens[i];
                    if (!(token is TypeCobol.Compiler.Preprocessor.ImportedToken))
                    {//Don't take in account imported tokens -> This avoid including lines that come from COPYs files.
                        int curLineIndex = node.CodeElement.ConsumedTokens[i].Line;
                        if (curLineIndex == 0)
                        {   //Very bizarre ??? It happens with some COBOL85 samples like:
                            //CCC1B045.PGM, CCTF0011.PGM, CCTZ015B, CCTZ0300B, etc..
                            return null;
                        }
                        if (lineNumbers.Count > 0)
                        {//Add lines between
                            int lastLine = lineNumbers[lineNumbers.Count - 1];
                            while (++lastLine < curLineIndex)
                            {
                                lineNumbers.Add(lastLine);
                                SourceDocument.SourceLine srcLine = TargetDocument[lastLine - 1];
                                if (srcFirstLine != null) lineOffsets.Add(srcLine.From - srcFirstLine.From);
                            }
                        }
                        SourceDocument.SourceLine curLine = TargetDocument[curLineIndex - 1];
                        if (srcFirstLine == null)
                            srcFirstLine = curLine;
                        lineNumbers.Add(curLineIndex);
                        lineOffsets.Add(curLine.From - srcFirstLine.From);
                        span = 0;
                        while ((i < node.CodeElement.ConsumedTokens.Count) && ((curLineIndex == node.CodeElement.ConsumedTokens[i].Line)
                            || (node.CodeElement.ConsumedTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken)))
                        {
                            if (!(node.CodeElement.ConsumedTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken))
                            {
                                if (from == -1)
                                    from = node.CodeElement.ConsumedTokens[i].Column;
                                span = node.CodeElement.ConsumedTokens[i].EndColumn;
                            }
                            i++;
                        }
                        to = (curLine.From + span) - srcFirstLine.From;
                    }
                    else
                    {
                        i++;
                    }
                } while (i < node.CodeElement.ConsumedTokens.Count);
                lineNumbers.TrimExcess();
                lineOffsets.TrimExcess();
                return new Tuple<int, int, int, List<int>, List<int>>(from, to, span, lineNumbers, lineOffsets);
            }
            return null;
        }

        /// <summary>
        /// Handler called after an Action has beeen executed.
        /// </summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The action in fact</param>
        protected virtual void OnAfterAction(object sender, EventArgs e)
        {
            Codegen.Actions.Action action = (Codegen.Actions.Action)e;
             //Collect erased nodes.
            if (action is TypeCobol.Codegen.Actions.IEraseAction)
            {
                TypeCobol.Codegen.Actions.IEraseAction erase = (TypeCobol.Codegen.Actions.IEraseAction)action;
                IList<Node> nodes = erase.ErasedNodes;
                if (nodes != null)
                {
                    foreach (Node node in nodes)
                    {
                        node.SetFlag(Node.Flag.GeneratorErasedNode, true);
                        ErasedNodes.Add(node);
                    }                    
                }
            }
            //Collect cloned nodes.
            if (action is TypeCobol.Codegen.Actions.ICloneAction)
            {
                TypeCobol.Codegen.Actions.ICloneAction clone = (TypeCobol.Codegen.Actions.ICloneAction)action;
                IList<Node> nodes = clone.ClonedNodes;
                if (nodes != null)
                {
                    foreach (Node node in nodes)
                    {                        
                        ClonedNodes.Add(node);
                    }
                }
            }
        }

        /// <summary>
        /// Generate Code
        /// </summary>
        /// <param name="compilationUnit"> Compilation Unit resulting from TypeCobol Parsing</param>
        /// <param name="columns">Columns layout</param>
        public virtual void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            //Check if there is any error in diags
            if (compilationUnit.AllDiagnostics().Any(d => d.Info.Severity == Compiler.Diagnostics.Severity.Error))
            {
                AnalyticsWrapper.Telemetry.TrackEvent(EventType.Generation, "Diagnostics Detected", LogType.Genration);
                throw new GenerationException("Unable to generate because of error diagnostics", compilationUnit.TextSourceInfo.Name, null, false, false);
            }

            // STEP 0: Initialize the global values.
            RootNode = compilationUnit.ProgramClassDocumentSnapshot.Root;
            SymTable = compilationUnit.ProgramClassDocumentSnapshot.Root.SymbolTable;
            Layout = columns;
            //Create the Initial target document.
            CreateTargetDocument(false);
            // STEP 1: modify tree to adapt it to destination language            
            // 1.1 Run the Qualifier action on this node
            Qualifier qualifier = new Qualifier(this, RootNode);
            qualifier.Execute();
            // 1.2 Perform other actions
            Actions.Perform(RootNode);
            // STEP 2: convert tree to destination language code
            TreeToCode();
        }

        /// <summary>
        /// Create the Target Document.        
        /// </summary>
        /// <param name="bTrackFirtNonCblDirectiveLine">True if the First non Cobol Directive line must be tracked and return returned</param>
        /// <returns>if bTrackFirtNonCblDirectiveLine is set to true this method return the first non cbl directive line it a 0 based line number.</returns>
        protected virtual int CreateTargetDocument(bool bTrackFirtNonCblDirectiveLine)
        {
            int iNonDirectiveLine = -1;
            TargetDocument = new Compiler.Source.SourceDocument(/*new StringSourceText()*/);
            //Insert all input lines
            StringWriter sw = new StringWriter();
            int i = 0; //Line count
            foreach (TypeCobol.Compiler.Scanner.ITokensLine line in this.CompilationResults.TokensLines)
            {
                if (bTrackFirtNonCblDirectiveLine && iNonDirectiveLine < 0)
                {
                    TypeCobol.Compiler.Parser.CodeElementsLine cel = (TypeCobol.Compiler.Parser.CodeElementsLine) line;
                    if (!(cel.TokensWithCompilerDirectives.Count == 1 &&
                        cel.TokensWithCompilerDirectives[0].TokenFamily == TokenFamily.CompilerDirective))
                    {
                        iNonDirectiveLine = i;
                    }
                    i++;
                }
                sw.WriteLine(line.Text);
            }
            //Load the Original source code
            TargetDocument.LoadSourceText(sw.ToString());
            //TargetDocument.Dump();
            return iNonDirectiveLine;
        }

        /// <summary>
        /// Perform Tree to Code generation
        /// </summary>
        protected virtual void TreeToCode()
        {
            if (RootNode != null)
                RootNode.Accept(this);
        }
    
        /// <summary>
        /// Visit Method over tree node visitation
        /// </summary>
        /// <param name="node">The Node to visit</param>
        public void Visit(Node node)
        {
            bool doVisitChildren = Process(node);
            if (doVisitChildren)
            {
                foreach (var child in node.Children)
                    child.Accept(this);
            }
        }

        /// <summary>
        /// Add an error diagnostic.
        /// </summary>
        /// <param name="diag"></param>
        public void AddDiagnostic(Diagnostic diag) 
        {
            if(Diagnostics == null)
                Diagnostics = new List<Diagnostic> ();
            Diagnostics.Add(diag);
        }

        /// <summary>
        /// Abstract method to Process a node
        /// </summary>
        /// <param name="node">The node to process</param>
        /// <returns>true if child nodes must visited for acceptation, false otherwise.</returns>
        protected abstract bool Process(Node node);
        public abstract void GenerateLineMapFile(Stream stream);

        public string TypeCobolVersion { get; set; }
        public abstract bool HasLineMapData { get; }
    }
}
