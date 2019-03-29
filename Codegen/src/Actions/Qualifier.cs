using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// The Qualifier Action. This action is used to detect if a node is suject
    /// to Type Cobol Qualifier Style.
    /// </summary>
    public class Qualifier : EventArgs, Action
    {
        /// <summary>
        /// Internal visitor class.
        /// </summary>
        internal class TypeCobolCobolQualifierVistor : TypeCobol.Compiler.CodeElements.AbstractAstVisitor
        {
            /// <summary>
            /// The Generator Instance
            /// </summary>
            internal Generator Generator;
            /// <summary>
            /// The Current Node
            /// </summary>
            public Node CurrentNode;
            /// <summary>
            /// Qualifier items
            /// </summary>
            public IList<SymbolReference> Items;
            public IList<IList<SymbolReference>> AllItemsList;
            /// <summary>
            /// The Stack of Programs encountered
            /// </summary>
            public Stack<IProcCaller> ProcCallerStack;
            
            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="generator">The Generator instance</param>
            internal TypeCobolCobolQualifierVistor(Generator generator)
            {
                this.Generator = generator;
                this.AllItemsList = new List<IList<SymbolReference>>();
            }

            /// <summary>
            /// Avoid visiting Symbol Information Tokens
            /// </summary>
            public override bool IsSymbolInformationForTokensEnabled
            {
                get
                {
                    return false;
                }
            }

            /// <summary>
            /// CodeGen visitor can modify Node's children
            /// </summary>
            public override bool CanModifyChildrenNode => true;

            /// <summary>
            /// Visitor
            /// </summary>
            /// <param name="typeCobolQualifiedSymbolReference"></param>
            /// <returns></returns>
            public override bool Visit(TypeCobol.Compiler.CodeElements.TypeCobolQualifiedSymbolReference typeCobolQualifiedSymbolReference)
            {   //Yes it has TypeCobol qualifier
                IList<SymbolReference> items = typeCobolQualifiedSymbolReference.AsList();
                if (Items == null)
                {
                    Items = items;
                }
                else
                {
                    SymbolReference first = items[0];
                    SymbolReference last = items[items.Count - 1];
                    if (AllItemsListContains(first))
                        return true;
                    if (ItemsContains(first) || ItemsContains(last))
                    {
                        Items = items;
                    }
                    else
                    {
                        AllItemsList.Add(Items);
                        Items = items;
                    }
                }
                return true;
            }

            /// <summary>
            /// Detecting call site parameter
            /// </summary>
            /// <param name="callSiteParameter"></param>
            /// <returns></returns>
            public override bool Visit(CallSiteParameter callSiteParameter)
            {
                return false;
            }

            /// <summary>
            /// Set of already used storage area.
            /// </summary>
            HashSet<TypeCobol.Compiler.CodeElements.StorageArea> UsedStorageArea;
            public override bool Visit(StorageArea storageArea)
            {
                if (this.CurrentNode.IsFlagSet(Node.Flag.NodeContainsIndex) || this.CurrentNode.IsFlagSet(Node.Flag.NodeContainsBoolean))
                {
                    Tuple<int, int, int, List<int>, List<int>> sourcePositions = this.Generator.FromToPositions(this.CurrentNode);
                    foreach (TypeCobol.Compiler.CodeElements.StorageArea storage_area in this.CurrentNode.QualifiedStorageAreas.Keys)
                    {
                        if (this.CurrentNode.IsFlagSet(Node.Flag.NodeContainsIndex))
                        {
                            QualifiedStorageAreaSelecterForIndexes(storage_area, sourcePositions);
                        }
                        if (this.CurrentNode.IsFlagSet(Node.Flag.NodeContainsBoolean))
                        {
                            QualifiedStorageAreaSelecterForBoolean(storage_area, sourcePositions);
                        }
                       
                    }
                    this.CurrentNode.Comment = true;
                }
                return true;
            }

            private void QualifiedStorageAreaSelecterForBoolean(StorageArea storage_area, Tuple<int, int, int, List<int>, List<int>> sourcePositions)
            {
                if (UsedStorageArea != null && UsedStorageArea.Contains(storage_area))
                    return;
                string name = storage_area?.GetStorageAreaThatNeedDeclaration?.SymbolReference?.Name;
                GenerateToken item = null;
                item = new GenerateToken(
                    new TokenCodeElement(storage_area?.GetStorageAreaThatNeedDeclaration?.SymbolReference?.NameLiteral?.Token), name + "-value", sourcePositions);
                item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                this.CurrentNode.Add(item);
                if (UsedStorageArea == null)
                {
                    UsedStorageArea = new HashSet<StorageArea>();
                }
                UsedStorageArea.Add(storage_area);
            }

            private void QualifiedStorageAreaSelecterForIndexes(StorageArea storageArea, Tuple<int, int, int, List<int>, List<int>> sourcePositions)
            {
                if (storageArea.SymbolReference != null && !storageArea.SymbolReference.IsQualifiedReference)
                {
                    if (UsedStorageArea != null && UsedStorageArea.Contains(storageArea))
                        return;


                    string qualified_name = this.CurrentNode.GetQualifiedName(storageArea);
                    
                    GenerateToken item = null;
                    string hashName = GeneratorHelper.ComputeIndexHashName(qualified_name, this.CurrentNode);
                    item = new GenerateToken(
                        new TokenCodeElement(storageArea.SymbolReference.NameLiteral.Token), hashName, sourcePositions);
                    item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                    this.CurrentNode.Add(item);
                    if (UsedStorageArea == null)
                    {
                        UsedStorageArea = new HashSet<StorageArea>();
                    }
                    UsedStorageArea.Add(storageArea);
                }
            }

            /// <summary>
            /// Generate TypeCobol Qualified Symbol Reference node for a given StorageArea.
            /// </summary>
            /// <param name="storageArea">The Storage Area to generate the node</param>
            /// <param name="codeElement">The underlying code element</param>
            /// <returns>true if some nodes have been generated, false otherwise</returns>
            private bool GenQualifiedStorage(StorageArea storageArea, CodeElement codeElement)
            {
                if (storageArea?.GetStorageAreaThatNeedDeclaration?.SymbolReference != null && !storageArea.GetStorageAreaThatNeedDeclaration.SymbolReference.IsTypeCobolQualifiedReference)
                    return false;
                if (CurrentNode == null)
                    return false;
                TypeCobolQualifiedSymbolReference tcqsr = storageArea.GetStorageAreaThatNeedDeclaration.SymbolReference as TypeCobolQualifiedSymbolReference;
                int start = -1;
                for (int i = 0; i < codeElement.ConsumedTokens.Count; i++)
                {
                    if (codeElement.ConsumedTokens[i] == tcqsr?.Head.NameLiteral.Token)
                    {
                        start = i;
                        break;
                    }
                }
                int end = -1;
                for (int i = 0; i < codeElement.ConsumedTokens.Count; i++)
                {
                    if (codeElement.ConsumedTokens[i] == tcqsr?.Tail.NameLiteral.Token)
                    {
                        end = i;
                        break;
                    }
                }
                List<GenerateToken> items = new List<GenerateToken>();
                List<string> names = new List<string>();
                for (int i = end; i <= start; i++)
                {
                    if (codeElement.ConsumedTokens[i].TokenType == Compiler.Scanner.TokenType.QualifiedNameSeparator)
                    {
                        GenerateToken qns = new GenerateToken(new TokenCodeElement(codeElement.ConsumedTokens[i]), string.Intern(" OF "),
                            null);
                        qns.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                        CurrentNode.Add(qns);
                    }
                    else
                    {
                        GenerateToken item = new GenerateToken(
                            new TokenCodeElement(codeElement.ConsumedTokens[i]), codeElement.ConsumedTokens[i].Text,
                            null);
                        item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                        CurrentNode.Add(item);
                        items.Add(item);
                        names.Add(codeElement.ConsumedTokens[i].Text);
                    }
                }
                names.Reverse();
                for (int i = 0; i < names.Count; i++)
                {
                    items[i].ReplaceCode = names[i];
                }
                return items.Count > 0;
            }


            /// <summary>
            /// The Goal of this override is to generate hash names for pure Cobol85 Indices used as Qualified Names.
            /// </summary>
            /// <param name="indexDefinition">The Index Definition instance</param>
            /// <returns>true do to keep on visiting, false otherwise.</returns>
            public override bool Visit(IndexDefinition indexDefinition)
            {
                if (indexDefinition.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                    return true;
                if (!indexDefinition.IsPartOfATypeDef && indexDefinition.Parent != null && indexDefinition.IsFlagSet(Node.Flag.IndexUsedWithQualifiedName))
                {//Check if the parent is a data definition with a type which is not a TypeDef

                    var dataDef = indexDefinition.Parent as DataDefinition;
                    if (dataDef != null) {
                        var dde = dataDef.CodeElement as DataDescriptionEntry;
                        if (dde != null)
                        {
                            if (dde.Indexes != null)
                            {
                                foreach (var index in dde.Indexes)
                                {
                                    if (index.Name.Equals(indexDefinition.Name))
                                    {
                                        Tuple<int, int, int, List<int>, List<int>> sourcePositions = this.Generator.FromToPositions(indexDefinition.Parent);
                                        string qualified_name = indexDefinition.QualifiedName.ToString();
                                        GenerateToken item = null;
                                        string hashName = GeneratorHelper.ComputeIndexHashName(qualified_name, indexDefinition.Parent);
                                        item = new GenerateToken(
                                            new TokenCodeElement(index.NameLiteral.Token), hashName,
                                            sourcePositions);
                                        item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                                        indexDefinition.Parent.Add(item);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                indexDefinition.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                return true;
            }
            /// <summary>
            /// Checks if the current symbol reference is contained in the aready collected Items list.
            /// </summary>
            /// <param name="sr">The Symbol Reference to Check</param>
            /// <returns>True if it is aready in, false otherwise</returns>
            private bool ItemsContains(SymbolReference sr)
            {
                if (Items == null)
                    return false;
                foreach (SymbolReference r in Items)
                    if (sr == r)
                        return true;
                return false;
            }

            /// <summary>
            /// Checks if the current symbol reference is contained in the aready collected Items list.
            /// </summary>
            /// <param name="sr">The Symbol Reference to Check</param>
            /// <returns>True if it is aready in, false otherwise</returns>
            private bool AllItemsListContains(SymbolReference sr)
            {
                if (AllItemsList == null)
                    return false;
                foreach (var l in AllItemsList)
                {
                    foreach (SymbolReference r in l)
                        if (sr == r)
                            return true;
                }
                return false;
            }

            public override bool BeginNode(Node node)
            {
                PerformMatch();
                this.CurrentNode = node;

                var procCaller = node as IProcCaller;
                if (procCaller != null) {
                    if (this.ProcCallerStack == null)
                        this.ProcCallerStack = new Stack<IProcCaller>();
                    this.ProcCallerStack.Push(procCaller);
                    procCaller.ProcStyleCalls =
                        new Dictionary<string, Tuple<IList<SymbolReference>, Compiler.Nodes.ProcedureStyleCall>>();
                }
                
                return base.BeginNode(node);
            }

            public override void EndNode(Node node)
            {
                base.EndNode(node);
                PerformMatch();
                node.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                if (node is IProcCaller )
                {
                    this.ProcCallerStack.Pop();
                }
            }

            /// <summary>
            /// If the Current Node uses Reads and Writes Storage Areas then if symbol references are
            /// TypeCobol Symbol References, a substitution node is generated for them.
            /// </summary>
            private void GenQualificationForNodeWithReadsWritesStorage()
            {
                if (!HasMatch && CurrentNode != null && CurrentNode.CodeElement != null)
                {//Check if this node has Reads/Writes Storages
                    if (CurrentNode.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                        return;

                    //Don't transform Procedure Call
                    if (CurrentNode is TypeCobol.Compiler.Nodes.ProcedureStyleCall)
                        return;

                    CurrentNode.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                    bool bHasGeNodes = false;
                    if (CurrentNode.CodeElement.StorageAreaReads != null)
                        foreach (var storage in CurrentNode.CodeElement.StorageAreaReads)
                        {
                            if (GenQualifiedStorage(storage, CurrentNode.CodeElement))
                                bHasGeNodes = true;
                        }
                    if (CurrentNode.CodeElement.StorageAreaWrites != null)
                        foreach (var storage in CurrentNode.CodeElement.StorageAreaWrites)
                        {
                            if (GenQualifiedStorage(storage.StorageArea, CurrentNode.CodeElement))
                                bHasGeNodes = true;
                        }
                    if (bHasGeNodes)
                    {
                        CurrentNode.Comment = true;
                    }
                }
            }

            private void PerformMatch()
            {
                if (HasMatch)
                {
                    //Add in the list the last qualified sequence
                    AllItemsList.Add(Items);
                    Perform(this.CurrentNode);
                    Items = null;
                    AllItemsList.Clear();
                }
                else 
                {//Check if this node has Reads/Writes Storages
                    GenQualificationForNodeWithReadsWritesStorage();
                }
            }

            private static bool EqualItems(IList<SymbolReference> items1, IList<SymbolReference> items2)
            {
                if (items1.Count != items2.Count)
                    return false;
                int n = items1.Count;
                for (int i = 0; i < n; i++)
                    if (items1[i] != items2[i])
                        return false;
                return true;
            }

            /// <summary>
            /// Checks and handles any procedure call resolution.
            /// </summary>
            /// <param name="items">The items to check if they correspond to the procedure qualified name</param>
            /// <returns>true if it was a Procedure style call, false otherwise.</returns>
            private bool IsProcedureStyleCallItems(IList<SymbolReference> items, out string hashFunction)
            {
                hashFunction = null;

                TypeCobol.Compiler.Nodes.ProcedureStyleCall procStyleCall = CurrentNode as TypeCobol.Compiler.Nodes.ProcedureStyleCall;
                if (procStyleCall != null)
                {
                    ProcedureStyleCallStatement procStyleCallStmt = procStyleCall.CodeElement;
                   
                    TypeCobolQualifiedSymbolReference tcqsr = procStyleCallStmt.ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction as
                                                                  TypeCobolQualifiedSymbolReference ?? procStyleCallStmt.ProcdurePointerOrTCProcedureFunction as TypeCobolQualifiedSymbolReference;

                    if (tcqsr != null)
                    {
                        IList<SymbolReference> names_items = tcqsr.AsList();
                        if (names_items.Count != items.Count)
                            return false;
                        if (EqualItems(items, names_items))
                        {//This is a reference to a Function Call.
                            hashFunction = procStyleCall.FunctionDeclaration.Hash;
                            if (ProcCallerStack != null && ProcCallerStack.Count > 0)
                            {   //Memorize the (hash,ProcedureStyleCall) In the Program procedure style call dictionary.
                                var program = ProcCallerStack.Peek();
                                if (!program.ProcStyleCalls.ContainsKey(hashFunction))
                                    program.ProcStyleCalls[hashFunction] = new Tuple<IList<SymbolReference>, TypeCobol.Compiler.Nodes.ProcedureStyleCall>(items, procStyleCall);
                            }
                            return true;
                        }
                    }
                    
                }
                return false;
            }
            /// <summary>
            /// Have we matched a Type Cobol Qualifier ?
            /// </summary>
            public bool HasMatch
            {
                get
                {
                    return this.CurrentNode != null && Items != null;
                }
            }

            /// <summary>
            /// For all Items the indices of their token in the consumed Tokens.
            /// </summary>
            /// <returns>The List of boundary tuple.</returns>
            private List<Tuple<int, int>> ItemsListIndexBoundary(IList<TypeCobol.Compiler.Scanner.Token> nodeTokens)
            {
                List<Tuple<int, int>> boundaries = new List<Tuple<int, int>>();
                foreach (var items in AllItemsList)
                {
                    SymbolReference sr1 = items[0];
                    int index1 = FindSymbolRefererenceTokenIndex(sr1, nodeTokens);
                    SymbolReference sr2 = items[items.Count - 1];
                    int index2 = FindSymbolRefererenceTokenIndex(sr2, nodeTokens);
                    boundaries.Add(new Tuple<int, int>(index2, index1));
                }
                return boundaries;
            }

            /// <summary>
            /// Get the index of the corresponding SymbolReference token in the given token list.
            /// </summary>
            /// <param name="sr">The SymbolReference token</param>
            /// <param name="nodeTokens">The loken list</param>
            /// <returns>The index if found, -1 otherwise.</returns>
            private static int FindSymbolRefererenceTokenIndex(SymbolReference sr, IList<TypeCobol.Compiler.Scanner.Token> nodeTokens)
            {
                for (int i = 0; i < nodeTokens.Count; i++)
                {
                    if (nodeTokens[i] == sr.NameLiteral.Token)
                        return i;
                }
                return -1;
            }


            /// <summary>
            /// Determine if a Data Definition is included a TypeDefintion
            /// </summary>
            /// <param name="dataDef">The Data Definition to check</param>
            /// <returns>true if the DataDefintion is included in a TypeDefinition, false otherwise</returns>
            internal bool IsTypeDefinition(TypeCobol.Compiler.Nodes.DataDefinition dataDef)
            {
                while (dataDef != null)
                {
                    if (dataDef is TypeCobol.Compiler.Nodes.TypeDefinition)
                        return true;
                    dataDef = dataDef.Parent as TypeCobol.Compiler.Nodes.DataDefinition;
                }
                return false;
            }


            /// <summary>
            /// Check if the given items are in the source node qualified storage areas.
            /// </summary>
            /// <param name="items">The items to check</param>
            /// <param name="sourceNode">The source node</param>
            /// <param name="qualified_name">The qualified name that corresponds the the items.</param>
            /// <returns>true if the items are in the source node storage areas, false otherwise</returns>
            internal bool AreItemsInNodeQualifiedStorageAreas(IList<SymbolReference> items, Node sourceNode, out string qualified_name)
            {
                qualified_name = null;
                if (sourceNode.QualifiedStorageAreas == null)
                    return false;
                foreach (TypeCobol.Compiler.CodeElements.StorageArea storage_area in sourceNode.QualifiedStorageAreas.Keys)
                {
                    if (storage_area.SymbolReference != null && storage_area.SymbolReference.IsTypeCobolQualifiedReference)
                    {
                        TypeCobolQualifiedSymbolReference tc_sr = storage_area.SymbolReference as TypeCobolQualifiedSymbolReference;
                        IList<SymbolReference> tcsr_items = tc_sr?.AsList();
                        int nCountInner = 0;
                        foreach (SymbolReference item in items)
                        {
                            if (tcsr_items != null && tcsr_items.IndexOf(item) >= 0)
                                nCountInner++;
                            else
                                break;
                        }
                        if (nCountInner == items.Count)
                        {

                            qualified_name = this.CurrentNode.GetQualifiedName(storage_area);

                            return true;
                        }
                    }
                }
                return false;
            }

            /// <summary>
            /// Perform the qualification action
            /// </summary>
            /// <param name="sourceNode">The source Node on which to perform teh action</param>
            /// <param name="visitor">The Visitor which as locate teh Source Node</param>
            internal void Perform(Node sourceNode)
            {
                if (sourceNode.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                    return;
                    TypeCobol.Compiler.Nodes.DataDescription dataDescription = null;
                if (sourceNode is TypeCobol.Compiler.Nodes.DataDescription && IsTypeDefinition(sourceNode as TypeCobol.Compiler.Nodes.DataDescription))
                {
                    dataDescription = sourceNode as TypeCobol.Compiler.Nodes.DataDescription;
                    if (dataDescription.QualifiedTokenSubsitutionMap == null)
                        dataDescription.QualifiedTokenSubsitutionMap = new Dictionary<Compiler.Scanner.Token, string>();                    
                }

                //Now this Node Is Visited
                sourceNode.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                Tuple<int, int, int, List<int>, List<int>> sourcePositions = this.Generator.FromToPositions(sourceNode);
                IList<TypeCobol.Compiler.Scanner.Token> nodeTokens = sourceNode.CodeElement.ConsumedTokens;
                List<Tuple<int, int>> boundaries = ItemsListIndexBoundary(nodeTokens);
                int b = 0;
                bool bWasProcCall = false;
                foreach (var items in AllItemsList)
                {
                    string hashFunction;
                    bool bProcCall = IsProcedureStyleCallItems(items, out hashFunction);
                    Tuple<int, int> range = boundaries[b++];
                    Items = items;
                    int i = range.Item1;
                    if (bProcCall)
                    {   //----------------------------------------------------------------------------------------------
                        // This is for a procedure call.
                        // The Code below is commented. This code was used to test that in normal situation
                        // The TypeCobolQualifierReference for the function name can be replaced by a the hash code name.
                        //----------------------------------------------------------------------------------------------
                        //SymbolReference sr1 = Items[Items.Count - 1];
                        //SymbolReference sr2 = Items[0];                        
                        //List<TypeCobol.Compiler.Scanner.Token> consumedTokens = new List<TypeCobol.Compiler.Scanner.Token>();
                        //for (; i <= range.Item2; i++)
                        //{
                        //    if (nodeTokens[i] == sr1.NameLiteral.Token)
                        //    {
                        //        consumedTokens.Add(nodeTokens[i]);                                
                        //    }
                        //    else if (nodeTokens[i] == sr2.NameLiteral.Token)
                        //    {
                        //        consumedTokens.Add(nodeTokens[i]);
                        //        break;
                        //    }
                        //}
                        //GenerateQualifierToken item = new GenerateQualifierToken(new QualifierTokenCodeElement(consumedTokens), "'" + hashFunction + "'",
                        //    sourcePositions);
                        //item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                        //sourceNode.Add(item);
                        //------------------------------------------------------------------------------------------------------------------------

                        bWasProcCall = true;//Remember that we have a Procedure Style Call Node.
                        continue;//Continue
                    }
                    if (sourceNode.IsFlagSet(Node.Flag.NodeContainsIndex))
                    {
                        //So we must know if this qualified name is for an Index Name
                        string qualified_name;
                        bool bAreIn = AreItemsInNodeQualifiedStorageAreas(items, sourceNode, out qualified_name);
                        if (bAreIn)
                        {
                            GenerateToken item = null;
                            string hashName = GeneratorHelper.ComputeIndexHashName(qualified_name, sourceNode);
                            //Now all items in the qualified name must be replaced with the hash name by the Generator.
                            //So all items except the last one are replaced by a blank, the last item will be the HashName
                            for (int r = i; r <= range.Item2 - 1; r++)
                            {                                
                                item = new GenerateToken(
                                    new TokenCodeElement(nodeTokens[r]), "",
                                    sourcePositions);
                                item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                                sourceNode.Add(item);
                             }
                            item = new GenerateToken(
                                new TokenCodeElement(nodeTokens[range.Item2]), hashName,
                                sourcePositions);
                            item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                            sourceNode.Add(item);
                            continue;
                        }
                    }

                    for (int j = 0; j < Items.Count; j++)
                    {
                        SymbolReference sr = Items[Items.Count - j - 1];
                        for (; i <= range.Item2; i++)
                        {
                            if (nodeTokens[i] == sr.NameLiteral.Token)
                            {
                                TypeCobol.Compiler.Scanner.Token tokenColonColon = null;
                                //Look for the corresponding ::
                                for (++i; i <= range.Item2; i++)
                                {
                                    if (!(nodeTokens[i] is TypeCobol.Compiler.Preprocessor.ImportedToken))
                                    {
                                        if (nodeTokens[i].TokenType == TypeCobol.Compiler.Scanner.TokenType.QualifiedNameSeparator)
                                        {
                                            tokenColonColon = nodeTokens[i];
                                            i++;
                                            break;
                                        }
                                    }
                                }
                                //We got It ==> Create our Generate Nodes
                                if (dataDescription != null)
                                {
                                    dataDescription.QualifiedTokenSubsitutionMap[sr.NameLiteral.Token] = Items[j].ToString();
                                    if (tokenColonColon != null)
                                        dataDescription.QualifiedTokenSubsitutionMap[tokenColonColon] = "OF";
                                }
                                else
                                {
                                    GenerateToken item = null;
                                    string replace_value = Items[j].ToString();

                                    if (this.CurrentNode.IsFlagSet(Node.Flag.NodeContainsBoolean))
                                    {
                                        if (this.CurrentNode.QualifiedStorageAreas.Keys.Any(flaggedStorageArea => flaggedStorageArea?.SymbolReference?.NameLiteral.Value == replace_value))
                                        {
                                            replace_value = replace_value + "-value";
                                        }
                                    }
                                   
                                    item = new GenerateToken(new TokenCodeElement(sr.NameLiteral.Token), replace_value, sourcePositions);
                                    item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                                    sourceNode.Add(item);
                                    if (tokenColonColon != null)
                                    {
                                        item = new GenerateToken(new TokenCodeElement(tokenColonColon), string.Intern(" OF "),
                                            sourcePositions);
                                        item.SetFlag(Node.Flag.HasBeenTypeCobolQualifierVisited, true);
                                        sourceNode.Add(item);
                                    }
                                }
                                break;//We got it
                            }
                        }
                    }
                }
                //Now Comment the Source Node, only and only if it was not a Procedure Style Call,
                //Because the Qualifier action is the first action performed, before any Expand action.
                //Expand Action does not expand Commented Nodes, it will comment theses nodes itself.
                if (!bWasProcCall)
                    sourceNode.Comment = true;
            }

        }

        /// <summary>
        /// The Code Element of a Qualifier Token.
        /// </summary>
        internal class TokenCodeElement : TypeCobol.Compiler.CodeElements.CodeElement
        {
            public TokenCodeElement(TypeCobol.Compiler.Scanner.Token token) : base((CodeElementType)0)
            {
                base.ConsumedTokens = new List<TypeCobol.Compiler.Scanner.Token>();
                base.ConsumedTokens.Add(token);
            }
            public TokenCodeElement(List<TypeCobol.Compiler.Scanner.Token> consumedTokens)
                : base((CodeElementType)0)
            {
                base.ConsumedTokens = consumedTokens;
            }
        }

        /// <summary>
        /// A Node to just generate Qualifier tokens.
        /// </summary>
        internal class GenerateToken : GenericNode<CodeElement>, GeneratedAndReplace
        {
            /// <summary>
            /// 
            /// </summary>
            /// <param name="codelement">The Code element of this Node</param>
            /// <param name="code">The replace code</param>
            /// <param name="sourcePositions">The Positions of the Source Node</param>
            public GenerateToken(CodeElement codelement, string code, Tuple<int, int, int, List<int>, List<int>> sourcePositions)
                : base(codelement)
            {
                ReplaceCode = code;
                SourceNodePositions = sourcePositions;
            }

            /// <summary>
            /// Source Node Positions
            /// </summary>
            public Tuple<int, int, int, List<int>, List<int>> SourceNodePositions
            {
                get;
                private set;
            }

            public string ReplaceCode
            {
                get;
                set;
            }


            public bool IsLeaf
            {
                get { return true; }
            }

            public override bool VisitNode(IASTVisitor astVisitor)
            {
                return true;
            }
        }


        /// <summary>
        /// Add the given GenerateToken in the given node at the right position in the
        /// node's children sorted by other GenerateToken instances locations.
        /// </summary>
        /// <param name="token">The token to add as child</param>
        /// <param name="node">The parent node</param>
        internal static void AddGenerateTokenAtSortedLocationIndex(GenerateToken token, Node node)
        {
            if (node.ChildrenCount == 0)
            {
                node.Add(token);
                return;
            }

            int currentIndex = -1;//curent child index 
            int insertIndex = -1;//Prefered position index
            int lastIndex = -1;//Last index of a GenerateToken child
            int line = token.CodeElement.ConsumedTokens[0].Line;
            int column = token.CodeElement.ConsumedTokens[0].Column;
            Node target = node.Children.FirstOrDefault(n =>
                {
                    bool istoken = n is GenerateToken;
                    bool bResult =  istoken &&
                                   (line <= n.CodeElement.ConsumedTokens[0].Line &&
                                    column <= n.CodeElement.ConsumedTokens[0].Column);
                    currentIndex++;
                    if (bResult)
                    {
                        insertIndex = currentIndex;
                    }

                    if (istoken)
                    {
                        lastIndex = currentIndex;
                    }
                    return bResult;
                }
            );
            if (target == null)
            {//No Match
                node.Add(token, lastIndex != -1 ? lastIndex + 1 : -1);
            }
            else
            {
                node.Add(token, insertIndex);
            }            
        }

        /// <summary>
        /// The Source of the Qualifation
        /// </summary>
        private Node Source;
        /// <summary>
        /// The Generator Instance
        /// </summary>
        internal Generator Generator;
        /// <summary>
        /// Node to text for qualificers
        /// </summary>
        /// <param name="node">The source node</param>
        /// <param name="generator">The Generator instance</param>
        public Qualifier(Generator generator, Node node)
        {
            this.Source = node;
            this.Generator = generator;
        }
        public string Group
        {
            get
            {
                return null;
            }
        }
        /// <summary>
        /// Execute the Qualification action
        /// <param name="generator">The Genarator instance</param>
        /// </summary>
        public void Execute()
        {
            if (Source == null)
                return;
            if (Source.IsFlagSet(Node.Flag.HasBeenTypeCobolQualifierVisited))
                return;
            TypeCobolCobolQualifierVistor visitor = new TypeCobolCobolQualifierVistor(Generator);
            Source.AcceptASTVisitor(visitor);
        }
    }
}
