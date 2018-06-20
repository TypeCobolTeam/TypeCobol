using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Diagnostics
{
    public class CrossCompleteChecker : AbstractAstVisitor
    {
        private Node CurrentNode { get; set; }

        public override bool BeginNode(Node node)
        {
            CurrentNode = node;
            //Build node StorageAreaWritesDataDefinition and StorageAreaReadsDataDefinition dictionaries
            //from CodelElement StorageAreaReads and StorageAreaWrites
            CodeElement codeElement = node.CodeElement;
            if (codeElement?.StorageAreaReads != null)
            {
                foreach (var storageAreaRead in codeElement.StorageAreaReads)
                {
                    CheckVariable(node, storageAreaRead,true);
                }
            }
            if (codeElement?.StorageAreaWrites != null)
            {
                foreach (var storageAreaWrite in codeElement.StorageAreaWrites)
                {
                    CheckVariable(node, storageAreaWrite.StorageArea,false);
                }
            }
            //Build node StorageAreaWritesDataDefinition and StorageAreaReadsDataDefinition dictionaries
            //for Corresponding instruction from StorageAreaGroupsCorrespondingImpact
            if (codeElement?.StorageAreaGroupsCorrespondingImpact != null)
            {
                CheckVariable(node, codeElement.StorageAreaGroupsCorrespondingImpact.SendingGroupItem, true);
                CheckVariable(node, codeElement.StorageAreaGroupsCorrespondingImpact.ReceivingGroupItem, false);
            }

            RedefinesChecker<CodeElement>.OnNode(node);
            FunctionDeclarationChecker<CodeElement>.OnNode(node);
            FunctionCallChecker.OnNode(node);
            SetStatementChecker.OnNode(node);
            TypedDeclarationChecker.OnNode(node);
            RenamesChecker.OnNode(node);
            ReadOnlyPropertiesChecker.OnNode(node);

            return true;
        }

        public override bool BeginCodeElement(CodeElement codeElement)
        {
            //This checker is only for Node after the full AST has been created
            return false;
        }

        public override bool Visit(PerformProcedure performProcedureNode)
        {
            SectionOrParagraphUsageChecker.CheckReferenceToParagraphOrSection(performProcedureNode);
            return true;
        }

        public override bool Visit(Paragraph paragraph)
        {
            SectionOrParagraphUsageChecker.CheckParagraph(paragraph);
            return true;
        }

        public override bool Visit(ProcedureDivision procedureDivision)
        {
            LibraryChecker.CheckLibrary(procedureDivision);
            return true;
        }

        public override bool Visit(Section section)
        {
            SectionOrParagraphUsageChecker.CheckSection(section);
            return true;
        }

        public override bool Visit(Move move)
        {
            var moveCorresponding = move?.CodeElement as MoveCorrespondingStatement;
            if (moveCorresponding == null)
                return true;

            Tuple<string, DataDefinition> searchedDataDefintion = null;
            DataDefinition fromVariable = null;
            DataDefinition toVariable = null;
            //For MoveCorrespondingStatement check children compatibility
            if (move.StorageAreaReadsDataDefinition.TryGetValue(moveCorresponding.FromGroupItem,
                out searchedDataDefintion))
            {
                fromVariable = searchedDataDefintion.Item2;
            }
            if (move.StorageAreaWritesDataDefinition.TryGetValue(moveCorresponding.ToGroupItem,
                out searchedDataDefintion))
            {
                toVariable = searchedDataDefintion.Item2;
            }
            if (fromVariable == null || toVariable == null)
            {
                return true; //Do not continue, the variables hasn't been found. An error will be raised later by CheckVariable()
            }

           var fromVariableChildren = fromVariable.Children.Where(c => c?.Name != null);
            var toVariableChildren = toVariable.Children.Where(c => c?.Name != null);

            var matchingChildrenNames = fromVariableChildren.Select(c => c.Name.ToLowerInvariant()).Intersect(toVariableChildren.Select(c => c.Name.ToLowerInvariant()));

            foreach (var matchingChildName in matchingChildrenNames)
            {
                var retrievedChildrenFrom = fromVariableChildren.Where(c => c.Name.ToLowerInvariant() == matchingChildName);
                var retrievedChildrenTo = toVariableChildren.Where(c => c.Name.ToLowerInvariant() == matchingChildName);

                if ((retrievedChildrenFrom != null && retrievedChildrenFrom.Count() != 1) || (retrievedChildrenTo != null && retrievedChildrenTo.Count() != 1))
                    DiagnosticUtils.AddError(move, string.Format("Multiple symbol \"{0}\" detected in MOVE CORR", matchingChildName));

                var retrievedChildFrom = (retrievedChildrenFrom.First() as DataDefinition);
                var retrievedChildTo = (retrievedChildrenTo.First() as DataDefinition);

                if (retrievedChildFrom == null || retrievedChildTo == null)
                    continue; //Doesn't have to happen but in case...

                var fromDataType = retrievedChildFrom.DataType;
                var toDataType = retrievedChildTo.DataType;

                if (fromDataType != toDataType && fromDataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85 && toDataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85) //Check DataType matching
                    DiagnosticUtils.AddError(move, string.Format("Symbol {0} of type {1} do not match symbol {2} of type {3}", retrievedChildFrom.VisualQualifiedName, fromDataType, retrievedChildTo.VisualQualifiedName, toDataType));
            }

            return true;
        }

        public override bool Visit(TypeDefinition typeDefinition)
        {
            //Cobol 2002 rule
            //TODO need to clarify if we have 1 visitor per LanguageLevel
            //For performance reason it seems better to have only one here
            TypeDefinitionChecker.CheckTypeDefinition(typeDefinition);
            return true;
        }

        public override bool VisitVariableWriter(VariableWriter variableWriter)
        {
            WriteTypeConsistencyChecker.OnNode(variableWriter, CurrentNode);
            return true;
        }

        public override bool Visit(DataDefinition dataDefinition)
        {
            if (dataDefinition.CodeElement is CommonDataDescriptionAndDataRedefines)
            {
                CheckPicture(dataDefinition);
            }

            //Check if DataDefinition is level 88 and declared under BOOL variable
            if (!(dataDefinition.CodeElement is DataDefinitionEntry)) return true;

            var levelNumber = ((DataDefinitionEntry) dataDefinition.CodeElement).LevelNumber;
            var dataDefinitionParent = (dataDefinition.Parent as DataDefinition);
            if (levelNumber != null && dataDefinitionParent != null &&
                dataDefinitionParent.DataType == DataType.Boolean && levelNumber.Value == 88)
            {
                DiagnosticUtils.AddError(dataDefinition.CodeElement,
                    "The Level 88 symbol '" + dataDefinition.Name + "' cannot be declared under a BOOL typed symbol");
            }
            if (levelNumber != null && !(levelNumber.Value == 01 || levelNumber.Value == 77) &&
                dataDefinitionParent == null)
            {
                DiagnosticUtils.AddError(dataDefinition.CodeElement,
                    "The variable '" + dataDefinition.Name + "' can only be of level 01 or 77");
            }

            return true;
        }

        public override bool Visit(IndexDefinition indexDefinition)
        {
            var found =
                indexDefinition.SymbolTable.GetVariablesExplicit(new URI(indexDefinition.Name))
                    .Where(i => i.GetParentTypeDefinition == null)
                    .ToList();
            if (indexDefinition.GetParentTypeDefinition != null) return true;
            if (found.Count > 1) //If multiple index with same name found, display a warning.
            {
                DiagnosticUtils.AddError(indexDefinition.Parent.CodeElement,
                    "An index named '" + indexDefinition.Name + "' is already defined.", MessageCode.Warning);
            }
            return true;
        }

        public static void CheckPicture(Node node, CommonDataDescriptionAndDataRedefines customCodeElement = null)
        {
            var codeElement = customCodeElement ?? node.CodeElement as CommonDataDescriptionAndDataRedefines;
            if (codeElement?.Picture == null) return;


            // if there is not the same number of '(' than of ')'
            if ((codeElement.Picture.Value.Split('(').Length - 1) != (codeElement.Picture.Value.Split(')').Length - 1))
            {
                DiagnosticUtils.AddError(node, "missing '(' or ')'");
            }
            // if the first '(' is after first ')' OR last '(' is after last ')'
            else if (codeElement.Picture.Value.IndexOf("(") > codeElement.Picture.Value.IndexOf(")") || codeElement.Picture.Value.LastIndexOf("(") > codeElement.Picture.Value.LastIndexOf(")"))
                DiagnosticUtils.AddError(node, "missing '(' or ')'");
            else
            {
                foreach (Match match in Regex.Matches(codeElement.Picture.Value, @"\(([^)]*)\)"))
                {
                    try //Try catch is here because of the risk to parse a non numerical value
                    {
                        int.Parse(match.Value, System.Globalization.NumberStyles.AllowParentheses);
                    }
                    catch (Exception)
                    {
                        var m = "Given value is not correct : " + match.Value + " expected numerical value only";
                        DiagnosticUtils.AddError(node, m);
                    }
                }
            }
        }

        public static DataDefinition CheckVariable(Node node, StorageArea storageArea, bool isReadStorageArea)
        {
            if (storageArea == null || !storageArea.NeedDeclaration)
                return null;

            var area = storageArea.GetStorageAreaThatNeedDeclaration;
            if (area.SymbolReference == null) return null;
            //Do not handle TCFunctionName, it'll be done by TypeCobolChecker
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName)) return null;
            //need to initialize the dictionaries before the search
            if (isReadStorageArea && node.StorageAreaReadsDataDefinition == null)
            {
                node.StorageAreaReadsDataDefinition = new Dictionary<StorageArea, Tuple<string, DataDefinition>>();
            }
            if (!isReadStorageArea && node.StorageAreaWritesDataDefinition == null)
            {
                node.StorageAreaWritesDataDefinition = new Dictionary<StorageArea, Tuple<string,DataDefinition>>();
            }

            //search for existing data definitinon before constructing one
            Tuple<string,DataDefinition> searchExistingDataDefinition;
            if (isReadStorageArea)
            {
                node.StorageAreaReadsDataDefinition
                    .TryGetValue(storageArea, out searchExistingDataDefinition);
            }
            else
            {
                node.StorageAreaWritesDataDefinition
                    .TryGetValue(storageArea, out searchExistingDataDefinition);
            }
            if (searchExistingDataDefinition!=null)
            {
                IndexAndFlagDataDefiniton(searchExistingDataDefinition.Item1, searchExistingDataDefinition.Item2,node,area,storageArea);
                return searchExistingDataDefinition.Item2;
            }
            //IEnumerable<DataDefinition> found;
            //var foundQualified = new List<KeyValuePair<string, DataDefinition>>();

            var isPartOfTypeDef = (node as DataDefinition) != null && ((DataDefinition)node).IsPartOfATypeDef;
            var foundQualified =
                node.SymbolTable.GetVariablesExplicitWithQualifiedName(area.SymbolReference != null
                    ? area.SymbolReference.URI
                    : new URI(area.ToString()),
                    isPartOfTypeDef ? ((DataDefinition) node).GetParentTypeDefinition
                    :null);
            var found = foundQualified.Select(v => v.Value);
            
            if (found.Count() == 1 && foundQualified.Count == 1)
            {
                string completeQualifiedName = foundQualified.First().Key;
                IndexAndFlagDataDefiniton(completeQualifiedName, found.First(), node, area,storageArea);
            }

            if (!found.Any())
                if (node.SymbolTable.GetFunction(area).Count < 1)
                    DiagnosticUtils.AddError(node, "Symbol " + area + " is not referenced");
            if (found.Count() > 1)
            {
                bool isFirst = true;
                string errorMessage = "Ambiguous reference to symbol " + area + " " + Environment.NewLine +
                                      "Symbols found: ";
                foreach (var symbol in foundQualified)
                {
                    // Multiline Version
                    //errorMessage += Environment.NewLine + "\t" + symbol.Key.Replace(".", "::");
                    // Inline version
                    errorMessage += (isFirst ? "" : " | ") + symbol.Key.Replace(".", "::");
                    isFirst = false;
                }
                DiagnosticUtils.AddError(node, errorMessage);
            }

            if (found.Count() == 1)
            {
                //add the found DataDefinition to a dictionary depending on the storage area type
                if (isReadStorageArea)
                {
                    string completeQualifiedName = foundQualified.First().Key;
                    node.StorageAreaReadsDataDefinition.Add(storageArea,new Tuple<string, DataDefinition>(completeQualifiedName,found.First()));
                }
                else
                {
                    string completeQualifiedName = foundQualified.First().Key;
                    node.StorageAreaWritesDataDefinition.Add(storageArea,new Tuple<string, DataDefinition>(completeQualifiedName,found.First()));
                }

                return found.First();
            }

            return null;
        }

        private static void IndexAndFlagDataDefiniton(string completeQualifiedName, DataDefinition dataDefinition,
            Node node, StorageArea area, StorageArea storageArea)
        {
            if (dataDefinition.IsIndex)
            {
                var index = dataDefinition;

                index.AddReferences(storageArea, node); //Add this node as a reference to the founded index

                if (area.SymbolReference.IsQualifiedReference)
                {
                    if (index.Name.Length > 22) //If index name is used with qualification and exceed 22 characters
                        DiagnosticUtils.AddError(index.Parent,
                            "Index name '" + index.Name + "' is over 22 characters.");
                    if (
                            index.Parent.CodeElement.IsInsideCopy())
                        //If index comes from a copy, do not support qualification
                        DiagnosticUtils.AddError(node,
                            "Index '" + index.Name + "' inside a COPY cannot be use with qualified symbol");
                }

                if (area.SymbolReference.IsQualifiedReference || index.IsPartOfATypeDef)
                //Index name is qualified or belongs to a typedef
                {
                    //Mark this node for generator
                    FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, node, storageArea,
                        completeQualifiedName);

                    foreach (
                        var reference in
                        index.GetReferences().Where(n => !n.Value.IsFlagSet(Node.Flag.NodeContainsIndex)))
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, reference.Value,
                            reference.Key, completeQualifiedName);
                    }
                }
                else if (!area.SymbolReference.IsQualifiedReference)
                //If it's an index but not use with qualified reference 
                {
                    //Check the previous references to see if one has been flagged as NodeContainsIndex then flag this node
                    if (index.GetReferences().Any(n => n.Value.IsFlagSet(Node.Flag.NodeContainsIndex)))
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, node, storageArea,
                            completeQualifiedName);
                    }
                }

                //No matter which node uses this index, if at least one time a node with the index with a qualified name, we need to flag the index parent 
                if (area.SymbolReference.IsQualifiedReference && !index.IsPartOfATypeDef)
                //If index is used with qualified name but doesn't belongs to typedef
                {
                    //Flag index node for code generator to let it know that this index will need hash.
                    index.SetFlag(Node.Flag.IndexUsedWithQualifiedName, true);
                }

                    if (area.SymbolReference.IsQualifiedReference && !area.SymbolReference.IsTypeCobolQualifiedReference)
                        DiagnosticUtils.AddError(node,
                            "Index can not be use with OF or IN qualifiers " + area);
                }
                else if (dataDefinition.DataType == DataType.Boolean && dataDefinition.CodeElement is DataDefinitionEntry &&
                         ((DataDefinitionEntry) dataDefinition?.CodeElement)?.LevelNumber?.Value != 88)
                {
                    if (!((node is Nodes.If && storageArea.Kind != StorageAreaKind.StorageAreaPropertySpecialRegister) || node is Nodes.Set || node is Nodes.Perform || node is Nodes.PerformProcedure || node is Nodes.WhenSearch || node is Nodes.When ) || storageArea.Kind == StorageAreaKind.StorageAreaPropertySpecialRegister)//Ignore If/Set/Perform/WhenSearch Statement
                    {
                        //Flag node has using a boolean variable + Add storage area into qualifiedStorageArea of the node. (Used in CodeGen)
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsBoolean, node, storageArea,
                            completeQualifiedName);
                    }
                }

            var specialRegister = storageArea as StorageAreaPropertySpecialRegister;
            if (specialRegister != null
                && specialRegister.SpecialRegisterName.TokenType == TokenType.ADDRESS
                && specialRegister.IsWrittenTo
                && !(node is ProcedureStyleCall))
            {
                var variabletoCheck = dataDefinition;
                //This variable has to be in Linkage Section
                if (!variabletoCheck.IsFlagSet(Node.Flag.LinkageSectionNode))
                    DiagnosticUtils.AddError(node,
                        "Cannot write into " + storageArea + ", " + variabletoCheck +
                        " is declared out of LINKAGE SECTION.");
            }

            if (specialRegister != null
                && specialRegister.SpecialRegisterName.TokenType == TokenType.ADDRESS
                && node is Call)
            {
                var callStatement = node.CodeElement as CallStatement;
                var currentCheckedParameter = callStatement?.InputParameters.FirstOrDefault(
                    param => param.StorageAreaOrValue.StorageArea == specialRegister);

                if (currentCheckedParameter != null)
                {
                    var variabletoCheck = dataDefinition;
                    //This variable has to be in Linkage Section
                    if (!variabletoCheck.IsFlagSet(Node.Flag.LinkageSectionNode) &&
                        currentCheckedParameter.SharingMode.Value == ParameterSharingMode.ByReference)
                        DiagnosticUtils.AddError(node,
                            "CALL with ADDRESS OF can only be used with a LINKAGE variable, or with a sharing mode BY CONTENT/BY VALUE");
                }
            }
        }


        private static void FlagNodeAndCreateQualifiedStorageAreas(Node.Flag flag, Node node, StorageArea storageArea,
            string completeQualifiedName)
        {
            node.SetFlag(flag, true);
            if (node.QualifiedStorageAreas == null)
                node.QualifiedStorageAreas = new Dictionary<StorageArea, string>();

            if (!node.QualifiedStorageAreas.ContainsKey(storageArea))
                node.QualifiedStorageAreas.Add(storageArea, completeQualifiedName);
        }
    }

    class SectionOrParagraphUsageChecker
    {
        public static void CheckReferenceToParagraphOrSection(PerformProcedure perform)
        {
            var performCE = (PerformProcedureStatement)perform.CodeElement;
            SymbolReference symbol;
            symbol = ResolveProcedureName(perform.SymbolTable, performCE.Procedure as AmbiguousSymbolReference, perform);
            if (symbol != null) performCE.Procedure = symbol;
            symbol = ResolveProcedureName(perform.SymbolTable, performCE.ThroughProcedure as AmbiguousSymbolReference,
                perform);
            if (symbol != null) performCE.ThroughProcedure = symbol;
        }

        /// <summary>Disambiguate between section and paragraph names</summary>
        /// <param name="table">Symbol table used for name resolution</param>
        /// <param name="symbol">Symbol to disambiguate</param>
        /// <param name="ce">Original CodeElement ; error diagnostics will be added to it if name resolution fails</param>
        /// <returns>symbol as a SymbolReference whith a SymbolType properly set</returns>
        private static SymbolReference ResolveProcedureName(SymbolTable table, SymbolReference symbol, Node node)
        {
            if (symbol == null) return null;

            SymbolReference sname = null, pname = null;
            var sfound = table.GetSection(symbol.Name);
            if (sfound.Count > 0) sname = new SymbolReference(symbol.NameLiteral, SymbolType.SectionName);
            var pfound = table.GetParagraph(symbol.Name);
            if (pfound.Count > 0) pname = new SymbolReference(symbol.NameLiteral, SymbolType.ParagraphName);

            if (pname == null)
            {
                if (sname == null)
                {
                    DiagnosticUtils.AddError(node, "Symbol " + symbol.Name + " is not referenced");
                }
                else
                {
                    if (sfound.Count > 1)
                        DiagnosticUtils.AddError(node, "Ambiguous reference to section " + symbol.Name);
                    return sname;
                }
            }
            else
            {
                if (sname == null)
                {
                    if (pfound.Count > 1)
                        DiagnosticUtils.AddError(node, "Ambiguous reference to paragraph " + symbol.Name);
                    return pname;
                }
                else
                {
                    DiagnosticUtils.AddError(node, "Ambiguous reference to procedure " + symbol.Name);
                }
            }
            return null;
        }

        protected static void Check<T>(T node, [NotNull] IList<T> found) where T : Node
        {
            if (found.Count > 1) DiagnosticUtils.AddError(node, "Symbol \'" + node.Name + "\' already declared");
        }

        public static void CheckSection(Section section)
        {
            Check(section, section.SymbolTable.GetSection(section.Name));
        }

        public static void CheckParagraph(Paragraph paragraph)
        {
            Check(paragraph, paragraph.SymbolTable.GetParagraph(paragraph.Name));
        }
    }

    class WriteTypeConsistencyChecker
    {
        public static void OnNode(VariableWriter variableWriter, Node node)
        {
            if (variableWriter == null)
            {
                return; //not our job
            }
            var variables = variableWriter.VariablesWritten;
            foreach (var variable in variables) CheckVariable(node, variable.Key, variable.Value);
        }

        /// <param name="wname">Receiving item; must be found and its type known</param>
        /// <param name="sent">Sending item; must be found and its type known</param>
        private static void CheckVariable(Node node, StorageArea wname, object sent)
        {
            DataDefinition sendingTypeDefinition = null, receivingTypeDefinition = null;

            if (sent == null || wname == null) return; //Both items needed
            //var wsymbol = CrossCompleteChecker.CheckVariable(node, wname,false);
            Tuple<string, DataDefinition> searchExistingDataDefinition;
            DataDefinition wsymbol = null;
            //check if dico not null
            if (node.StorageAreaWritesDataDefinition != null)
            {
                node.StorageAreaWritesDataDefinition.TryGetValue(wname, out searchExistingDataDefinition);
                wsymbol = searchExistingDataDefinition?.Item2;
            }


            if (wsymbol != null)
                receivingTypeDefinition = wsymbol.TypeDefinition ?? GetDataDefinitionType(node, wsymbol,false);

            var sname = sent as QualifiedName;
            if (sname != null)
            {
                var ssymbol = node.GetDataDefinitionForQualifiedName(sname);
                if (ssymbol == null) return; // sending symbol name unresolved
                sendingTypeDefinition = ssymbol.TypeDefinition ?? GetDataDefinitionType(node, ssymbol,true);
            }
            else if (sent is StorageArea)
            {
                DataDefinition rsymbol=null;
                //var rsymbol = CrossCompleteChecker.CheckVariable(node, (StorageArea) sent,true);    
                if (node.StorageAreaReadsDataDefinition != null)
                {
                    node.StorageAreaReadsDataDefinition.TryGetValue((StorageArea)sent, out searchExistingDataDefinition);
                    rsymbol = searchExistingDataDefinition?.Item2;
                }

                if (rsymbol != null)
                    sendingTypeDefinition = rsymbol.TypeDefinition ?? GetDataDefinitionType(node, rsymbol,true);
            }
            else
            {
                //This will resolve the following cases MOVE 1 TO myVar / MOVE true TO myVar / MOVE "test" TO myVar. 
                if (sent is bool?) sendingTypeDefinition = GeneratedDefinition.BooleanGeneratedDefinition;
                if (sent is double?) sendingTypeDefinition = GeneratedDefinition.NumericGeneratedDefinition;
                if (sent is string) sendingTypeDefinition = GeneratedDefinition.AlphanumericGeneratedDefinition;
            }

            //TypeDefinition Comparison
            if (receivingTypeDefinition != null && !(receivingTypeDefinition.Equals(sendingTypeDefinition) || (wname is StorageAreaPropertySpecialRegister && sent is StorageAreaPropertySpecialRegister)))
            {
                var isUnsafe = ((VariableWriter) node).IsUnsafe;
                if (receivingTypeDefinition.DataType.RestrictionLevel > RestrictionLevel.WEAK)
                {
                    if (!isUnsafe)
                    {
                        var sendingName = sendingTypeDefinition != null ? sendingTypeDefinition.DataType.Name : null;
                        var receivingName = receivingTypeDefinition.DataType.Name;

                        if (sendingTypeDefinition != null &&
                            sendingTypeDefinition.DataType.Name == receivingTypeDefinition.DataType.Name)
                        //In case type names are equals
                        {
                            sendingName = sendingTypeDefinition.VisualQualifiedName.ToString().Replace(".", "::");
                            receivingName = receivingTypeDefinition.VisualQualifiedName.ToString().Replace(".", "::");
                        }

                        var message = string.Format("Cannot write {0} to {1} typed variable {2}:{3}.", sendingName,
                            receivingTypeDefinition.DataType.RestrictionLevel == RestrictionLevel.STRONG
                                ? "strongly"
                                : "strictly", wname, receivingName);

                        DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
                    }
                }
                else
                {
                    if (isUnsafe)
                    {
                        var message = "Useless UNSAFE with non strongly typed receiver.";
                        DiagnosticUtils.AddError(node, message, MessageCode.SyntaxWarningInParser);
                    }
                }
            }
        }

       

        //TODO move this method to DataDefinition
        /// <summary>
        /// Allows to get DataType of a DataDefinition Node
        /// </summary>
        /// <param name="node"></param>
        /// <param name="symbol"></param>
        /// <param name="isReadDictionary"></param>
        /// <returns></returns>
        private static DataDefinition GetDataDefinitionType(Node node, Node symbol, bool isReadDictionary)
        {
            var data = symbol as DataDefinition;
            if (data != null)
            {
                var dataCondition = data as DataCondition;
                if (dataCondition != null)
                    return new GeneratedDefinition(dataCondition.CodeElement().DataType.Name,
                        dataCondition.CodeElement().DataType);

                DataDescriptionEntry entry;
                var descriptionEntry = data.CodeElement as DataDescriptionEntry;
                if (descriptionEntry != null)
                {
                    entry = descriptionEntry;
                }
                else if (data.CodeElement as DataRedefinesEntry!=null)
                {
                    var redefines = (DataRedefinesEntry) data.CodeElement;
                    var searchedDataDefinition = node.GetDataDefinitionForQualifiedName(redefines.RedefinesDataName.URI, isReadDictionary);
                    if (searchedDataDefinition as DataDescription != null)
                    {
                        entry = (DataDescriptionEntry) searchedDataDefinition.CodeElement;
                    }
                    else
                    {
                        entry = GetDataDescriptionEntry(node, redefines, isReadDictionary);
                    }
                }
                else if (data is IndexDefinition)
                {
                    entry = null;
                }
                else
                    throw new NotImplementedException(data.CodeElement.GetType().Name);

                if (entry == null)
                    return null;

                if (entry.UserDefinedDataType == null)
                    return new GeneratedDefinition(entry.DataType.Name, entry.DataType);
            }
            ITypedNode typed = symbol as ITypedNode;
            if (typed == null) return null; // symbol untyped
            var types = node.SymbolTable.GetType(typed);
            // return null if symbol type not found or ambiguous
            return types.Count != 1 ? null : types[0];
        }

        /// <summary>
        /// Quick and dirty method, this checker need to be refactored
        /// </summary>
        /// <param name="node"></param>
        /// <param name="dataRedefinesEntry"></param>
        /// <param name="isReadDictionary"></param>
        /// <returns></returns>
        private static DataDescriptionEntry GetDataDescriptionEntry(Node node,
            DataRedefinesEntry dataRedefinesEntry, bool isReadDictionary)
        {
            var searchedDataDefinition = node.GetDataDefinitionForQualifiedName(dataRedefinesEntry.RedefinesDataName.URI, isReadDictionary);
            if (searchedDataDefinition == null)
            {
                return null;
            }
            if (searchedDataDefinition is DataDescription)
            {
                return (DataDescriptionEntry)searchedDataDefinition.CodeElement;
            }
            if (searchedDataDefinition is DataRedefines)
            {
                return GetDataDescriptionEntry(node, (DataRedefinesEntry)searchedDataDefinition.CodeElement, isReadDictionary);
            }
            throw new NotImplementedException(searchedDataDefinition.Name);
        }

    }
}
