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
            //from CodeElement StorageAreaReads and StorageAreaWrites
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

            FunctionCallChecker.OnNode(node);
            TypedDeclarationChecker.OnNode(node);
            RenamesChecker.OnNode(node);
            ReadOnlyPropertiesChecker.OnNode(node);
            GlobalStorageSectionChecker.OnNode(node);

            return true;
        }

        public override bool BeginCodeElement(CodeElement codeElement)
        {
            //This checker is only for Node after the full AST has been created
            return false;
        }

        public override bool Visit(FunctionDeclaration functionDeclaration)
        {
            FunctionDeclarationChecker.OnNode(functionDeclaration);
            CheckMultipleFormComParam(functionDeclaration.CodeElement);
            return true;
        }

        public override bool Visit(DataRedefines dataRedefines)
        {
            RedefinesChecker.OnNode(dataRedefines);
            return true;
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
            ProcedureDivisionHeader ce = procedureDivision.CodeElement as ProcedureDivisionHeader;
            if (ce?.FormalizedCommentDocumentation != null && procedureDivision.Parent is FunctionDeclaration)
            {
                DiagnosticUtils.AddError(ce,
                    "Formalized Comments can be placed above Procedure Division only for Programs",
                    MessageCode.ErrorFormalizedCommentMissplaced);
            }
            return true;
        }

        public override bool Visit(Section section)
        {
            SectionOrParagraphUsageChecker.CheckSection(section);
            return true;
        }

        public override bool Visit(Set setStatement)
        {
            SetStatementChecker.CheckStatement(setStatement);
            return true;
        }

        public override bool Visit(Move move)
        {
            var moveCorresponding = move?.CodeElement as MoveCorrespondingStatement;
            var moveSimple = move?.CodeElement as MoveSimpleStatement;

            if (moveCorresponding != null)
            {
                DataDefinition fromVariable = null;
                DataDefinition toVariable = null;
                //For MoveCorrespondingStatement check children compatibility
                fromVariable = move.GetDataDefinitionFromStorageAreaDictionary(moveCorresponding.FromGroupItem, true);
                toVariable = move.GetDataDefinitionFromStorageAreaDictionary(moveCorresponding.ToGroupItem, false);
                

                if (fromVariable == null || toVariable == null)
                {
                    return
                        true; //Do not continue, the variables hasn't been found. An error will be raised later by CheckVariable()
                }
                var fromVariableChildren = fromVariable.Children.Where(c => c?.Name != null);
                var toVariableChildren = toVariable.Children.Where(c => c?.Name != null);

                var matchingChildrenNames = fromVariableChildren.Select(c => c.Name.ToLowerInvariant())
                    .Intersect(toVariableChildren.Select(c => c.Name.ToLowerInvariant()));

                foreach (var matchingChildName in matchingChildrenNames)
                {
                    var retrievedChildrenFrom =
                        fromVariableChildren.Where(c => c.Name.ToLowerInvariant() == matchingChildName);
                    var retrievedChildrenTo =
                        toVariableChildren.Where(c => c.Name.ToLowerInvariant() == matchingChildName);

                    if (retrievedChildrenFrom.Count() != 1 || retrievedChildrenTo.Count() != 1)
                        DiagnosticUtils.AddError(move,
                            string.Format("Multiple symbol \"{0}\" detected in MOVE CORR", matchingChildName));

                    var retrievedChildFrom = (retrievedChildrenFrom.First() as DataDefinition);
                    var retrievedChildTo = (retrievedChildrenTo.First() as DataDefinition);

                    if (retrievedChildFrom == null || retrievedChildTo == null)
                        continue; //Doesn't have to happen but in case...

                    var fromDataType = retrievedChildFrom.DataType;
                    var toDataType = retrievedChildTo.DataType;

                    if (fromDataType != toDataType && fromDataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85 &&
                        toDataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85) //Check DataType matching
                        DiagnosticUtils.AddError(move,
                            string.Format("Symbol {0} of type {1} do not match symbol {2} of type {3}",
                                retrievedChildFrom.VisualQualifiedName, fromDataType,
                                retrievedChildTo.VisualQualifiedName, toDataType));
                }

            }
            else
            {
                if (moveSimple?.StorageAreaWrites == null)
                {
                    return true;
                }
                foreach (var area in moveSimple.StorageAreaWrites)
                {
                    var receiver = area.StorageArea;
                    if (receiver is FunctionCallResult)
                        DiagnosticUtils.AddError(move, "MOVE: illegal <function call> after TO");
                }
            }


            return true;
        }

        public override bool Visit(Evaluate evaluate)
        {
            if(evaluate.GetChildren<WhenOther>().Count == 0)
                DiagnosticUtils.AddError(evaluate,
                    "\"when other\" is missing", MessageCode.Warning);
            return true; 
        }

        public override bool Visit(If ifNode)
        {
            if(ifNode?.Children != null && !(ifNode.Children.Last() is End))
            {
                DiagnosticUtils.AddError(ifNode,
                    "\"end-if\" is missing", MessageCode.Warning);
            }
            return true;
        }

        public override bool Visit(TypeDefinition typeDefinition)
        {
            //Cobol 2002 rule
            //TODO need to clarify if we have 1 visitor per LanguageLevel
            //For performance reason it seems better to have only one here
            TypeDefinitionChecker.CheckTypeDefinition(typeDefinition);
            CheckMultipleFormComParam(typeDefinition.CodeElement);
            return true;
        }

        public override bool Visit(Program program)
        {
            ProgramChecker.OnNode(program);

            //// Set a Warning if the FormCom parameter in unknown or if the program parameter have no description

            ProcedureDivisionHeader procedureDivision = program.Children.FirstOrDefault(c => c is ProcedureDivision)?.CodeElement as ProcedureDivisionHeader;
            var formCom = procedureDivision?.FormalizedCommentDocumentation;

            if (formCom != null && procedureDivision.UsingParameters!= null)
            {
                CheckMultipleFormComParam(procedureDivision);
                // Get the parameters inside the Formalized Comment that are not inside the program parameters
                var formComParamOrphan = formCom.Parameters.Keys.Except(
                    procedureDivision.UsingParameters.Select(p => p.StorageArea.SymbolReference?.Name)) ?? Enumerable.Empty<string>();

                // For each of them, place a warning on the orphan parameter definition (UserDefinedWord Token inside the FormCom)
                foreach (var orphan in formComParamOrphan)
                {
                    var tokens =
                        procedureDivision.ConsumedTokens.Where(t => t.TokenType == TokenType.UserDefinedWord && t.Text == orphan);
                    foreach (var token in tokens)
                    {
                        DiagnosticUtils.AddError(procedureDivision,
                            "Parameter name does not match to any program parameter: " + orphan,
                            token, code: MessageCode.Warning);
                    }
                }

                
                // Get the parameters inside the program parameters that are not inside the Formalized Comment
                var sameParameters = procedureDivision.UsingParameters.Where(p =>
                    formCom.Parameters.Keys.Contains(p.StorageArea.SymbolReference?.Name));

                var programParamWithoutDesc = procedureDivision.UsingParameters.Except(sameParameters);

                // For each of them, place a warning on the parameter definition
                foreach (var param in programParamWithoutDesc)
                {
                    var tokens = procedureDivision.ConsumedTokens.Where(t =>
                        t.TokenType == TokenType.UserDefinedWord &&
                        t.Text == param.StorageArea.SymbolReference?.Name);
                    foreach (var token in tokens)
                    {
                        DiagnosticUtils.AddError(procedureDivision,
                            "Parameter does not have any description inside the formalized comments: " + param.StorageArea.SymbolReference?.Name,
                            token, code: MessageCode.Warning);
                    }
                }
            }

            return true;
        }

        public override bool VisitVariableWriter(VariableWriter variableWriter)
        {
            WriteTypeConsistencyChecker.OnNode(variableWriter, CurrentNode);
            return true;
        }

        public override bool Visit(DataDefinition dataDefinition)
        {
            var commonDataDataDefinitionCodeElement =
                dataDefinition.CodeElement as CommonDataDescriptionAndDataRedefines;

            DataDefinitionEntry dataDefinitionEntry = dataDefinition.CodeElement;
            
            if (dataDefinitionEntry == null) return true;

            var levelNumber = dataDefinitionEntry.LevelNumber;
            if (levelNumber != null)
            {
                var dataDefinitionParent = (dataDefinition.Parent as DataDefinition);
                var levelNumberValue = levelNumber.Value;
                if (dataDefinitionParent != null)
                {
                    //Check if DataDefinition is level 88 and declared under a Type BOOL variable
                    if (dataDefinitionParent.DataType == DataType.Boolean && levelNumberValue == 88)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The Level 88 symbol '" + dataDefinition.Name + "' cannot be declared under a BOOL typed symbol");
                    }
                }
                else
                {
                    //Parent is not a DataDefinition so it's a top level data definition under a section (eg working-storage)
                    //These top level DataDefinition can only be level 01 or 77
                    if (!(levelNumberValue == 01 || levelNumberValue == 77))
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The variable '" + dataDefinition.Name + "' can only be of level 01 or 77", dataDefinitionEntry);
                    }
                }

                //Level 88 and 66 cannot have Children.
                if ((levelNumberValue == 88 || levelNumberValue == 66))
                {
                    if (dataDefinition.ChildrenCount != 0)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The variable '" + dataDefinition.Name + "' with level 88 and 66 cannot be group item.", dataDefinitionEntry);
                    }

                    if (dataDefinition.Usage != null)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The variable '" + dataDefinition.Name + "' with level 88 and 66 cannot have USAGE.", dataDefinitionEntry);
                    }
                }
            }


            if (HasChildrenThatDeclareData(dataDefinition))
            {
                if (dataDefinition.Picture != null)
                {
                    DiagnosticUtils.AddError(dataDefinition,
                        "Group item " + dataDefinition.Name + " cannot have a \"PICTURE\"", dataDefinitionEntry);
                }

                if (commonDataDataDefinitionCodeElement?.UserDefinedDataType != null)
                {
                    DiagnosticUtils.AddError(dataDefinition,
                        "Group item  " + dataDefinition.Name + " cannot have a \"TYPE\"", dataDefinitionEntry);
                }

                if (commonDataDataDefinitionCodeElement?.IsBlankWhenZero?.Value == true)
                {
                    DiagnosticUtils.AddError(dataDefinition,
                        "Group itm " + dataDefinition.Name + " cannot have \"Blank when zero\" clause", dataDefinitionEntry);
                }

                return true;
            }

            DataDefinitionChecker.OnNode(dataDefinition);

            return true;
        }

        /// <summary>
        /// Test if the received DataDefinition has other children than DataConditionEntry or DataRenamesEntry
        /// </summary>
        /// <param name="dataDefinition">Item to check</param>
        /// <returns>True if there are only DataConditionEntry or DataRenamesEntry childrens</returns>
        private static bool HasChildrenThatDeclareData(DataDefinition dataDefinition)
        {
            return dataDefinition.Children.Any(elem=>elem.CodeElement != null && 
                                                     elem.CodeElement.Type != CodeElementType.DataConditionEntry && 
                                                     elem.CodeElement.Type != CodeElementType.DataRenamesEntry);
        }

        public override bool Visit(IndexDefinition indexDefinition)
        {
            var found =
                indexDefinition.SymbolTable.GetVariablesExplicit(new URI(indexDefinition.Name))
                    .Where(i => i.ParentTypeDefinition == null)
                    .ToList();
            if (indexDefinition.ParentTypeDefinition != null) return true;
            if (found.Count > 1) //If multiple index with same name found, display a warning.
            {
                DiagnosticUtils.AddError(indexDefinition.Parent,
                    "An index named '" + indexDefinition.Name + "' is already defined.", MessageCode.Warning);
            }
            return true;
        }

        public static DataDefinition CheckVariable(Node node, StorageArea storageArea, bool isReadStorageArea)
        {
            if (storageArea == null || !storageArea.NeedDeclaration)
                return null;

            var area = storageArea.GetStorageAreaThatNeedDeclaration;
            if (area.SymbolReference == null) return null;
            //Do not handle TCFunctionName, it'll be done by TypeCobolChecker
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName)) return null;

            var parentTypeDefinition = (node as DataDefinition)?.ParentTypeDefinition;
            var foundQualified =
                node.SymbolTable.GetVariablesExplicitWithQualifiedName(area.SymbolReference != null
                        ? area.SymbolReference.URI
                        : new URI(area.ToString()),
                    parentTypeDefinition);
            var found = foundQualified.Select(v => v.Value);

            var foundCount = found.Count();

            if (foundCount == 0)
            {
                if (node.SymbolTable.GetFunction(area).Count < 1)
                    DiagnosticUtils.AddError(node, "Symbol " + area + " is not referenced", area.SymbolReference);
            }
            else if (foundCount > 1)
            {
                bool isFirst = true;
                string errorMessage = "Ambiguous reference to symbol " + area + " " + Environment.NewLine +
                                      "Symbols found: ";
                foreach (var symbol in foundQualified)
                {
                    // Multiline Version
                    //errorMessage += Environment.NewLine + "\t" + symbol.Key.Replace(".", "::");
                    // Inline version
                    //                                        if qualified name list is not null, create a string          otherwise is the qualified name of the DataDefinition
                    errorMessage += (isFirst ? "" : " | ") + (symbol.Key != null ? symbol.Key.ToString().Replace(".", "::") : symbol.Value.QualifiedName.ToString().Replace(".", "::"));
                    isFirst = false;
                }
                DiagnosticUtils.AddError(node, errorMessage, area.SymbolReference);
            }
            else if (foundCount == 1)
            {
                var dataDefinitionFound = found.First();
                var dataDefinitionPath = foundQualified.First().Key;

                if (foundQualified.Count == 1)
                {
                    IndexAndFlagDataDefiniton(dataDefinitionPath, dataDefinitionFound, node, area, storageArea);
                }
                //add the found DataDefinition to a dictionary depending on the storage area type
                if (isReadStorageArea)
                {
                    //need to initialize the dictionaries
                    if (node.StorageAreaReadsDataDefinition == null)
                    {
                        node.StorageAreaReadsDataDefinition = new Dictionary<StorageArea, DataDefinition>();
                    }
                    node.StorageAreaReadsDataDefinition.Add(storageArea,dataDefinitionFound);
                }
                else
                {
                    //need to initialize the dictionaries
                    if (node.StorageAreaWritesDataDefinition == null)
                    {
                        node.StorageAreaWritesDataDefinition = new Dictionary<StorageArea, DataDefinition>();
                    }
                    node.StorageAreaWritesDataDefinition.Add(storageArea,dataDefinitionFound);
                }

                return dataDefinitionFound;
            }

            return null;
        }

        private static void IndexAndFlagDataDefiniton(DataDefinitionPath dataDefinitionPath, DataDefinition dataDefinition,
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
                            "Index name '" + index.Name + "' is over 22 characters.", area.SymbolReference);
                    if (
                            index.Parent.CodeElement.IsInsideCopy())
                        //If index comes from a copy, do not support qualification
                        DiagnosticUtils.AddError(node,
                            "Index '" + index.Name + "' inside a COPY cannot be use with qualified symbol", area.SymbolReference);
                }

                if (area.SymbolReference.IsQualifiedReference || index.IsPartOfATypeDef)
                //Index name is qualified or belongs to a typedef
                {
                    //Mark this node for generator
                    FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, node, storageArea, dataDefinitionPath);

                    foreach (var reference in index.GetReferences())
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, reference.Value,
                            reference.Key, dataDefinitionPath);
                    }
                }
                else if (!area.SymbolReference.IsQualifiedReference)
                //If it's an index but not use with qualified reference 
                {
                    //If the index has already been flaged UsedWithQualifiedName, we need to flag the current node
                    if(index.IsFlagSet(Node.Flag.IndexUsedWithQualifiedName))
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, node, storageArea,
                            dataDefinitionPath);
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
                            "Index can not be use with OF or IN qualifiers " + area, area.SymbolReference);
                }
                else if (dataDefinition.DataType == DataType.Boolean)
                {
                    if (!((node is Nodes.If && storageArea.Kind != StorageAreaKind.StorageAreaPropertySpecialRegister) || node is Nodes.Set || node is Nodes.Perform || node is Nodes.PerformProcedure || node is Nodes.WhenSearch || node is Nodes.When ) || storageArea.Kind == StorageAreaKind.StorageAreaPropertySpecialRegister)//Ignore If/Set/Perform/WhenSearch Statement
                    {
                        //Flag node has using a boolean variable + Add storage area into qualifiedStorageArea of the node. (Used in CodeGen)
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsBoolean, node, storageArea,
                            dataDefinitionPath);
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
                        " is declared out of LINKAGE SECTION.", area.SymbolReference);
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

        /// <summary>
        /// Add a warning if a Field is set more than one time
        /// </summary>
        private static void CheckMultipleFormComParam(CodeElement codeElement)
        {
            var tokenGroups = codeElement.ConsumedTokens.GroupBy(t => t.TokenType);
            foreach (var tokenGroup in tokenGroups)
            {
                if ((int)tokenGroup.Key >= 513 && (int)tokenGroup.Key <= 520 && tokenGroup.Count() > 1)
                {
                    foreach (var token in tokenGroup)
                    {
                        DiagnosticUtils.AddError(codeElement,
                            "Formalized comment field is declared more than once : " + token.Text,
                            token, code: MessageCode.Warning);
                    }
                }
            }
        }

        private static void FlagNodeAndCreateQualifiedStorageAreas(Node.Flag flag, Node node, StorageArea storageArea,
            DataDefinitionPath dataDefinitionPath)
        {
            node.SetFlag(flag, true);
            if (node.QualifiedStorageAreas == null)
                node.QualifiedStorageAreas = new Dictionary<StorageArea, DataDefinitionPath>();

            if (!node.QualifiedStorageAreas.ContainsKey(storageArea))
                node.QualifiedStorageAreas.Add(storageArea, dataDefinitionPath);
        }
    }
    
    class SectionOrParagraphUsageChecker
    {
        public static void CheckReferenceToParagraphOrSection(PerformProcedure perform)
        {
            var performCE = perform.CodeElement;
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
                    DiagnosticUtils.AddError(node, "Symbol " + symbol.Name + " is not referenced", symbol);
                }
                else
                {
                    if (sfound.Count > 1)
                        DiagnosticUtils.AddError(node, "Ambiguous reference to section " + symbol.Name, symbol);
                    return sname;
                }
            }
            else
            {
                if (sname == null)
                {
                    if (pfound.Count > 1)
                        DiagnosticUtils.AddError(node, "Ambiguous reference to paragraph " + symbol.Name, symbol);
                    return pname;
                }
                else
                {
                    DiagnosticUtils.AddError(node, "Ambiguous reference to procedure " + symbol.Name, symbol);
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
            DataDefinition searchExistingDataDefinition;
            DataDefinition wsymbol = null;
            //check if dico not null
            if (node.StorageAreaWritesDataDefinition != null)
            {
                node.StorageAreaWritesDataDefinition.TryGetValue(wname, out searchExistingDataDefinition);
                wsymbol = searchExistingDataDefinition;
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
                    rsymbol = searchExistingDataDefinition;
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

                        DiagnosticUtils.AddError(node, message, wname.SymbolReference, code: MessageCode.SemanticTCErrorInParser);
                    }
                }
                else
                {
                    if (isUnsafe)
                    {
                        var message = "Useless UNSAFE with non strongly typed receiver.";
                        DiagnosticUtils.AddError(node, message, code: MessageCode.SyntaxWarningInParser);
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
                    return new GeneratedDefinition(dataCondition.CodeElement.DataType.Name,
                        dataCondition.CodeElement.DataType);

                DataDescriptionEntry entry;
                var descriptionEntry = data.CodeElement as DataDescriptionEntry;
                if (descriptionEntry != null)
                {
                    entry = descriptionEntry;
                }
                else if (data.CodeElement is DataRedefinesEntry)
                {
                    var redefines = (DataRedefinesEntry) data.CodeElement;
                    var searchedDataDefinition = node.GetDataDefinitionForQualifiedName(redefines.RedefinesDataName.URI, isReadDictionary);
                    if (searchedDataDefinition is DataDescription)
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
            } else
            {
                return null;
            }

            if (data?.TypeDefinition != null)
                return data.TypeDefinition;

            var types = node.SymbolTable.GetType(data);
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
