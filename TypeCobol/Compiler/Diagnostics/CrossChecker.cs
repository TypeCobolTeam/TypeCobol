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
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Diagnostics
{
    public class CrossCompleteChecker : AbstractAstVisitor
    {
        private Node CurrentNode { get; set; }

        public override bool BeginNode(Node node)
        {
            CurrentNode = node;

            CodeElement codeElement = node.CodeElement;
            if (codeElement?.StorageAreaReads != null)
            {
                foreach (var storageAreaRead in codeElement.StorageAreaReads)
                {
                    CheckVariable(node, storageAreaRead);
                }
            }
            if (codeElement?.StorageAreaWrites != null)
            {
                foreach (var storageAreaWrite in codeElement.StorageAreaWrites)
                {
                    CheckVariable(node, storageAreaWrite.StorageArea);
                }
            }

            RedefinesChecker.OnNode(node);
            FunctionDeclarationChecker.OnNode(node);
            FunctionCallChecker.OnNode(node);
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

            //For MoveCorrespondingStatement check children compatibility
            var FromVariable = move.SymbolTable.GetVariables(moveCorresponding.FromGroupItem); //Left member of the move corr statement
            var ToVariable = move.SymbolTable.GetVariables(moveCorresponding.ToGroupItem); //Right member of the move corr statement

            if ((FromVariable != null && FromVariable.Count() != 1) || (ToVariable != null && ToVariable.Count() != 1))
                return true; //Do not continue, the variables hasn't been found. An error will be raised later by CheckVariable()

            var fromVariableChildren = FromVariable.First().Children.Where(c => c?.Name != null);
            var toVariableChildren = ToVariable.First().Children.Where(c => c?.Name != null);

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

        /// <summary>
        /// Determines if the given usage is an ElementaryItem usage
        /// </summary>
        /// <param name="usage"></param>
        /// <returns>true if the usage is ElementaryItem</returns>
        public static bool CanBeElementaryItemUsage(DataUsage usage)
        {
            switch (usage)
            {
                case DataUsage.None:
                    return false;
                /// <summary>
                /// p230: BINARY
                /// p231: COMPUTATIONAL or COMP (binary)
                /// p231: COMPUTATIONAL-4 or COMP-4 (binary)
                /// </summary>
                case DataUsage.Binary:
                    return true;
                /// <summary>
                /// p231: COMPUTATIONAL-5 or COMP-5 (native binary)
                /// </summary>
                case DataUsage.NativeBinary:
                    return true;
                /// <summary>
                /// p231: PACKED-DECIMAL
                /// p231: COMPUTATIONAL-3 or COMP-3 (internal decimal)
                /// </summary>
                case DataUsage.PackedDecimal:
                    return true;
                /// <summary>
                //// p231: COMPUTATIONAL-1 or COMP-1 (floating-point)
                /// </summary>
                case DataUsage.FloatingPoint:
                    return true;
                /// <summary>
                /// p231: COMPUTATIONAL-2 or COMP-2 (long floating-point)
                /// </summary>
                case DataUsage.LongFloatingPoint:
                    return true;
                /// <summary>
                /// p232: DISPLAY phrase 
                /// </summary>
                case DataUsage.Display:
                    return false;
                /// <summary>
                /// p233: DISPLAY-1 phrase
                /// </summary>
                case DataUsage.DBCS:
                    return false;
                /// <summary>
                /// p233: FUNCTION-POINTER phrase 
                /// </summary>
                case DataUsage.FunctionPointer:
                    return true;
                /// <summary>
                /// p233: INDEX phrase 
                /// Index data item
                /// An index data item is a data item that can hold the value of an index.
                /// You define an index data item by specifying the USAGE IS INDEX clause in a data
                /// description entry. The name of an index data item is a data-name. An index data
                /// item can be used anywhere a data-name or identifier can be used, unless stated
                /// otherwise in the rules of a particular statement. You can use the SET statement to
                /// save the value of an index (referenced by index-name) in an index data item.
                /// </summary>
                case DataUsage.Index:
                    return false;
                /// <summary>
                /// p234: NATIONAL phrase
                /// </summary>
                case DataUsage.National:
                    return false;
                /// <summary>
                /// p234: OBJECT REFERENCE phrase 
                /// </summary>
                case DataUsage.ObjectReference:
                    return true;
                /// <summary>
                /// p235: POINTER phrase
                /// </summary>
                case DataUsage.Pointer:
                    return true;
                /// <summary>
                /// p236: PROCEDURE-POINTER phrase 
                /// </summary>
                case DataUsage.ProcedurePointer:
                    return true;
                default:
                    return false;
            }
        }

        public override bool Visit(DataDefinition dataDefinition)
        {
            CommonDataDescriptionAndDataRedefines commonDataDataDefinitionCodeElement =
                dataDefinition.CodeElement as CommonDataDescriptionAndDataRedefines;
            if (commonDataDataDefinitionCodeElement!=null)
            {
                CheckPicture(dataDefinition);
            }

            //Check if DataDefinition is level 88 and declared under BOOL variable
            if (!(dataDefinition.CodeElement is DataDefinitionEntry)) return true;

            var levelNumber = ((DataDefinitionEntry)dataDefinition.CodeElement).LevelNumber;
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
            //Level 88 and 66 cannot have Children.
            if (levelNumber != null && (levelNumber.Value == 88 || levelNumber.Value == 66) && dataDefinition.ChildrenCount != 0)
            {
                DiagnosticUtils.AddError(dataDefinition.CodeElement,
                    "The variable '" + dataDefinition.Name + "' with level 88 and 66 cannot be group item.");
            }
            if (dataDefinition.Usage != null)
            {
                if (levelNumber != null && (levelNumber.Value == 88 || levelNumber.Value == 66))
                {//page 229 : Level 88 and 66 cannot have use
                    DiagnosticUtils.AddError(dataDefinition.CodeElement,
                        "The variable '" + dataDefinition.Name + "' with level 88 and 66 cannot have USAGE.");
                }
            }
            if (dataDefinition.Picture != null)
            {//only children with level 77 or 88 can be children of a PICTURE Elementary Item
                if (dataDefinition.Children.Any(elem => elem.CodeElement != null && 
                                                        elem.CodeElement.Type != CodeElementType.DataConditionEntry &&
                                                        elem.CodeElement.Type != CodeElementType.DataRenamesEntry))
                {
                    DiagnosticUtils.AddError(dataDefinition,
                              "Group item " + dataDefinition.Name +
                                  " contained the \"PICTURE\" clause.");
                }          
            }

            if (dataDefinition.Picture == null && dataDefinition.Usage != null && dataDefinition.ChildrenCount > 0)
            {   //This DataDefinition Has no PICTURE but has an USAGE and Children : page 230
                if (CanBeElementaryItemUsage(dataDefinition.Usage.Value))
                {
                    
                    if (dataDefinition.Usage.Value != DataUsage.Binary &&
                        dataDefinition.Usage.Value != DataUsage.NativeBinary &&
                        dataDefinition.Usage.Value != DataUsage.PackedDecimal &&
                        dataDefinition.Usage.Value != DataUsage.FloatingPoint &&
                        dataDefinition.Usage.Value != DataUsage.LongFloatingPoint)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "Elementary item USAGE " + dataDefinition.Name +
                              " seen has Group Item.");
                    }
                }
            }

            //Type definitions are considered UserDefinedDataType; 
            //Types inside a TypeDef are not checked as they may occur as children
            if (commonDataDataDefinitionCodeElement?.UserDefinedDataType != null && dataDefinition.IsPartOfATypeDef == false && dataDefinition.ChildrenCount > 0 )
            {
                if (dataDefinition.Children.Any(elem => elem.CodeElement != null && 
                                                        elem.CodeElement.Type != CodeElementType.DataConditionEntry &&
                                                        elem.CodeElement.Type != CodeElementType.DataRenamesEntry))
                {
                    //can be type!;
                    DiagnosticUtils.AddError(dataDefinition,
                              "Item " + dataDefinition.Name +
                                " is a TYPE that does not allow Group Item definition.");      
                }
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

        public static DataDefinition CheckVariable(Node node, StorageArea storageArea)
        {
            if (storageArea == null || !storageArea.NeedDeclaration)
                return null;

            var area = storageArea.GetStorageAreaThatNeedDeclaration;
            IEnumerable<DataDefinition> found;
            var foundQualified = new List<KeyValuePair<string, DataDefinition>>();

            if (area.SymbolReference == null) return null;
            //Do not handle TCFunctionName, it'll be done by TypeCobolChecker
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName)) return null;

            var isPartOfTypeDef = (node as DataDefinition) != null && ((DataDefinition) node).IsPartOfATypeDef;
            foundQualified =
                node.SymbolTable.GetVariablesExplicitWithQualifiedName(area.SymbolReference != null
                    ? area.SymbolReference.URI
                    : new URI(area.ToString()),
                    isPartOfTypeDef ? ((DataDefinition) node).GetParentTypeDefinition
                    :null);
            found = foundQualified.Select(v => v.Value);

            if (found.Count() == 1 && foundQualified.Count == 1)
            {
                if (found.First().IsIndex)
                {
                    var index = found.First();
                    string completeQualifiedName = foundQualified.First().Key;

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
                else if (found.First().DataType == DataType.Boolean && found.First().CodeElement is DataDefinitionEntry &&
                         ((DataDefinitionEntry) found.First()?.CodeElement)?.LevelNumber?.Value != 88)
                {
                    if (!((node is Nodes.If && storageArea.Kind != StorageAreaKind.StorageAreaPropertySpecialRegister) || node is Nodes.Set || node is Nodes.Perform || node is Nodes.WhenSearch || node is Nodes.When))//Ignore If/Set/Perform/WhenSearch Statement
                    {
                        //Flag node has using a boolean variable + Add storage area into qualifiedStorageArea of the node. (Used in CodeGen)
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsBoolean, node, storageArea,
                            foundQualified.First().Key);
                    }
                }
                else if (found.First().Usage == DataUsage.Pointer && found.First().CodeElement is DataDefinitionEntry)
                {
                    if (node.CodeElement is SetStatementForIndexes && !node.IsFlagSet(Node.Flag.NodeContainsPointer))
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsPointer, node, storageArea,
                            foundQualified.First().Key);
                        var receivers = node["receivers"] as List<DataDefinition>;
                        int intSender;
                        if (!Int32.TryParse(node["sender"].ToString(), out intSender))
                        {
                            if (!node.SymbolTable.DataEntries.Any(
                                x => x.Key == node["sender"].ToString() &&
                                     x.Value.First().DataType.Name == "Numeric"))
                                DiagnosticUtils.AddError(node, "Increment only support integer values");
                        }
                        foreach (var receiver in receivers)
                        {
                            if (receiver.Usage != DataUsage.Pointer)
                                DiagnosticUtils.AddError(node, "[Set [pointer1, pointer2 ...] UP|DOWN BY n] only support pointers.");
                            
                            if (((DataDefinitionEntry)receiver.CodeElement).LevelNumber.Value > 49)
                                DiagnosticUtils.AddError(node, "Only pointer declared in level 01 to 49 can be use in instructions SET UP BY and SET DOWN BY.");
                        }
                    }
                }

                var specialRegister = storageArea as StorageAreaPropertySpecialRegister;
                if (specialRegister != null 
                    && specialRegister.SpecialRegisterName.TokenType == TokenType.ADDRESS 
                    && specialRegister.IsWrittenTo 
                    && !(node is ProcedureStyleCall))
                {
                    var variabletoCheck = found.First();
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
                        var variabletoCheck = found.First();
                        //This variable has to be in Linkage Section
                        if (!variabletoCheck.IsFlagSet(Node.Flag.LinkageSectionNode) &&
                            currentCheckedParameter.SharingMode.Value == ParameterSharingMode.ByReference)
                            DiagnosticUtils.AddError(node,
                                "CALL with ADDRESS OF can only be used with a LINKAGE variable, or with a sharing mode BY CONTENT/BY VALUE");
                    }
                }

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
                return found.First();


            return null;
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
            var performCE = (PerformProcedureStatement) perform.CodeElement;
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
            var wsymbol = CrossCompleteChecker.CheckVariable(node, wname);
            if (wsymbol != null)
                receivingTypeDefinition = wsymbol.TypeDefinition ?? GetDataDefinitionType(node.SymbolTable, wsymbol);

            if (sent is QualifiedName)
            {
                var sname = sent as QualifiedName;
                var ssymbol = GetSymbol(node.SymbolTable, sname);
                if (ssymbol == null) return; // sending symbol name unresolved
                sendingTypeDefinition = ssymbol.TypeDefinition ?? GetDataDefinitionType(node.SymbolTable, ssymbol);
            }
            else if (sent is StorageArea)
            {
                var rsymbol = CrossCompleteChecker.CheckVariable(node, (StorageArea) sent);
                if (rsymbol != null)
                    sendingTypeDefinition = rsymbol.TypeDefinition ?? GetDataDefinitionType(node.SymbolTable, rsymbol);
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

        private static DataDefinition GetSymbol(SymbolTable table, SymbolReference symbolReference)
        {
            var found = table.GetVariables(symbolReference);
            if (found.Count() != 1) return null; // symbol undeclared or ambiguous -> not my job
            return found.First();
        }

        private static DataDefinition GetSymbol(SymbolTable table, QualifiedName qualifiedName)
        {
            var found = table.GetVariablesExplicit(qualifiedName);
            if (found.Count() != 1) return null; // symbol undeclared or ambiguous -> not my job
            return found.First();
        }

        //TODO move this method to DataDefinition
        /// <summary>
        /// Allows to get DataType of a DataDefinition Node
        /// </summary>
        /// <param name="table"></param>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private static DataDefinition GetDataDefinitionType(SymbolTable table, Node symbol)
        {
            var data = symbol as DataDefinition;
            if (data != null)
            {
                var dataCondition = data as DataCondition;
                if (dataCondition != null)
                    return new GeneratedDefinition(dataCondition.CodeElement().DataType.Name,
                        dataCondition.CodeElement().DataType);

                DataDescriptionEntry entry;
                if (data.CodeElement is DataDescriptionEntry)
                {
                    entry = (DataDescriptionEntry) data.CodeElement;
                }
                else if (data.CodeElement is DataRedefinesEntry)
                {
                    var redefines = (DataRedefinesEntry) data.CodeElement;
                    var node = GetSymbol(table, redefines.RedefinesDataName);
                    if (node is DataDescription)
                    {
                        entry = (DataDescriptionEntry) node.CodeElement;
                    }
                    else
                    {
                        entry = GetDataDescriptionEntry(table, redefines);
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
            var types = table.GetType(typed);
            if (types.Count != 1) return null; // symbol type not found or ambiguous
            return types[0];
        }

        /// <summary>
        /// Quick and dirty method, this checker need to be refactored
        /// </summary>
        /// <param name="table"></param>
        /// <param name="dataRedefinesEntry"></param>
        /// <returns></returns>
        private static DataDescriptionEntry GetDataDescriptionEntry(SymbolTable table,
            DataRedefinesEntry dataRedefinesEntry)
        {
            var node = GetSymbol(table, dataRedefinesEntry.RedefinesDataName);
            if (node == null)
            {
                return null;
            }
            if (node is DataDescription)
            {
                return (DataDescriptionEntry) node.CodeElement;
            }
            if (node is DataRedefines)
            {
                return GetDataDescriptionEntry(table, (DataRedefinesEntry) node.CodeElement);
            }
            throw new NotImplementedException(node.Name);
        }

    }
}
