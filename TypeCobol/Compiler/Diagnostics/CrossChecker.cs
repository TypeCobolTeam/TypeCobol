using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using System.Text.RegularExpressions;

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
                indexDefinition.SymbolTable.GetVariables(new URI(indexDefinition.Name))
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

        private void CheckVariable(Node node, StorageArea storageArea)
        {
            if (storageArea == null || !storageArea.NeedDeclaration)
                return;

            var area = storageArea.GetStorageAreaThatNeedDeclaration;
            List<DataDefinition> found;
            var foundQualified = new List<KeyValuePair<string, DataDefinition>>();

            if (area.SymbolReference == null) return;
            //Do not handle TCFunctionName, it'll be done by TypeCobolChecker
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName)) return;

            var isPartOfTypeDef = (node as DataDefinition) != null && ((DataDefinition) node).IsPartOfATypeDef;
            if (isPartOfTypeDef)
                found = node.SymbolTable.GetVariables(area, ((DataDefinition) node).GetParentTypeDefinition);
            else
            {
                foundQualified =
                    node.SymbolTable.GetVariablesExplicitWithQualifiedName(area.SymbolReference != null
                        ? area.SymbolReference.URI
                        : new URI(area.ToString()));
                found = foundQualified.Select(v => v.Value).ToList();
            }

            if (found.Count == 1 && foundQualified.Count == 1)
            {
                if (found[0].IsIndex)
                {
                    var index = found[0];
                    string completeQualifiedName = foundQualified.First().Key;

                    index.AddReferences(storageArea, node); //Add this node as a reference to the founded index

                    if (area.SymbolReference.IsQualifiedReference)
                    {
                        if (index.Name.Length > 22) //If index name is used with qualification and exceed 22 characters
                            DiagnosticUtils.AddError(index.Parent.CodeElement,
                                "Index name '" + index.Name + "' is over 22 characters.");
                        if (
                                index.Parent.CodeElement.IsInsideCopy())
                            //If index comes from a copy, do not support qualification
                            DiagnosticUtils.AddError(node.CodeElement,
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
                        DiagnosticUtils.AddError(node.CodeElement,
                            "Index can not be use with OF or IN qualifiers " + area);
                }
                else if (found[0].DataType == DataType.Boolean && found[0].CodeElement is DataDefinitionEntry &&
                         ((DataDefinitionEntry) found[0]?.CodeElement)?.LevelNumber?.Value != 88)
                {
                    if (!(node is Nodes.If || node is Nodes.Set)) //Ignore Conditional(If) and Set statement
                    {
                        //Flag node has using a boolean variable + Add storage area into qualifiedStorageArea of the node. (Used in CodeGen)
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsBoolean, node, storageArea,
                            foundQualified.First().Key);
                    }
                }

            }

            if (found.Count < 1)
                if (node.SymbolTable.GetFunction(area).Count < 1)
                    DiagnosticUtils.AddError(node.CodeElement, "Symbol " + area + " is not referenced");
            if (found.Count > 1)
                DiagnosticUtils.AddError(node.CodeElement, "Ambiguous reference to symbol " + area);

        }

        private void FlagNodeAndCreateQualifiedStorageAreas(Node.Flag flag, Node node, StorageArea storageArea,
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
        private static void CheckVariable(Node node, QualifiedName wname, object sent)
        {
            DataDefinition sendingTypeDefinition = null, receivingTypeDefinition = null;

            if (sent == null || wname == null) return; //Both items needed
            var wsymbol = GetSymbol(node.SymbolTable, wname);
            if (wsymbol == null) return; // receiving symbol name unresolved
            receivingTypeDefinition = wsymbol.TypeDefinition;
            if (receivingTypeDefinition == null) //No TypeDefinition found, try to get DataType
            {
                receivingTypeDefinition = GetDataDefinitionType(node.SymbolTable, wsymbol);
            }

            var sname = sent as QualifiedName;
            if (sname != null)
            {
                var ssymbol = GetSymbol(node.SymbolTable, sname);
                if (ssymbol == null) return; // sending symbol name unresolved
                sendingTypeDefinition = ssymbol.TypeDefinition;
                if (sendingTypeDefinition == null) //No TypeDefinition found try to get DataType
                {
                    sendingTypeDefinition = GetDataDefinitionType(node.SymbolTable, ssymbol);
                }
            }
            else
            {
                //This will resolve the following cases MOVE 1 TO myVar / MOVE true TO myVar / MOVE "test" TO myVar. 
                if (sent is bool?) sendingTypeDefinition = GeneratedDefinition.BooleanGeneratedDefinition;
                if (sent is double?) sendingTypeDefinition = GeneratedDefinition.NumericGeneratedDefinition;
                if (sent is string) sendingTypeDefinition = GeneratedDefinition.AlphanumericGeneratedDefinition;
            }

            //TypeDefinition Comparison
            if (receivingTypeDefinition != null && !receivingTypeDefinition.Equals(sendingTypeDefinition))
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
                                : "strictly", wname.Head, receivingName);

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
            if (found.Count != 1) return null; // symbol undeclared or ambiguous -> not my job
            return found[0];
        }

        private static DataDefinition GetSymbol(SymbolTable table, QualifiedName qualifiedName)
        {
            var found = table.GetVariables(qualifiedName);
            if (found.Count != 1) return null; // symbol undeclared or ambiguous -> not my job
            return found[0];
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
