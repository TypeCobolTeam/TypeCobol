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
using Antlr4.Runtime;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Symbols;
using String = System.String;

namespace TypeCobol.Compiler.Diagnostics
{
    public class CrossCompleteChecker : AbstractAstVisitor
    {
        public CrossCompleteChecker([NotNull]TypeCobolOptions compilerOptions)
        {
            _compilerOptions = compilerOptions;
        }

        private readonly TypeCobolOptions _compilerOptions;
        private Node _currentNodeScope;

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
                    CheckVariable(node, storageAreaRead, true);
                }
            }
            if (codeElement?.StorageAreaWrites != null)
            {
                foreach (var storageAreaWrite in codeElement.StorageAreaWrites)
                {
                    CheckVariable(node, storageAreaWrite.StorageArea, false);
                }
            }
            //Build node StorageAreaWritesDataDefinition and StorageAreaReadsDataDefinition dictionaries
            //for Corresponding instruction from StorageAreaGroupsCorrespondingImpact
            if (codeElement?.StorageAreaGroupsCorrespondingImpact != null)
            {
                CheckVariable(node, codeElement.StorageAreaGroupsCorrespondingImpact.SendingGroupItem, true);
                CheckVariable(node, codeElement.StorageAreaGroupsCorrespondingImpact.ReceivingGroupItem, false);
            }

            return true;
        }


        public override bool Visit(GlobalStorageSection globalStorageSection)
        {
            GlobalStorageSectionChecker.OnNode(globalStorageSection);
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
        public override bool Visit(DataRenames dataRenames)
        {
            RenamesChecker.OnNode(dataRenames);
            return true;
        }

        public override bool Visit(ProcedureStyleCall call)
        {
            FunctionCallChecker.OnNode(call);
            return true;
        }

        public override bool Visit(PerformProcedure performProcedureNode)
        {
            var performCE = performProcedureNode.CodeElement;

            if (performCE.Procedure != null)
            {
                var procedure = SectionOrParagraphUsageChecker.ResolveTargetSectionOrParagraph(performProcedureNode, performCE.Procedure, _currentNodeScope);
                switch (procedure.Item1)
                {
                    case SymbolType.SectionName:
                        performProcedureNode.ProcedureSectionSymbol = (SectionSymbol) procedure.Item2?.SemanticData;
                        break;
                    case SymbolType.ParagraphName:
                        performProcedureNode.ProcedureParagraphSymbol = (ParagraphSymbol) procedure.Item2?.SemanticData;
                        break;
                }
            }

            if (performCE.ThroughProcedure != null)
            {
                var throughProcedure = SectionOrParagraphUsageChecker.ResolveTargetSectionOrParagraph(performProcedureNode, performCE.ThroughProcedure, _currentNodeScope);
                switch (throughProcedure.Item1)
                {
                    case SymbolType.SectionName:
                        performProcedureNode.ThroughProcedureSectionSymbol = (SectionSymbol) throughProcedure.Item2?.SemanticData;
                        break;
                    case SymbolType.ParagraphName:
                        performProcedureNode.ThroughProcedureParagraphSymbol = (ParagraphSymbol) throughProcedure.Item2?.SemanticData;
                        break;
                }
            }

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
            //Save current scope node for paragraph resolution
            _currentNodeScope = procedureDivision;
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
            //Save current scope node for paragraph resolution
            _currentNodeScope = section;
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
            if (evaluate.GetChildren<WhenOther>().Count == 0)
                DiagnosticUtils.AddError(evaluate,
                    "\"when other\" is missing", MessageCode.Warning);
            return true;
        }

        public override bool Visit(If ifNode)
        {
            if (ifNode?.Children != null && !(ifNode.Children.Last() is End))
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

            ProcedureDivisionHeader procedureDivision =
                program.Children.FirstOrDefault(c => c is ProcedureDivision)?.CodeElement as ProcedureDivisionHeader;
            var formCom = procedureDivision?.FormalizedCommentDocumentation;

            if (formCom != null && procedureDivision.UsingParameters != null)
            {
                CheckMultipleFormComParam(procedureDivision);
                // Get the parameters inside the Formalized Comment that are not inside the program parameters
                var formComParamOrphan = formCom.Parameters.Keys.Except(
                                             procedureDivision.UsingParameters.Select(p =>
                                                 p.StorageArea.SymbolReference?.Name)) ?? Enumerable.Empty<string>();

                // For each of them, place a warning on the orphan parameter definition (UserDefinedWord Token inside the FormCom)
                foreach (var orphan in formComParamOrphan)
                {
                    var tokens =
                        procedureDivision.ConsumedTokens.Where(t =>
                            t.TokenType == TokenType.UserDefinedWord && t.Text == orphan);
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
                            "Parameter does not have any description inside the formalized comments: " +
                            param.StorageArea.SymbolReference?.Name,
                            token, code: MessageCode.Warning);
                    }
                }
            }

            return true;
        }

        public override bool VisitVariableWriter(VariableWriter variableWriter)
        {
            WriteTypeConsistencyChecker.OnNode(variableWriter, CurrentNode);
            ReadOnlyPropertiesChecker.OnNode(variableWriter, CurrentNode);
            return true;
        }

        public override bool Visit(DataDefinition dataDefinition)
        {
            TypedDeclarationChecker.OnNode(dataDefinition);

            var commonDataDataDefinitionCodeElement =
                dataDefinition.CodeElement as CommonDataDescriptionAndDataRedefines;
            if (commonDataDataDefinitionCodeElement != null)
            {
                CheckPicture(dataDefinition);
            }


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
                    //Perf note: first compare levelNumberValue because it's faster than DataType
                    if (levelNumberValue == 88 && dataDefinitionParent.DataType == DataType.Boolean)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The Level 88 symbol '" + dataDefinition.Name +
                            "' cannot be declared under a BOOL typed symbol");
                    }
                }
                else
                {
                    //Parent is not a DataDefinition so it's a top level data definition under a section (eg working-storage)
                    //These top level DataDefinition can only be level 01 or 77
                    if (!(levelNumberValue == 01 || levelNumberValue == 77))
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The variable '" + dataDefinition.Name + "' can only be of level 01 or 77",
                            dataDefinitionEntry);
                    }
                }

                //Level 88 and 66 cannot have Children.
                if ((levelNumberValue == 88 || levelNumberValue == 66))
                {
                    if (dataDefinition.ChildrenCount != 0)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The variable '" + dataDefinition.Name + "' with level 88 and 66 cannot be group item.",
                            dataDefinitionEntry);
                    }

                    if (dataDefinition.Usage != null)
                    {
                        DiagnosticUtils.AddError(dataDefinition,
                            "The variable '" + dataDefinition.Name + "' with level 88 and 66 cannot have USAGE.",
                            dataDefinitionEntry);
                    }
                }

            }

            //Check if Strict Typedef declaration uses Sync clause
            if (dataDefinition.SemanticData.HasFlag(Symbol.Flags.InsideTypedef) &&
                dataDefinition.SemanticData.HasFlag(Symbol.Flags.Sync))
            {
                //Typedef instruction => check if it's marked Strict
                if (dataDefinition.SemanticData.Kind == Symbol.Kinds.Typedef && dataDefinition.SemanticData.HasFlag(Symbol.Flags.Strict))
                {
                    DiagnosticUtils.AddError(dataDefinition, $"Cannot declare Type definition {dataDefinition.Name} with Sync clause because it is Strict.");
                }
                //Variable inside Typedef => check if parent typedef is marked Strict
                else if (dataDefinition.SemanticData.NearestParent(Symbol.Kinds.Typedef).HasFlag(Symbol.Flags.Strict))
                {
                    DiagnosticUtils.AddError(dataDefinition, $"{dataDefinition.Name} is part of a declaration using Sync clause in Strict Type definition {dataDefinition.ParentTypeDefinition?.Name}.");
                }

            }
            //Check if variable of user defined Strict Type is declared or has a parent declared with Sync clause (flag is inherited so no need to iterate through parents)
            else if (dataDefinition.SemanticData.HasFlag(Symbol.Flags.HasATypedefType) && 
                dataDefinition.TypeDefinition?.RestrictionLevel == RestrictionLevel.STRICT &&
                dataDefinition.SemanticData.HasFlag(Symbol.Flags.Sync))
            {
                DiagnosticUtils.AddError(dataDefinition, $"{dataDefinition.Name} cannot be declared or have a parent declared with Sync clause because its Type definition {dataDefinition.DataType.Name} is Strict.");
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
                        "Group item " + dataDefinition.Name + " cannot have \"Blank when zero\" clause",
                        dataDefinitionEntry);
                }

                return true;
            }

            DataDefinitionChecker.OnNode(dataDefinition);

            return true;
        }

        public override bool Visit(End end)
        {
            // Check end statement is aligned with the matching opening statement
            if (_compilerOptions.CheckEndAlignment.IsActive && end.CodeElement.Type != CodeElementType.SentenceEnd)
            {
                CodeElement parentCodeElement = end.Parent.CodeElement; ;
                if (parentCodeElement?.IsInsideCopy() == false && end.IsInsideCopy() == false)
                {
                    CheckEndNode(parentCodeElement, end.CodeElement);
                }
            }
            return true;
        }

        public override bool Visit(FunctionEnd functionEnd)
        {
            // Check end statement is aligned with the matching opening statement
            if (_compilerOptions.CheckEndAlignment.IsActive)
            {
                CodeElement parentCodeElement = functionEnd.Parent.CodeElement;
                if (parentCodeElement?.IsInsideCopy() == false && functionEnd.IsInsideCopy() == false)
                {
                    Token openingDeclareToken = parentCodeElement.ConsumedTokens.FirstOrDefault(t => t.TokenType == TokenType.DECLARE);
                    CheckEndNode(openingDeclareToken, functionEnd.CodeElement);
                }
            }
            return true;
        }

        public override bool Visit(DataDescription dataDescription)
        {
            DataDescriptionEntry dataDescriptionEntry = dataDescription.CodeElement;

            //Check if the DataDescription is an empty group
            if (dataDescriptionEntry.LevelNumber != null && dataDescription.IsDataDescriptionGroup && dataDescription.ChildrenCount == 0)
            {
                //Get current node index
                var nodeIndex = dataDescription.Parent.IndexOf(dataDescription);
                //Get sibling nodes
                var siblingNodes = dataDescription.Parent.Children;
                //Check if next node is inside a copy when this isn't the last node
                if (siblingNodes.Count > nodeIndex + 1 && siblingNodes[nodeIndex + 1].IsInsideCopy())
                {
                    //Get next sibling node
                    var nextSibling = siblingNodes[nodeIndex + 1];
                    DiagnosticUtils.AddError(dataDescription, $"Cannot include copy {nextSibling.CodeElement?.FirstCopyDirective.TextName} " +
                                                              $"under level {dataDescriptionEntry.LevelNumber} " +
                                                              $"because copy starts at level {((DataDescription)nextSibling).CodeElement.LevelNumber}.", dataDescriptionEntry);
                }
                //Last node so this is an empty group item
                else
                {
                    DiagnosticUtils.AddError(dataDescription, "A group item cannot be empty.", dataDescriptionEntry);
                }
            }

            return true;
        }

        public override bool Visit(SourceComputer sourceComputer)
        {
            if (sourceComputer.CodeElement.DebuggingMode?.Value == true)
            {
                Token token = sourceComputer.CodeElement.DebuggingMode.Token;
                DiagnosticUtils.AddError(sourceComputer.CodeElement, "Debugging mode is active", token, null, MessageCode.Warning);
            }
            return true;
        }

        /// <summary>
        /// Test if the received DataDefinition has other children than DataConditionEntry or DataRenamesEntry
        /// </summary>
        /// <param name="dataDefinition">Item to check</param>
        /// <returns>True if there are only DataConditionEntry or DataRenamesEntry children</returns>
        private static bool HasChildrenThatDeclareData([NotNull] DataDefinition dataDefinition)
        {
            //We only need to check the last children:
            //DataConditionEntry is a level 88, DataRenamesEntry is level 66 and they cannot have children
            //DataDescription and DataRedefines are level between 1 and 49 inclusive.
            //As the level number drive the positioning of Node inside the Children property DataConditionEntry and DataRenamesEntry will always be
            //positioned before dataDescription.
            if (dataDefinition.ChildrenCount > 0)
            {
                var lastChild = ((DataDefinition) dataDefinition.Children[dataDefinition.ChildrenCount - 1]);

                return lastChild.CodeElement != null
                       && lastChild.CodeElement.Type != CodeElementType.DataConditionEntry
                       && lastChild.CodeElement.Type != CodeElementType.DataRenamesEntry;
            }

            return false;
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
            else if (codeElement.Picture.Value.IndexOf("(", StringComparison.Ordinal) >
                     codeElement.Picture.Value.IndexOf(")", StringComparison.Ordinal) ||
                     codeElement.Picture.Value.LastIndexOf("(", StringComparison.Ordinal) >
                     codeElement.Picture.Value.LastIndexOf(")", StringComparison.Ordinal))
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
                        DiagnosticUtils.AddError(node, m, codeElement);
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
                    DiagnosticUtils.AddError(node, "Symbol " + area + " is not referenced", area.SymbolReference, MessageCode.SemanticTCErrorInParser);
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
                    errorMessage += (isFirst ? "" : " | ") + (symbol.Key != null
                                        ? symbol.Key.ToString().Replace(".", "::")
                                        : symbol.Value.QualifiedName.ToString().Replace(".", "::"));
                    isFirst = false;
                }
                DiagnosticUtils.AddError(node, errorMessage, area.SymbolReference, MessageCode.SemanticTCErrorInParser);
            }
            else if (foundCount == 1)
            {
                var dataDefinitionFound = found.First();
                var dataDefinitionPath = foundQualified.First().Key;

                if (foundQualified.Count == 1)
                {
                    IndexAndFlagDataDefiniton(dataDefinitionPath, dataDefinitionFound, node, area, storageArea);
                }

                if (!node.IsFlagSet(Node.Flag.GlobalStorageSection))
                {
                    if (dataDefinitionFound.IsFlagSet(Node.Flag.GlobalStorageSection) || dataDefinitionPath != null && dataDefinitionPath.CurrentDataDefinition.IsFlagSet(Node.Flag.GlobalStorageSection))
                    {
                        if (node is DataDefinition)
                        {
                            DiagnosticUtils.AddError(node, "A Global-Storage Section variable cannot be referenced in another Data Section", area.SymbolReference);
                        }
                        //We must find the enclosing FunctionDeclaration or Program (if node is outside a function/procedure)
                        node.GetEnclosingProgramOrFunctionNode().SetFlag(Node.Flag.UseGlobalStorage, true);
                    }
                }

                if (!isReadStorageArea && node.SymbolTable.CurrentScope == SymbolTable.Scope.Function)
                {
                    var paramDesc = (dataDefinitionPath?.CurrentDataDefinition ?? dataDefinitionFound) as ParameterDescription;
                    //Check if we're dealing with an input parameter
                    if (paramDesc?.PassingType == ParameterDescription.PassingTypes.Input)
                    {
                        var specialRegister = storageArea as StorageAreaPropertySpecialRegister;
                        //Unless this is a format 5 set statement, we have an error. So we're checking we're not in the following format  :
                        //set (address of)? identifier(pointer) TO (address of)? identifier | NULL
                        if (specialRegister?.SpecialRegisterName.TokenType != TokenType.ADDRESS)
                        {
                            DiagnosticUtils.AddError(node, "Input variable '" + paramDesc.Name + "' is modified by an instruction", area.SymbolReference);
                        }
                    }
                }

                //Initialize node caches for DataDef and Symbol.
                IDictionary<StorageArea, DataDefinition> dataDefinitionStorage;
                IDictionary<StorageArea, VariableSymbol> symbolStorage;
                if (isReadStorageArea)
                {
                    //Initialize reads dictionaries
                    if (node.StorageAreaReadsDataDefinition == null)
                        node.StorageAreaReadsDataDefinition = new Dictionary<StorageArea, DataDefinition>();
                    if (node.StorageAreaReadsSymbol == null)
                        node.StorageAreaReadsSymbol = new Dictionary<StorageArea, VariableSymbol>();

                    //Target read dictionaries
                    dataDefinitionStorage = node.StorageAreaReadsDataDefinition;
                    symbolStorage = node.StorageAreaReadsSymbol;
                }
                else
                {
                    //Initialize writes dictionaries
                    if (node.StorageAreaWritesDataDefinition == null)
                        node.StorageAreaWritesDataDefinition = new Dictionary<StorageArea, DataDefinition>();
                    if (node.StorageAreaWritesSymbol == null)
                        node.StorageAreaWritesSymbol = new Dictionary<StorageArea, VariableSymbol>();

                    //Target writes dictionaries
                    dataDefinitionStorage = node.StorageAreaWritesDataDefinition;
                    symbolStorage = node.StorageAreaWritesSymbol;
                }

                //Add DataDefinition found and corresponding VariableSymbol into caches
                dataDefinitionStorage.Add(storageArea, dataDefinitionFound);
                var variableSymbol = dataDefinitionFound.SemanticData as VariableSymbol;
                symbolStorage.Add(storageArea, variableSymbol);//Beware, variableSymbol my be null !

                //SemanticDomain validation : check that the symbol has been built.
                if (dataDefinitionFound.ParentTypeDefinition == null)
                {
                    System.Diagnostics.Debug.Assert(variableSymbol != null);
                }
                else
                {
                    //TODO SemanticDomain: requires type expansion.
                }

                return dataDefinitionFound;
            }

            return null;
        }

        private static void IndexAndFlagDataDefiniton(DataDefinitionPath dataDefinitionPath,
            DataDefinition dataDefinition,
            Node node, StorageArea area, StorageArea storageArea)
        {
            if (dataDefinition.IsIndex)
            {
                var index = dataDefinition;

                index.AddReferences(storageArea, node); //Add this node as a reference to the founded index

                if (area.SymbolReference.IsQualifiedReference || index.IsPartOfATypeDef)
                //Index name is qualified or belongs to a typedef
                {
                    if (index.Name.Length > 22) //If index name is used with qualification and exceed 22 characters
                        DiagnosticUtils.AddError(index.Parent,
                            "Index name '" + index.Name + "' is over 22 characters.", area.SymbolReference);
                    if (
                            index.Parent.CodeElement.IsInsideCopy())
                        //If index comes from a copy, do not support qualification
                        DiagnosticUtils.AddError(node,
                            "Index '" + index.Name + "' inside a COPY cannot be use with qualified symbol", area.SymbolReference);

                    //Mark this node for generator
                    FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, node, storageArea,
                        dataDefinitionPath);

                    foreach (var reference in index.GetReferences())
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, reference.Value,
                            reference.Key, dataDefinitionPath);
                    }

                    //No matter which node uses this index, if at least one time a node uses the index with a qualified name, we need to flag the index parent
                    //Also we always flag indexes declared as part of a typedef because we're already in a TypeCobol context
                    //Flag index node for code generator to let it know that this index will need hash.
                    index.SetFlag(Node.Flag.IndexUsedWithQualifiedName, true);
                }
                else if (!area.SymbolReference.IsQualifiedReference)
                    //If it's an index but not use with qualified reference 
                {
                    //If the index has already been flaged UsedWithQualifiedName, we need to flag the current node
                    if (index.IsFlagSet(Node.Flag.IndexUsedWithQualifiedName))
                    {
                        FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsIndex, node, storageArea,
                            dataDefinitionPath);
                    }
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
                    FlagNodeAndCreateQualifiedStorageAreas(Node.Flag.NodeContainsBoolean, node, storageArea, dataDefinitionPath);
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
                        "Cannot write into " + storageArea + ", " + variabletoCheck.Name +
                        " is declared out of LINKAGE SECTION.", area.SymbolReference);
            }

            if (specialRegister != null
                && specialRegister.SpecialRegisterName.TokenType == TokenType.ADDRESS
                && node is Call)
            {
                var callStatement = node.CodeElement as CallStatement;
                var currentCheckedParameter = callStatement?.InputParameters.FirstOrDefault(
                    param => param.StorageAreaOrValue?.StorageArea == specialRegister);

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
                if ((int) tokenGroup.Key >= 513 && (int) tokenGroup.Key <= 520 && tokenGroup.Count() > 1)
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

        private void CheckEndNode([CanBeNull]IToken openingToken, CodeElement endCodeElement)
        {
            // Check end statement is aligned with the matching opening statement
            if (openingToken != null && openingToken.Line != endCodeElement.Line &&
                openingToken.StartIndex != endCodeElement.StartIndex)
            {
                DiagnosticUtils.AddError(endCodeElement,
                    "a End statement is not aligned with the matching opening statement",
                    _compilerOptions.CheckEndAlignment.GetMessageCode());
            }
        }
    }

    class SectionOrParagraphUsageChecker
    {
        /// <summary>
        /// Disambiguate between Section or Paragraph reference.
        /// </summary>
        /// <param name="callerNode">Node using the paragraph or the reference.</param>
        /// <param name="target">A non-null Symbol reference to disambiguate.</param>
        /// /// <param name="scopeNode">The node scope in which the perform statement is declared</param>
        /// <returns>A tuple made of a SymbolType and a Node when the target has been correctly resolved.
        /// The returned SymbolType is non-ambiguous if the type of the target has been determined.</returns>
        /// <remarks>This method will create appropriate diagnostics on Node if resolution is inconclusive.</remarks>
        public static (SymbolType, Node) ResolveTargetSectionOrParagraph(Node callerNode, [NotNull] SymbolReference target, Node scopeNode)
        {
            IList<Section> sections;
            IList<Paragraph> paragraphs;
            var symbolType = target.Type;
            //First step : check target type
            if (target.IsAmbiguous)
            {
                //Have to search for both sections and paragraphs in SymbolTable
                sections = GetSections();
                paragraphs = GetParagraphs();
            }
            else
            {
                switch (symbolType)
                {
                    case SymbolType.SectionName:
                        sections = GetSections();
                        paragraphs = null;
                        break;
                    case SymbolType.ParagraphName:
                        sections = null;
                        paragraphs = GetParagraphs();
                        break;
                    default:
                        //Invalid SymbolType for a procedure name
                        return (symbolType, null);
                }
            }
            //Second step : Resolve according to whether we're dealing with a section or a paragraph name
            //Here we're dealing with a section
            if (paragraphs == null || paragraphs.Count == 0)
            {
                if (sections == null || sections.Count == 0)
                {
                    DiagnosticUtils.AddError(callerNode, $"Symbol {target.Name} is not referenced", target, MessageCode.SemanticTCErrorInParser);
                    return (symbolType, null);
                }

                symbolType = SymbolType.SectionName;
                if (sections.Count > 1)
                {
                    DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to section {target.Name}", target, MessageCode.SemanticTCErrorInParser);
                    return (symbolType, null);
                }

                return (symbolType, sections[0]);
            }
            //Here we're dealing with either a section or a paragraph
            else
            {
                //No section matches the name so it's a paragraph
                if (sections == null || sections.Count == 0)
                {
                    //We set the symbol type to paragraph
                    symbolType = SymbolType.ParagraphName;
                    //Now we enter the tricky part if we have more than one paragraph with the same name
                    if (paragraphs.Count > 1)
                    {
                        //If the name is qualified, no need to check further => it's an error
                        if (target.IsQualifiedReference)
                        {
                            DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to paragraph {target.Name}", target, MessageCode.SemanticTCErrorInParser);
                        }
                        //Otherwise we need to check the existing declarations within the scope in which the perform statement is declared
                        else
                        {
                            IEnumerable<Paragraph> matchingParagraphs = null;
                            //Perform statement is declared in a section scope
                            if (scopeNode is Section)
                            {
                                //we're filtering out all paragraphs not declared in section scopeNode
                                matchingParagraphs = paragraphs.Where(p => p.Parent == scopeNode);
                            }
                            //Perform statement directly declared in the PROCEDURE DIVISION
                            else
                            {
                                //we're filtering out all paragraphs that have a section
                                matchingParagraphs = paragraphs.Where(p => p.Parent.SemanticData.Kind != Symbol.Kinds.Section);
                            }
                            //matchingParagraphs = 0 => no paragraph matches the name in the given scope (scopeNode), as we have more than one paragraph with that name in other scopes resolution is ambiguous
                            //matchingParagraphs > 1 => multiple paragraphs match the given name in the given scope (scopeNode), resolution is ambiguous
                            if (matchingParagraphs.Count() != 1)
                                DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to paragraph {target.Name}", target, MessageCode.SemanticTCErrorInParser);
                        }
                            
                        return (symbolType, null);
                    }

                    return (symbolType, paragraphs[0]);
                }
                //The reference is ambiguous, we don't know what we're dealing with
                else
                {
                    DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to procedure {target.Name}", target, MessageCode.SemanticTCErrorInParser);
                    return (SymbolType.TO_BE_RESOLVED, null);
                }
            }

            IList<Section> GetSections() => callerNode.SymbolTable.GetSection(target.Name);

            IList<Paragraph> GetParagraphs() => callerNode.SymbolTable.GetParagraph(target);
        }

        private static void CheckIsEmpty<T>(string nodeTypeName, T node) where T : Node
        {
            // a section/paragraph (node) is empty when it has no child or when its child/children is/are an End node
            bool empty = true; // default value
            foreach (Node child in node.Children)  
            {
                if (child is Sentence || child is Paragraph)
                {
                    // have child(ren); at least one End node
                    if ((child.Children.Count == 1 && (child.Children[0] is Nodes.End)) == false)
                    {
                        // not only one END node
                        empty = false;
                        break;
                    }
                }
                else
                {
                    // a statement exists
                    empty = false;
                    break;
                }
            }
            if (empty)
            {
                DiagnosticUtils.AddError(node, nodeTypeName + " \'" + node.Name + "\' is empty",
                    MessageCode.Warning);
            }
        }

        public static void CheckSection(Section section)
        {
            //Get all sections with the same name
            var sections = section.SymbolTable.GetSection(section.Name);

            //Sections can be declared with the same name but then "perform thru" is not possible
            if (sections.Count > 1)
            {
                DiagnosticUtils.AddError(section, $"Section \'{section.Name}\' already declared", MessageCode.Warning);
            }

            //Check if any paragraphs also have that name
            var paragraphs = section.SymbolTable.GetParagraphs(p => p.Name.Equals(section.Name, StringComparison.OrdinalIgnoreCase)).ToList();
            
            //A section cannot have the same name as a paragraph
            if (paragraphs.Count > 0)
                DiagnosticUtils.AddError(section, $"Section {section.Name} is also declared as a paragraph", MessageCode.SemanticTCErrorInParser);

            CheckIsEmpty("Section", section);
        }

        public static void CheckParagraph(Paragraph paragraph)
        {
            //Get all paragraphs with the same name and having the same section name
            var paragraphs = paragraph.SymbolTable.GetParagraphs(p => p.Name.Equals(paragraph.Name, StringComparison.OrdinalIgnoreCase) && p.SemanticData.Owner == paragraph.SemanticData.Owner).ToList();

            //Paragraphs can't have the same name within the same section
            if (paragraphs.Count > 1)
            {
                //Get the name of the scope to display in diagnostic message
                var scope = paragraph.Parent.Name.Equals(String.Empty) ? paragraph.Parent.ID : paragraph.Parent.Name ;
                DiagnosticUtils.AddError(paragraph, $"Paragraph \'{paragraph.Name}\' already declared in {scope}");
            }

            //Get all the sections with the same name as paragraph
            var sections = paragraph.SymbolTable.GetSection(paragraph.Name);

            //A paragraph cannot have the same name as a section
            if(sections.Count > 0)
                DiagnosticUtils.AddError(paragraph, $"Paragraph {paragraph.Name} is also declared as a section", MessageCode.SemanticTCErrorInParser);

            CheckIsEmpty("Paragraph", paragraph);
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
                receivingTypeDefinition = wsymbol.TypeDefinition ?? GetDataDefinitionType(node, wsymbol, false);

            var sname = sent as QualifiedName;
            if (sname != null)
            {
                var ssymbol = node.GetDataDefinitionForQualifiedName(sname);
                if (ssymbol == null) return; // sending symbol name unresolved
                sendingTypeDefinition = ssymbol.TypeDefinition ?? GetDataDefinitionType(node, ssymbol, true);
            }
            else if (sent is StorageArea)
            {
                DataDefinition rsymbol = null;
                //var rsymbol = CrossCompleteChecker.CheckVariable(node, (StorageArea) sent,true);    
                if (node.StorageAreaReadsDataDefinition != null)
                {
                    node.StorageAreaReadsDataDefinition.TryGetValue((StorageArea) sent,
                        out searchExistingDataDefinition);
                    rsymbol = searchExistingDataDefinition;
                }

                if (rsymbol != null)
                    sendingTypeDefinition = rsymbol.TypeDefinition ?? GetDataDefinitionType(node, rsymbol, true);
            }
            else
            {
                //This will resolve the following cases MOVE 1 TO myVar / MOVE true TO myVar / MOVE "test" TO myVar. 
                if (sent is bool?) sendingTypeDefinition = GeneratedDefinition.BooleanGeneratedDefinition;
                if (sent is double?) sendingTypeDefinition = GeneratedDefinition.NumericGeneratedDefinition;
                if (sent is string) sendingTypeDefinition = GeneratedDefinition.AlphanumericGeneratedDefinition;
            }

            //TypeDefinition Comparison
            if (receivingTypeDefinition != null && !(receivingTypeDefinition.Equals(sendingTypeDefinition) ||
                                                     (wname is StorageAreaPropertySpecialRegister &&
                                                      sent is StorageAreaPropertySpecialRegister)))
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

                        DiagnosticUtils.AddError(node, message, wname.SymbolReference,
                            code: MessageCode.SemanticTCErrorInParser);
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
                    var searchedDataDefinition =
                        node.GetDataDefinitionForQualifiedName(redefines.RedefinesDataName.URI, isReadDictionary);
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
            }
            else
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
            var searchedDataDefinition =
                node.GetDataDefinitionForQualifiedName(dataRedefinesEntry.RedefinesDataName.URI, isReadDictionary);
            if (searchedDataDefinition == null)
            {
                return null;
            }
            if (searchedDataDefinition is DataDescription)
            {
                return (DataDescriptionEntry) searchedDataDefinition.CodeElement;
            }
            if (searchedDataDefinition is DataRedefines)
            {
                return GetDataDescriptionEntry(node, (DataRedefinesEntry) searchedDataDefinition.CodeElement,
                    isReadDictionary);
            }
            throw new NotImplementedException(searchedDataDefinition.Name);
        }

    }
}