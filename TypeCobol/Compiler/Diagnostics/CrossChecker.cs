using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using Antlr4.Runtime;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Diagnostics
{
    public class CrossCompleteChecker : AbstractAstVisitor
    {
        public CrossCompleteChecker([NotNull]TypeCobolOptions compilerOptions)
        {
            _compilerOptions = compilerOptions;
            _searchTables = new Dictionary<Search, List<DataDefinition>>();
        }

        private readonly TypeCobolOptions _compilerOptions;

        /// <summary>
        /// For each encountered Search statement, stores target table and its parent table definitions.
        /// If the target table could not be resolved or if it's actually not a table, no entry is added.
        /// Every entry is either null (if table is part of a TC typedef) or not empty (the searched table itself is always part of the list).
        /// </summary>
        private readonly Dictionary<Search, List<DataDefinition>> _searchTables;

        //Holds a reference to the last section node visited as to know in which current section we are
        private Section _currentSection;

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
            FormalizedCommentsChecker.CheckFunctionComments(functionDeclaration);
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

            (performProcedureNode.ProcedureParagraphSymbol, performProcedureNode.ProcedureSectionSymbol) = SectionOrParagraphUsageChecker.ResolveParagraphOrSection(performProcedureNode, performCE.Procedure, _currentSection);
            (performProcedureNode.ThroughProcedureParagraphSymbol, performProcedureNode.ThroughProcedureSectionSymbol) = SectionOrParagraphUsageChecker.ResolveParagraphOrSection(performProcedureNode, performCE.ThroughProcedure, _currentSection);

            return true;
        }

        public override bool Visit(Sort sort)
        {
            var sortStatement = sort.CodeElement;

            (sort.InputProcedureParagraphSymbol, sort.InputProcedureSectionSymbol) = SectionOrParagraphUsageChecker.ResolveParagraphOrSection(sort, sortStatement.InputProcedure, _currentSection);
            (sort.InputThroughProcedureParagraphSymbol, sort.InputThroughProcedureSectionSymbol) = SectionOrParagraphUsageChecker.ResolveParagraphOrSection(sort, sortStatement.ThroughInputProcedure, _currentSection);

            (sort.OutputProcedureParagraphSymbol, sort.OutputProcedureSectionSymbol) = SectionOrParagraphUsageChecker.ResolveParagraphOrSection(sort, sortStatement.OutputProcedure, _currentSection);
            (sort.OutputThroughProcedureParagraphSymbol, sort.OutputThroughProcedureSectionSymbol) = SectionOrParagraphUsageChecker.ResolveParagraphOrSection(sort, sortStatement.ThroughOutputProcedure, _currentSection);

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
            //initializing the current section to null
            _currentSection = null;
            return true;
        }

        public override bool Visit(Section section)
        {
            //Save current scope node for paragraph resolution
            _currentSection = section;
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

                var senderIsAlphanumeric = false;
                DataDefinition senderDataDefinition = null;
                if (moveSimple.SendingVariable?.StorageArea?.Kind == StorageAreaKind.DataOrCondition
                    && move.StorageAreaReadsDataDefinition?.TryGetValue(moveSimple.SendingVariable.StorageArea, out senderDataDefinition) == true)
                {
                    senderIsAlphanumeric = senderDataDefinition.DataType == DataType.Alphanumeric;
                }

                foreach (var area in moveSimple.StorageAreaWrites)
                {
                    var receiver = area.StorageArea;
                    if (receiver == null) continue;

                    if (receiver.Kind == StorageAreaKind.FunctionCallResult)
                    {
                        DiagnosticUtils.AddError(move, "MOVE: illegal <function call> after TO");
                    }
                    else if (senderIsAlphanumeric
                              && receiver.Kind == StorageAreaKind.DataOrCondition
                              && move.StorageAreaWritesDataDefinition != null
                              && move.StorageAreaWritesDataDefinition.TryGetValue(receiver, out var receiverDataDefinition))
                    {
                        if (receiverDataDefinition.DataType == DataType.Numeric || receiverDataDefinition.DataType == DataType.NumericEdited)
                        {
                            if (receiverDataDefinition.Usage != null && receiverDataDefinition.Usage != DataUsage.None)
                            {
                                DiagnosticUtils.AddError(move, $"Moving alphanumeric '{senderDataDefinition.Name}' to numeric '{receiverDataDefinition.Name}' declared with an USAGE may lead to unexpected results.", code: MessageCode.Warning);
                            }
                        }
                    }
                }
            }

            return true;
        }

        public override bool Visit(Search search)
        {
            int whenSearchCount = 0;
            bool onAtEndFound = false;
            int index = 0;
            foreach (var child in search.Children)
            {
                if (child is OnAtEnd)
                {
                    if (onAtEndFound)
                    {
                        DiagnosticUtils.AddError(child, "ON AT END clause must be unique.");
                    }
                    else
                    {
                        onAtEndFound = true;
                        if (index > 0)
                        {
                            DiagnosticUtils.AddError(child, "ON AT END clause must appear before WHEN.");
                        }
                    }
                }
                else if (child is When whenSearch)
                {
                    whenSearchCount++;
                    if (whenSearchCount > 1 && search.CodeElement.StatementType == StatementType.SearchBinaryStatement)
                    {
                        DiagnosticUtils.AddError(whenSearch,
                            "Invalid WHEN clause, binary SEARCH only allows a single WHEN clause");
                    }
                }

                index++;
            }

            if (whenSearchCount == 0)
            {
                DiagnosticUtils.AddError(search, "Search statement must have at least one when element.");
            }

            var tableToSearch = search.CodeElement.TableToSearch?.StorageArea;
            if (tableToSearch != null)
            {
                var searchedTable = search.GetDataDefinitionFromStorageAreaDictionary(tableToSearch);
                if (searchedTable != null)
                {
                    if (searchedTable.IsTableOccurence)
                    {
                        var parentTableDefinitions = searchedTable.GetParentTableDefinitions();
                        _searchTables.Add(search, parentTableDefinitions);

                        //Check keys and indexes for binary search
                        if (search.CodeElement.StatementType == StatementType.SearchBinaryStatement)
                        {
                            //Searched table must have at least one KEY
                            var keys = searchedTable.GetTableSortingKeys();
                            if (keys == null || keys.Length == 0)
                            {
                                DiagnosticUtils.AddError(search, $"Cannot use binary SEARCH on '{searchedTable.Name}' because it has no KEY.");
                            }

                            //Main table and all parent tables must have at least one index
                            if (parentTableDefinitions != null)
                            {
                                foreach (var table in parentTableDefinitions)
                                {
                                    var indexes = table.GetIndexes();
                                    if (indexes == null || indexes.Length == 0)
                                    {
                                        var message = table == searchedTable
                                            ? $"Cannot use binary SEARCH on '{searchedTable.Name}' because it is not indexed."
                                            : $"Cannot use binary SEARCH on '{searchedTable.Name}' because parent table '{table.Name}' is not indexed.";
                                        DiagnosticUtils.AddError(search, message);
                                    }
                                }
                            }
                            //else TC not supported
                        }
                    }
                    else
                    {
                        //Not a table
                        DiagnosticUtils.AddError(search, $"Cannot SEARCH in '{searchedTable.Name}', data item is not a table.");
                    }
                }
                //else undefined reference
            }
            //else it's a syntax error

            return true;
        }

        
        public override bool Visit(When whenSearch)
        {
            if (!(whenSearch.Parent is Search search)) return true; //EVALUATE statement, not our concern here.

            if (whenSearch.ChildrenCount == 0)
            {
                var messageCode = search.CodeElement.StatementType == StatementType.SearchSerialStatement ? MessageCode.SyntaxErrorInParser : MessageCode.Warning;
                DiagnosticUtils.AddError(whenSearch, "Missing statement in \"when\" clause", messageCode);
            }

            if (search.CodeElement.StatementType == StatementType.SearchBinaryStatement && _searchTables.TryGetValue(search, out var tableDefinitions))
            {
                //TC not supported
                if (tableDefinitions == null) return true;

                //Main table
                System.Diagnostics.Debug.Assert(tableDefinitions.Count > 0);
                var searchedTable = tableDefinitions[0];

                //Init a dictionary of used keys
                Dictionary<string, bool> usedKeys = new Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase);
                var keys = searchedTable.GetTableSortingKeys();
                if (keys != null)
                {
                    foreach (var key in keys)
                    {
                        if (key.SortDirection != null && key.SortDirection.Value != SortDirection.None)
                        {
                            usedKeys.Add(key.SortKey.Name, false);//Set initial status of the key to 'not used'
                        }
                    }
                }

                //Collect every first index of searched table and its parent tables (multidimensional search)
                //Reverse order because parent tables are from child to parent but subscripts are from parent to child
                var expectedIndexes = tableDefinitions.Select(table => table.GetIndexes()?.FirstOrDefault()).Reverse().ToArray();

                //WHEN condition must use keys and first index of the table
                if (!CheckCondition(whenSearch.CodeElement.Condition))
                {
                    return true;
                }

                //Check all keys are properly used
                bool expectKeyUsed = true;
                foreach (var isKeyUsed in usedKeys.Values)
                {
                    if (isKeyUsed)
                    {
                        if (expectKeyUsed) continue; //OK

                        //KO all keys from first to "highest" used must be used
                        DiagnosticUtils.AddError(whenSearch, "All the table keys that precede a referenced key must be used.");
                        break;
                    }

                    if (expectKeyUsed)
                    {
                        //First time we see an unused key, so all following keys must not be used
                        expectKeyUsed = false;
                    }
                }

                //Check syntax of a whenSearchCondition (in a binary search)
                bool CheckCondition(ConditionalExpression whenSearchCondition)
                {
                    switch (whenSearchCondition)
                    {
                        case ConditionNameConditionOrSwitchStatusCondition conditionInstance:
                            DataOrConditionStorageArea dataOrConditionStorageArea = conditionInstance.ConditionReference;
                            if (dataOrConditionStorageArea.Subscripts.Length > 0)
                            {
                                return CheckDataOrConditionStorageArea(dataOrConditionStorageArea);
                            }
                            return true;

                        case RelationCondition relationCondition:
                            if (relationCondition.Operator?.SemanticOperator != RelationalOperatorSymbol.EqualTo)
                            {
                                DiagnosticUtils.AddError(whenSearch, "Invalid relational operator in WHEN SEARCH condition, EqualTo operator expected.");
                                return false;
                            }
                            return CheckOperand(relationCondition.LeftOperand);

                        case LogicalOperation logicalOperation:
                            if (logicalOperation.Operator.Value != LogicalOperator.AND)
                            {
                                DiagnosticUtils.AddError(whenSearch, "Invalid logical operator in WHEN SEARCH condition, AND operator expected.");
                            }
                            return CheckCondition(logicalOperation.LeftOperand) && CheckCondition(logicalOperation.RightOperand);

                        default:
                            DiagnosticUtils.AddError(whenSearch, "Invalid condition in WHEN SEARCH, only condition-names and key to value comparison are allowed.");
                            return false;
                    }
                }

                bool CheckOperand(ConditionOperand operand)
                {
                    if (operand.ArithmeticExpression is NumericVariableOperand numericVariableOperand
                        && numericVariableOperand.NumericVariable?.StorageArea is DataOrConditionStorageArea dataOrConditionStorageArea
                        && dataOrConditionStorageArea.Subscripts.Length > 0)
                    {
                        return CheckDataOrConditionStorageArea(dataOrConditionStorageArea);
                    }

                    DiagnosticUtils.AddError(whenSearch, "Left side operand of a WHEN condition must use first index of the table and at least one of declared keys.");
                    return false;
                }

                bool CheckDataOrConditionStorageArea(DataOrConditionStorageArea dataOrConditionStorageArea)
                {
                    //Check indexes for every dimension
                    if (dataOrConditionStorageArea.Subscripts.Length == expectedIndexes.Length)
                    {
                        for (int i = 0; i < dataOrConditionStorageArea.Subscripts.Length; i++)
                        {
                            var expectedIndex = expectedIndexes[i];
                            var subscript = dataOrConditionStorageArea.Subscripts[i];

                            //Check use of first table index for the current dimension
                            var usedIndexStorageArea = ((NumericVariableOperand)subscript.NumericExpression).IntegerVariable.StorageArea;
                            var usedIndex = whenSearch.GetDataDefinitionFromStorageAreaDictionary(usedIndexStorageArea);
                            if (usedIndex != null && (expectedIndex == null || !expectedIndex.Name.Equals(usedIndex.Name, StringComparison.OrdinalIgnoreCase)))
                            {
                                //Not the first index (or no index defined for the table)
                                DiagnosticUtils.AddError(whenSearch, "When subscripting, only first index declared for the table is allowed.");
                                return false;
                            }
                        }
                    }
                    //else invalid subscript count, this is already checked by CheckSubscripts. No need to report more errors on this condition.

                    //Collect used key
                    var usedKey = whenSearch.GetDataDefinitionFromStorageAreaDictionary(dataOrConditionStorageArea, true);
                    if (usedKey != null)
                    {
                        if (usedKeys.ContainsKey(usedKey.Name))
                        {
                            //Valid key, set key status to 'used'
                            usedKeys[usedKey.Name] = true;
                        }
                        else 
                        {
                            //Special check for 88 level definitions that are children of a selected used key.
                            if (usedKey.CodeElement?.LevelNumber?.Value == 88 &&
                                usedKey.Parent is DataDefinition parentUsedKey &&
                                usedKeys.ContainsKey(parentUsedKey.Name))
                            {
                                usedKeys[parentUsedKey.Name] = true;
                            }
                            else
                            {
                                //Not a key
                                DiagnosticUtils.AddError(whenSearch, $"'{usedKey.Name}' is not a sorting key of table '{searchedTable.Name}'.");
                                return false;
                            }
                        }
                    }
                    //else undefined reference

                    return true;
                }
            }

            return true;
        }

        public override bool Visit(Evaluate evaluate)
        {
            bool whenOtherSeen = false;
            //Start to loop on children from the end because:
            //- Grammar enforce that there is 0 to 1 "whenOther" and that it's the last element of an "evaluate"
            //- Invalid empty "when" can only be the last "when"
            for (int i = evaluate.ChildrenCount - 1; i >= 0; i--)
            {
                if (evaluate.Children[i] is WhenOther)
                {
                    whenOtherSeen = true;
                }
                else if (evaluate.Children[i] is Then then)
                {
                    if (then.ChildrenCount == 0)
                    {
                        // i is the Then node
                        // i-1 is the When group
                        var whenGroup = then.Parent.Children[i - 1];
                        System.Diagnostics.Debug.Assert(whenGroup.ChildrenCount > 0);
                        //Get the last When of the WhenGroup. This is the invalid empty When
                        var whenNode = whenGroup.Children[whenGroup.ChildrenCount - 1];
                        System.Diagnostics.Debug.Assert(whenNode.CodeElement?.Type == CodeElementType.WhenCondition);
                        //Syntax error.
                        DiagnosticUtils.AddError(whenNode, "Missing statement in \"when\" clause");
                    }
                    break; //Previous "when" are allowed to have no instructions (#1593)
                }
            }
            if (!whenOtherSeen)
            {
                DiagnosticUtils.AddError(evaluate, "\"when other\" is missing", MessageCode.Warning);
            }
            return true;
        }

        public override bool Visit(If ifNode)
        {
            #region Temporary debug code for #2266

            if (ifNode.Children.Count == 0)
            {
                var debugData = Logging.LoggingSystemExtensions.CreateDebugData(ifNode);
                Logging.LoggingSystem.LogMessage(Logging.LogLevel.Error, "CrossCompleteChecker.Visit, found invalid IF node", debugData);
            }

            #endregion

            if (!(ifNode.Children.Last() is End))
            {
                DiagnosticUtils.AddError(ifNode,
                    "\"end-if\" is missing", MessageCode.Warning);
            }
            return true;
        }

        public override bool Visit(Then thenNode)
        {
            //This check only applies to THEN nodes coming from IF statements.
            if (thenNode.ChildrenCount == 0 && thenNode.Parent.CodeElement?.Type == CodeElementType.IfStatement)
            {
                //THEN has no CodeElement, report on Parent IF.
                DiagnosticUtils.AddError(thenNode.Parent, "Missing statement or NEXT SENTENCE after IF condition.");
            }
            return true;
        }

        public override bool Visit(Else elseNode)
        {
            if (elseNode.ChildrenCount == 0)
            {
                DiagnosticUtils.AddError(elseNode, "Missing statement or NEXT SENTENCE after ELSE keyword.");
            }
            return true;
        }

        public override bool Visit(TypeDefinition typeDefinition)
        {
            //Cobol 2002 rule
            //TODO need to clarify if we have 1 visitor per LanguageLevel
            //For performance reason it seems better to have only one here
            TypeDefinitionChecker.CheckTypeDefinition(typeDefinition);
            FormalizedCommentsChecker.CheckTypeComments(typeDefinition);
            return true;
        }

        public override bool Visit(Program program)
        {
            // Check that program has a closing end
            CheckEndProgram(program);
            if (program.IsNested)
            {
                NestedProgram nestedProgram = (NestedProgram)program;
                if (nestedProgram.ContainingProgram.CodeElement.IsRecursive)
                {
                    DiagnosticUtils.AddError(program, "A Nested Program cannot be declared in a RECURSIVE program.");
                }
                if (program.CodeElement.IsRecursive)
                {
                    DiagnosticUtils.AddError(program, "A Nested Program cannot have a RECURSIVE attribute.");
                }
            }
            else if (program.CodeElement.IsCommon)
            {
                DiagnosticUtils.AddError(program, "A Root Program cannot have a COMMON attribute.");
            }
            FormalizedCommentsChecker.CheckProgramComments(program);
          
            return true;
        }

        public override bool Visit(ConfigurationSection configurationSection)
        {
            Program program = configurationSection.GetProgramNode();
            if (program != null && program.IsNested)
            {
                DiagnosticUtils.AddError(configurationSection, "A Nested Program cannot have a CONFIGURATION SECTION.");
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

            DataDefinitionEntry dataDefinitionEntry = dataDefinition.CodeElement;
            if (dataDefinitionEntry == null) return true;

            var commonDataDataDefinitionCodeElement = dataDefinitionEntry as CommonDataDescriptionAndDataRedefines;

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

            if (dataDefinition.SemanticData != null)
            {
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
            // Check if PROGRAM END is orphan
            if (_compilerOptions.CheckEndProgram.IsActive)
            {
                if (end.CodeElement.Type == CodeElementType.ProgramEnd && !(end.Parent is Program))
                {
                    DiagnosticUtils.AddError(end, "Unexpected orphan \"PROGRAM END\".", _compilerOptions.CheckEndProgram.GetMessageCode());
                }
            }

            // Check end statement is aligned with the matching opening statement
            if (_compilerOptions.CheckEndAlignment.IsActive && end.CodeElement.Type != CodeElementType.SentenceEnd)
            {
                CodeElement parentCodeElement = end.Parent.CodeElement; ;
                if (parentCodeElement?.IsInsideCopy() == false && end.IsInsideCopy() == false)
                {
                    CheckEndNode(parentCodeElement, end);
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
                    CheckEndNode(openingDeclareToken, functionEnd);
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
                //Get immediately following DataDefinition
                var nextData = siblingNodes.Skip(nodeIndex + 1).OfType<DataDefinition>().FirstOrDefault();
                if (nextData != null && nextData.IsInsideCopy())
                {
                    DiagnosticUtils.AddError(dataDescription, $"Cannot include copy {nextData.CodeElement.FirstCopyDirective.TextName} " +
                                                              $"under level {dataDescriptionEntry.LevelNumber} " +
                                                              $"because copy starts at level {nextData.CodeElement.LevelNumber}.", dataDescriptionEntry);
                }
                else
                {
                    //Last node so this is an empty group item
                    DiagnosticUtils.AddError(dataDescription, "A group item cannot be empty.", dataDescriptionEntry);
                }
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
                var lastChild = dataDefinition.Children[dataDefinition.ChildrenCount - 1];

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

        public override bool Visit(Exec exec)
        {
            // EXEC SQL and all its children must be in same source file
            string execTranslatorName = exec.CodeElement.ExecTranslatorName?.Name;
            if (string.Equals(execTranslatorName, "SQL", StringComparison.OrdinalIgnoreCase) && exec.ChildrenCount > 0)
            {
                var referenceCopyDirective = exec.CodeElement.FirstCopyDirective;
                foreach (var execChild in exec.Children)
                {
                    var copyDirective = execChild.CodeElement?.FirstCopyDirective;
                    if (referenceCopyDirective != copyDirective)
                    {
                        DiagnosticUtils.AddError(exec, "Syntax not supported: when embedding SQL statements, EXEC SQL and all its content (including END-EXEC) must be in the same source file.");
                        break; //Stop on first error, avoid reporting error for every child
                    }
                }
            }

            return true;
        }

        public override bool Visit(SpecialNames specialNames)
        {
            // Check for environment mnemonics that conflict with variables
            foreach (var environmentMnemonicName in specialNames.SymbolTable.GetEnvironmentMnemonicsNames())
            {
                var variables = specialNames.SymbolTable.GetVariables(d => string.Equals(d.Name, environmentMnemonicName, StringComparison.OrdinalIgnoreCase), SymbolTable.Scope.Global);
                foreach (var dataDefinition in variables)
                {
                    // Data definition name conflicts with mnemonic-name for environment
                    DiagnosticUtils.AddError(dataDefinition, $"The name '{dataDefinition.Name}' is already used to define a mnemonic for environment name. Rename this variable or the mnemonic to avoid ambiguous references.");
                }
            }

            return true;
        }

        public override bool Visit(Accept accept)
        {
            // Resolve input device
            if (accept.CodeElement is AcceptFromInputDeviceStatement acceptFromInputDeviceStatement)
            {
                CheckEnvironmentNameOrMnemonicForEnvironmentName(accept, acceptFromInputDeviceStatement.InputDevice);
            }

            return true;
        }

        public override bool Visit(Display display)
        {
            // Resolve output device
            CheckEnvironmentNameOrMnemonicForEnvironmentName(display, display.CodeElement.OutputDeviceName);
            return true;
        }

        public override bool Visit(Write write)
        {
            // Resolve number of lines of ADVANCING clause
            var variable = write.CodeElement.ByNumberOfLinesOrByMnemonicForEnvironmentName;
            var referenceToResolve = variable?.MainSymbolReference;
            if (referenceToResolve == null)
            {
                // Nothing to check
                return true;
            }

            if (referenceToResolve.IsAmbiguous)
            {
                // Check for mnemonic
                var mnemonic = CheckMnemonicForEnvironmentName(write, referenceToResolve);

                // Check for variable
                var ambiguousSymbolReference = (AmbiguousSymbolReference)referenceToResolve;
                System.Diagnostics.Debug.Assert(!ambiguousSymbolReference.IsQualifiedReference);
                System.Diagnostics.Debug.Assert(ambiguousSymbolReference.CandidateTypes != null);
                System.Diagnostics.Debug.Assert(ambiguousSymbolReference.CandidateTypes.Length == 2);
                System.Diagnostics.Debug.Assert(ambiguousSymbolReference.CandidateTypes.Contains(SymbolType.DataName));
                System.Diagnostics.Debug.Assert(ambiguousSymbolReference.CandidateTypes.Contains(SymbolType.MnemonicForEnvironmentName));
                var originalCandidateTypes = ambiguousSymbolReference.CandidateTypes;
                ambiguousSymbolReference.CandidateTypes = new[] { SymbolType.DataName }; //To call CheckVariable again
                var dataDefinition = CheckVariable(write, variable.StorageArea, true, true);
                ambiguousSymbolReference.CandidateTypes = originalCandidateTypes;

                // Add diagnostic
                if (mnemonic == null && dataDefinition == null)
                {
                    //Nothing found
                    DiagnosticUtils.AddError(write, $"Unable to resolve reference to '{referenceToResolve.Name}'.", referenceToResolve, MessageCode.SemanticTCErrorInParser);
                }
                else if (mnemonic != null && dataDefinition != null)
                {
                    //This is still ambiguous...
                    DiagnosticUtils.AddError(write, $"Ambiguous reference to '{referenceToResolve.Name}', the definition to be used could not be determined from the context.", referenceToResolve, MessageCode.SemanticTCErrorInParser);
                }
                //else either a mnemonic or a variable, nothing to do.
            }
            //else already checked by CheckVariable as a regular IntegerVariable with non-ambiguous symbol ref.

            return true;
        }

        private static void CheckEnvironmentNameOrMnemonicForEnvironmentName(Node node, SymbolReference environmentOrMnemonicReference)
        {
            var name = environmentOrMnemonicReference?.Name;
            if (name == null)
            {
                // Nothing to check
                return;
            }

            // Check for environment name
            if (Enum.TryParse<CobolWordsBuilder.EnvironmentName>(name, true, out _))
            {
                return;
            }

            // Check for mnemonic (check also for conflicting variables)
            var environmentMnemonicDefinition = CheckMnemonicForEnvironmentName(node, environmentOrMnemonicReference);
            var dataDefinitionCount = node.SymbolTable.GetVariables(environmentOrMnemonicReference).Count();
            if (environmentMnemonicDefinition == null)
            {
                string message = dataDefinitionCount == 0
                    // Nothing found
                    ? $"Unable to resolve reference to '{environmentOrMnemonicReference.Name}'."
                    // Cannot use variable here
                    : $"Cannot use '{environmentOrMnemonicReference.Name}' here, environment-name or mnemonic for environment-name was expected.";
                DiagnosticUtils.AddError(node, message, environmentOrMnemonicReference, MessageCode.SemanticTCErrorInParser);
            }
            else if (dataDefinitionCount > 0)
            {
                // A mnemonic has been found but it conflicts with one or more variables
                DiagnosticUtils.AddError(node, $"Ambiguous reference to '{environmentOrMnemonicReference.Name}', the definition to be used could not be determined from the context.", environmentOrMnemonicReference, MessageCode.SemanticTCErrorInParser);
            }
        }

        private static SymbolDefinition CheckMnemonicForEnvironmentName(Node node, SymbolReference mnemonicReference)
        {
            var candidates = node.SymbolTable.GetEnvironmentMnemonics(mnemonicReference);
            switch (candidates.Count)
            {
                case 0:
                    // Let caller decide whether it is a problem or not...
                    return null;
                case 1:
                    return candidates[0];
                default:
                    DiagnosticUtils.AddError(node, $"Ambiguous reference to '{mnemonicReference.Name}' environment mnemonic.", mnemonicReference, MessageCode.SemanticTCErrorInParser);
                    return null;
            }
        }

        public static DataDefinition CheckVariable(Node node, StorageArea storageArea, bool isReadStorageArea, bool allowNoMatch = false)
        {
            if (storageArea == null || !storageArea.NeedDeclaration)
                return null;

            var area = storageArea.GetStorageAreaThatNeedDeclaration;
            if (area.SymbolReference == null) return null;
            //Do not handle TCFunctionName, it'll be done by TypeCobolChecker
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName)) return null;
            //Will be handled specifically for each statement that could use mnemonics
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.MnemonicForEnvironmentName)) return null;

            var parentTypeDefinition = (node as DataDefinition)?.ParentTypeDefinition;

            var uri = area.SymbolReference != null ? area.SymbolReference.URI : new URI(area.ToString());
            var foundQualified = node.SymbolTable.GetVariablesExplicitWithQualifiedName(uri, parentTypeDefinition);

            if (foundQualified.Count == 0)
            {
                //Helper diagnostic for subtraction between numeric written without spaces around minus sign.
                //Only do this diagnostic if :
                //  - the variable contains no alpha, because otherwise the most standard use case is simply an undefined variable.
                //  - SymbolReference is NOT qualified (OF, IN, ::), otherwise it's clearly not an arithmetic subtraction.
                if (area.SymbolReference != null && area.SymbolReference.IsQualifiedReference == false)
                {
                    if (area.SymbolReference.Name.All(c => char.IsDigit(c) || c == '-'))
                    {
                        DiagnosticUtils.AddError(node, "Variable must contains at least one alphabetic char. Arithmetic operations require spaces around minus sign: " + string.Join(" - ", area.SymbolReference.Name.Split('-')));
                    }
                }

                if (!allowNoMatch && node.SymbolTable.GetFunction(area).Count < 1)
                    DiagnosticUtils.AddError(node, "Symbol " + area + " is not referenced", area.SymbolReference, MessageCode.SemanticTCErrorInParser);
            }
            else if (foundQualified.Count > 1)
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
            else if (foundQualified.Count == 1)
            {
                var dataDefinitionFound = foundQualified[0].Value;
                var dataDefinitionPath = foundQualified[0].Key;

                IndexAndFlagDataDefiniton(dataDefinitionPath, dataDefinitionFound, node, area, storageArea);

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
                        //We allow modification on indices because semantically they are value types, the procedure uses a copy of the input (issue #1789).
                        //Also format 5 set statements are allowed because it does not affect the input for the caller (issue #1625).
                        //set (address of)? identifier(pointer) TO (address of)? identifier | NULL
                        bool isModificationAllowed = dataDefinitionFound.IsTableIndex
                                                     ||
                                                     (storageArea is StorageAreaPropertySpecialRegister register && register.SpecialRegisterName.TokenType == TokenType.ADDRESS);
                        if (!isModificationAllowed)
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
                symbolStorage.Add(storageArea, variableSymbol); //Beware, as long as SemanticDomain is not complete, variableSymbol may be null !

                //SemanticDomain validation : check that the symbol has been built.
                if (dataDefinitionFound.ParentTypeDefinition == null)
                {
                    //TODO SemanticDomain: requires support for RENAMES.
                    System.Diagnostics.Debug.Assert(variableSymbol != null || dataDefinitionFound is DataRenames);
                }
                else
                {
                    //TODO SemanticDomain: requires type expansion.
                }

                //Check subscripts
                if (storageArea is DataOrConditionStorageArea dataOrConditionStorageArea)
                {
                    CheckSubscripts(node, dataOrConditionStorageArea, dataDefinitionFound);
                }

                return dataDefinitionFound;
            }

            return null;
        }

        private static void CheckSubscripts(Node node, DataOrConditionStorageArea dataOrConditionStorageArea, DataDefinition dataDefinition)
        {
            if (dataOrConditionStorageArea.IsPartOfFunctionArgument)
            {
                //Avoid checking uncertain subscripts, see issue #2001
                return;
            }

            switch (node.CodeElement?.Type)
            {
                //Those have their own specific subscript checking
                case CodeElementType.SearchStatement:
                case CodeElementType.ProcedureStyleCall:
                    return;
            }

            //Create a list of all parent OCCURS
            var tableDefinitions = dataDefinition.IsTableIndex ? new List<DataDefinition>() : dataDefinition.GetParentTableDefinitions();
            //TC not supported
            if (tableDefinitions == null) return;

            if (dataOrConditionStorageArea.Subscripts.Length < tableDefinitions.Count)
            {
                //Not enough subscripts
                DiagnosticUtils.AddError(node, $"Not enough subscripts for data item '{dataDefinition.Name}', check number of OCCURS clauses.", dataOrConditionStorageArea.SymbolReference);
                return;
            }

            if (dataOrConditionStorageArea.Subscripts.Length > tableDefinitions.Count)
            {
                //Too many subscripts
                DiagnosticUtils.AddError(node, $"Too many subscripts for data item '{dataDefinition.Name}', check number of OCCURS clauses.", dataOrConditionStorageArea.SymbolReference);
                return;
            }

            for (int i = 0; i < dataOrConditionStorageArea.Subscripts.Length; i++)
            {
                var subscript = dataOrConditionStorageArea.Subscripts[i];
                var tableDefinition = tableDefinitions[tableDefinitions.Count - 1 - i];//OCCURS are stored in reverse order

                //Do not check expressions, only literal values are checked
                if (!(subscript.NumericExpression is NumericVariableOperand subscriptNumeric)) continue;

                //Do not check variables, only literal values are checked
                System.Diagnostics.Debug.Assert(subscriptNumeric.NumericVariable == null);
                System.Diagnostics.Debug.Assert(subscriptNumeric.IntegerVariable != null);
                if (subscriptNumeric.IntegerVariable.Value == null) continue;

                var subscriptLiteral = subscriptNumeric.IntegerVariable.Value;

                //Check the value against the first allowed index
                if (subscriptLiteral.Value < 1)
                {
                    DiagnosticUtils.AddError(node, $"Subscript value '{subscriptLiteral.Value}' is invalid. Subscript must be a strictly positive integer.", subscriptLiteral.Token);
                }

                //Check the value against the max
                if (!tableDefinition.HasUnboundedNumberOfOccurences && subscriptLiteral.Value > tableDefinition.MaxOccurencesCount)
                {
                    DiagnosticUtils.AddError(node, $"Subscript value '{subscriptLiteral.Value}' exceeds the maximum occurrence count '{tableDefinition.MaxOccurencesCount}' of the table.", subscriptLiteral.Token);
                }
            }
        }

        private static void IndexAndFlagDataDefiniton(DataDefinitionPath dataDefinitionPath,
            DataDefinition dataDefinition,
            Node node, StorageArea area, StorageArea storageArea)
        {
            if (dataDefinition.IsTableIndex)
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
                if (!((node is Nodes.If && storageArea.Kind != StorageAreaKind.StorageAreaPropertySpecialRegister) || node is Nodes.Set || node is Nodes.Perform || node is Nodes.PerformProcedure || node is Nodes.When ) || storageArea.Kind == StorageAreaKind.StorageAreaPropertySpecialRegister)//Ignore If/Set/Perform/WhenSearch Statement
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

        private static void FlagNodeAndCreateQualifiedStorageAreas(Node.Flag flag, Node node, StorageArea storageArea,
            DataDefinitionPath dataDefinitionPath)
        {
            node.SetFlag(flag, true);
            if (node.QualifiedStorageAreas == null)
                node.QualifiedStorageAreas = new Dictionary<StorageArea, DataDefinitionPath>();

            if (!node.QualifiedStorageAreas.ContainsKey(storageArea))
                node.QualifiedStorageAreas.Add(storageArea, dataDefinitionPath);
        }

        private void CheckEndNode([CanBeNull]IToken openingToken, Node endNode)
        {
            System.Diagnostics.Debug.Assert(endNode is End || endNode is FunctionEnd);
            System.Diagnostics.Debug.Assert(endNode.CodeElement != null);

            // Check end statement is aligned with the matching opening statement
            var endCodeElement = endNode.CodeElement;
            if (openingToken != null && openingToken.Line != endCodeElement.Line &&
                openingToken.StartIndex != endCodeElement.StartIndex)
            {
                DiagnosticUtils.AddError(endNode,
                    "a End statement is not aligned with the matching opening statement",
                    _compilerOptions.CheckEndAlignment.GetMessageCode());
            }
        }

        private void CheckEndProgram(Program node)
        {
            var checkEndProgramOption = _compilerOptions.CheckEndProgram;
            var lastChild = node.Children.LastOrDefault();
            if (lastChild is End end)
            {
                // END PROGRAM is present
                node.SetFlag(Node.Flag.MissingEndProgram, false);
                if (checkEndProgramOption.IsActive)
                {
                    var programEnd = (ProgramEnd)end.CodeElement;
                    if (programEnd.ProgramName?.Name == null)
                    {
                        // No name is specified after END PROGRAM
                        DiagnosticUtils.AddError(end, $"\"PROGRAM END\" should have a program name. \"{node.Name}\" was assumed.", checkEndProgramOption.GetMessageCode());
                    }
                    else
                    {
                        if (!node.Name.Equals(programEnd.ProgramName.Name, StringComparison.OrdinalIgnoreCase))
                        {
                            // Wrong name is specified after END PROGRAM
                            DiagnosticUtils.AddError(end, $"Program name \"{programEnd.ProgramName.Name}\" did not match the name of any open program. The \"END PROGRAM\" marker was assumed to have ended program \"{node.Name}\".", checkEndProgramOption.GetMessageCode());
                        }
                    }
                }
            }
            else
            {
                // END PROGRAM is missing
                node.SetFlag(Node.Flag.MissingEndProgram, true);
                if (checkEndProgramOption.IsActive)
                {
                    if (node is SourceProgram && node.Parent.Children.OfType<SourceProgram>().Last() == node)
                    {
                        // Node is last program
                        if (!node.NestedPrograms.Any())
                        {
                            // Exception if only last program is not closed and has no nested program
                            // No diagnostic in this case
                            return;
                        }
                    }
                    DiagnosticUtils.AddError(node, "\"END PROGRAM\" is missing.", checkEndProgramOption.GetMessageCode());
                }
            }
        }
    }

    static class SectionOrParagraphUsageChecker
    {
        /// <summary>
        /// Disambiguate between Paragraph or Section reference.
        /// </summary>
        /// <param name="callerNode">Node using the paragraph or the reference.</param>
        /// <param name="target">A Symbol reference to disambiguate.</param>
        /// <param name="currentSection">The node scope in which the perform statement is declared</param>
        /// <returns>A tuple corresponding to the paragraph OR section found.
        /// The returned tuple has one null value and one filled value if the reference is non-ambiguous.</returns>
        /// <remarks>This method will create appropriate diagnostics on Node if resolution is inconclusive.</remarks>
        public static (ParagraphSymbol, SectionSymbol) ResolveParagraphOrSection(Node callerNode, SymbolReference target, Section currentSection)
        {
            if (target == null) return (null, null);

            var (sections, paragraphs) = callerNode.SymbolTable.GetSectionOrParagraph(target, currentSection);
            if (paragraphs == null || paragraphs.Count == 0)
            {
                if (sections == null || sections.Count == 0)
                {
                    //Nothing found
                    DiagnosticUtils.AddError(callerNode, $"Symbol {target.Name} is not referenced", target, MessageCode.SemanticTCErrorInParser);
                    return (null, null);
                }

                //We know for sure it's a section but is it ambiguous ?
                if (sections.Count > 1)
                {
                    DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to section {target.Name}", target, MessageCode.SemanticTCErrorInParser);
                    return (null, null);
                }

                //Return single section found
                return (null, (SectionSymbol)sections[0].SemanticData);
            }

            //No section matches the name so it's a paragraph
            if (sections == null || sections.Count == 0)
            {
                //Check if paragraph is ambiguous
                if (paragraphs.Count > 1)
                {
                    DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to paragraph {target.Name}", target, MessageCode.SemanticTCErrorInParser);
                    return (null, null);
                }

                //Return single paragraph found
                return ((ParagraphSymbol)paragraphs[0].SemanticData, null);
            }

            //The reference is ambiguous, we don't know what we're dealing with
            DiagnosticUtils.AddError(callerNode, $"Ambiguous reference to procedure {target.Name}", target, MessageCode.SemanticTCErrorInParser);
            return (null, null);
        }

        private static void CheckIsNotEmpty<T>(string nodeTypeName, T node) where T : Node
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
            {
                DiagnosticUtils.AddError(section, $"Section {section.Name} is also declared as a paragraph", MessageCode.SemanticTCErrorInParser);
                foreach (var paragraph in paragraphs)
                {
                    DiagnosticUtils.AddError(paragraph, $"Paragraph {paragraph.Name} is also declared as a section", MessageCode.SemanticTCErrorInParser);
                }
            }

            CheckIsNotEmpty("Section", section);
        }

        public static void CheckParagraph(Paragraph paragraph)
        {
            //Get all paragraphs with the same name and having the same parent
            var paragraphs = paragraph.SymbolTable.GetParagraphs(paragraph.Name, paragraph.Parent);
            if (paragraphs?.Count() > 1)
            {
                //Get the name of the scope to display in diagnostic message
                var scope = string.IsNullOrEmpty(paragraph.Parent.Name) ? paragraph.Parent.ID : paragraph.Parent.Name;
                DiagnosticUtils.AddError(paragraph, $"Paragraph \'{paragraph.Name}\' already declared in {scope}", MessageCode.Warning);
            }

            CheckIsNotEmpty("Paragraph", paragraph);
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
                if (sent is bool) sendingTypeDefinition = GeneratedDefinition.BooleanGeneratedDefinition;
                if (sent is double) sendingTypeDefinition = GeneratedDefinition.NumericGeneratedDefinition;
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
                else
                {
                    //TODO Unsupported DataRenames (and IndexDefinition ?)
                    entry = null;
                }

                if (entry == null)
                    return null;

                if (entry.UserDefinedDataType == null)
                    return new GeneratedDefinition(entry.DataType.Name, entry.DataType);
            }
            else
            {
                return null;
            }

            if (data.TypeDefinition != null)
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

            if (searchedDataDefinition is DataDescription)
            {
                return (DataDescriptionEntry) searchedDataDefinition.CodeElement;
            }
            if (searchedDataDefinition is DataRedefines)
            {
                return GetDataDescriptionEntry(node, (DataRedefinesEntry) searchedDataDefinition.CodeElement, isReadDictionary);
            }

            return null;
        }

    }
}