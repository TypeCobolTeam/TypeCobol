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
using TypeCobol.Compiler.Domain.Validator;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Diagnostics
{
    public class CrossCompleteChecker : AbstractAstVisitor
    {
        public CrossCompleteChecker([NotNull]TypeCobolOptions compilerOptions)
        {
            _compilerOptions = compilerOptions;
        }

        private readonly TypeCobolOptions _compilerOptions;

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

            var levelNumber = dataDescriptionEntry.LevelNumber;
            //Check if the DataDescription is an empty group
            if (levelNumber != null && dataDescription.IsDataDescriptionGroup && dataDescription.ChildrenCount == 0)
            {
                DiagnosticUtils.AddError(dataDescription, "A group item cannot be empty.", dataDescriptionEntry);
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

#if DOMAIN_CHECKER
        private class DelegateErrorReporter : IValidationErrorReporter
        {
            private readonly Action<ValidationError> _onError;

            public DelegateErrorReporter(Action<ValidationError> onError)
            {
                _onError = onError;
            }

            public void Report(ValidationError validationError)
            {
                _onError(validationError);
            }
        }

        /// <summary>
        /// Expand the top program.
        /// </summary>
        /// <param name="curPrg">Current program.</param>
        /// <returns>True if expansion succeeded, False otherwise.</returns>
        private static bool ExpandTopProgram(ProgramSymbol curPrg)
        {
            bool result = true;
            var topProgram = ProgramSymbol.GetTopProgram(curPrg);
            var expander = new ProgramExpander(new DelegateErrorReporter(v => result = false));

            expander.Expand(topProgram);
            if (!result)
            {
                //Reset expansion state for this test session
                topProgram.SetFlag(Symbol.Flags.SymbolExpanded, false);
            }

            return result;
        }

        /// <summary>
        /// This static method normalize path names by removing consecutive dots like ".."
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        static string NormalizePathNames(string name)
        {
            string[] paths = name.Split('.');
            string sep = "";
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            foreach (var s in paths)
            {
                if (s.Length > 0)
                {
                    sb.Append(sep);
                    sb.Append(s);
                    sep = ".";
                }
            }
            return sb.ToString();
        }

        static string NormalizePathNames(Symbol[] symbolPaths)
        {
            string sep = "";
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            foreach (var s in symbolPaths)
            {
                if (s == symbolPaths[symbolPaths.Length- 1])
                {
                    if (s.Kind == Symbol.Kinds.Typedef)
                    {//If the last symbol is a Typedef don't put it in the path.
                        continue;
                    }
                }
                    sb.Append(sep);
                    sb.Append(s.Name);
                    sep = ".";
            }
            return sb.ToString();
        }
#endif
        private DataDefinition CheckVariable(Node node, StorageArea storageArea, bool isReadStorageArea)
        {
            if (storageArea == null || !storageArea.NeedDeclaration)
                return null;
#if DOMAIN_CHECKER
            if (_compilerOptions.UseSemanticDomain)
            {
                //Check that a semantic data has been associated to this node.
                System.Diagnostics.Debug.Assert(node.SemanticData != null);
                System.Diagnostics.Debug.Assert(node.SemanticData.SemanticKind == SemanticKinds.Symbol);
                //The semantic data is a ProgramSymbol or a FunctionSymbol
                System.Diagnostics.Debug.Assert(((Symbol) node.SemanticData).Kind == Symbol.Kinds.Program ||
                                                ((Symbol) node.SemanticData).Kind == Symbol.Kinds.Function ||
                                                ((Symbol) node.SemanticData).Kind == Symbol.Kinds.Variable ||
                                                ((Symbol) node.SemanticData).Kind == Symbol.Kinds.Index);
            }
#endif

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
#if DOMAIN_CHECKER
            Scopes.Container<VariableSymbol>.Entry result = null;
            List<Symbol[]> foundSymbolTypedPaths = null;
            if (_compilerOptions.UseSemanticDomain)
            {
                switch (((Symbol) node.SemanticData).Kind)
                {
                    case Symbol.Kinds.Program:
                    case Symbol.Kinds.Function:
                    {
                        ProgramSymbol prg = (ProgramSymbol) node.SemanticData;
                        if (ExpandTopProgram(prg))
                        {
                            //Consider only succeeded expansions.
                            result = prg.ResolveReference(area.SymbolReference, true);
                            System.Diagnostics.Debug.Assert(result != null);
                            //Check that we found the same number of symbols
                            System.Diagnostics.Debug.Assert(result.Count == foundCount);
                        }
                    }
                        break;
                    case Symbol.Kinds.Variable:
                    case Symbol.Kinds.Index:
                        //Humm....
                        //This Storage area's SemanticData is a Variable.
                        //Thus this situation can only appears if the variable is inside a
                        //Typedef, or Inside a Program or a Function.
                        //But any way we have found it.
                        VariableSymbol @var = (VariableSymbol) node.SemanticData;
                        if (@var.HasFlag(Symbol.Flags.InsideTypedef))
                        {
                            System.Diagnostics.Debug.Assert(@var.TopParent(Symbol.Kinds.Typedef) != null);
                            //We looking inside a TYPEDEF.
                            TypedefSymbol tdSym = (TypedefSymbol) @var.TopParent(Symbol.Kinds.Typedef);
                            foundSymbolTypedPaths = new List<Symbol[]>();
                            result = tdSym.Get(ScopeSymbol.SymbolReferenceToPath(area.SymbolReference), null,
                                foundSymbolTypedPaths);
                            System.Diagnostics.Debug.Assert(result != null);
                            System.Diagnostics.Debug.Assert(result.Count == foundCount);
                        }
                        else
                        {
                            FunctionSymbol fun = (FunctionSymbol) @var.TopParent(Symbol.Kinds.Function);
                            ProgramSymbol prg = (ProgramSymbol) @var.TopParent(Symbol.Kinds.Program);
                            System.Diagnostics.Debug.Assert(prg != null || fun != null);
                            if (ExpandTopProgram(prg))
                            {
                                //Consider only succeeded expansions.
                                //Lookup itself in its program.
                                result = (fun ?? prg).ResolveReference(area.SymbolReference, true);
                                System.Diagnostics.Debug.Assert(result != null);
                                System.Diagnostics.Debug.Assert(result.Count == foundCount);
                            }
                        }

                        break;
                }
            }
#endif

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
                    errorMessage += (isFirst ? "" : " | ") + (symbol.Key != null
                                        ? symbol.Key.ToString().Replace(".", "::")
                                        : symbol.Value.QualifiedName.ToString().Replace(".", "::"));
                    isFirst = false;
                }
                DiagnosticUtils.AddError(node, errorMessage, area.SymbolReference);
            }
            else if (foundCount == 1)
            {
                var dataDefinitionFound = found.First();
                DataDefinitionPath dataDefinitionPath = foundQualified.First().Key;
#if DOMAIN_CHECKER
                if (_compilerOptions.UseSemanticDomain)
                {
                    if (result != null && result.Symbol != null)
                    {
                        System.Diagnostics.Debug.Assert(result.Symbol.TargetNode == null || result.Symbol.TargetNode == foundQualified.First().Value);
                        if (dataDefinitionPath != null)
                        {
                            string completeQualifiedName = dataDefinitionPath.ToString().Replace("::", ".");
                            if (result.Symbol.TargetNode == null)
                            {
                                //Special CASE DATE we don't capture the Target Node wich is created dynamically by TypeCobol.
                                System.Diagnostics.Debug.Assert(
                                    (dataDefinitionFound.Name == "YYYY" || dataDefinitionFound.Name == "DD" ||
                                     dataDefinitionFound.Name == "MM") &&
                                    result.Symbol.Owner != null && result.Symbol.Owner.HasFlag(Symbol.Flags.HasATypedefType)
                                    && result.Symbol.Owner is VariableTypeSymbol &&
                                    ((VariableTypeSymbol)result.Symbol.Owner).Typedef == BuiltinSymbols.Date);
                                //But ensure that the parent Node is the same
                                //System.Diagnostics.Debug.Assert(dataDefinitionFound.Parent == result.Symbol.Owner.TargetNode);
                            }
                            else
                                System.Diagnostics.Debug.Assert(dataDefinitionFound == result.Symbol.TargetNode);

                            //Check that the qualified name of the variable found is the same.
                            //I cannot do that because: Actually TypeCobol Path variable includes TYPEDEF.NAMES,
                            //New Domain doesn't include TYPEDEF.NAMES in paths. ==> cannot compare qualified path names.
                            string qname = foundSymbolTypedPaths != null
                                ? NormalizePathNames(foundSymbolTypedPaths[0])
                                : result.Symbol.FullTypedDotName;
                            System.Diagnostics.Debug.Assert(NormalizePathNames(completeQualifiedName).ToLower()
                                .Equals(qname.ToLower()));
                        }
                    }
                }
#endif
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
                    if (paramDesc?.PassingType == ParameterDescription.PassingTypes.Input)
                    {
                        DiagnosticUtils.AddError(node, "Input variable '" + paramDesc.Name + "' is modified by an instruction", area.SymbolReference);
                    }
                }

                //add the found DataDefinition to a dictionary depending on the storage area type
                if (isReadStorageArea)
                {
                    //need to initialize the dictionaries
                    if (node.StorageAreaReadsDataDefinition == null)
                    {
                        node.StorageAreaReadsDataDefinition = new Dictionary<StorageArea, DataDefinition>();
                    }
                    node.StorageAreaReadsDataDefinition.Add(storageArea, dataDefinitionFound);
                }
                else
                {
                    //need to initialize the dictionaries
                    if (node.StorageAreaWritesDataDefinition == null)
                    {
                        node.StorageAreaWritesDataDefinition = new Dictionary<StorageArea, DataDefinition>();
                    }
                    node.StorageAreaWritesDataDefinition.Add(storageArea, dataDefinitionFound);
                }

                return dataDefinitionFound;
            }

            return null;
        }

        private void IndexAndFlagDataDefiniton(DataDefinitionPath dataDefinitionPath,
            DataDefinition dataDefinition,
            Node node, StorageArea area, StorageArea storageArea)
        {
            if (dataDefinition.IsIndex)
            {
#if DOMAIN_CHECKER
                if (_compilerOptions.UseSemanticDomain)
                {
                    //Ensure that SemanticData is actually an index symbol
                    System.Diagnostics.Debug.Assert(((Symbol)dataDefinition.SemanticData).Kind == Symbol.Kinds.Index);
                }
#endif
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

        private void CheckEndNode(IToken openingToken, CodeElement endCodeElement)
        {
            // Check end statement is aligned with the matching opening statement
            if (openingToken.Line != endCodeElement.Line &&
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
        public static void CheckReferenceToParagraphOrSection(PerformProcedure perform)
        {
            var performCE = perform.CodeElement;
            SymbolReference symbol;
            symbol = ResolveProcedureName(perform.SymbolTable, performCE.Procedure as AmbiguousSymbolReference,
                perform);
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

        private static void Check<T>(string nodeTypeName, T node, [NotNull] IList<T> found) where T : Node
        {
            if (found.Count > 1)
                DiagnosticUtils.AddError(node, nodeTypeName + " \'" + node.Name + "\' already declared");

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
            Check("Section", section, section.SymbolTable.GetSection(section.Name));
        }

        public static void CheckParagraph(Paragraph paragraph)
        {
            Check("Paragraph", paragraph, paragraph.SymbolTable.GetParagraph(paragraph.Name));
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