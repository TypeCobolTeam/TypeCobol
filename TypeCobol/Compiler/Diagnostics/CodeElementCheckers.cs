using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using JetBrains.Annotations;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Types;
using TypeCobol.Tools;

namespace TypeCobol.Compiler.Diagnostics
{
    #region CodeElementCheckers

    class DataDescriptionChecker
    {
        public static void OnCodeElement(DataDescriptionEntry data, CodeElementsParser.DataDescriptionEntryContext context)
        {
            var external = GetContext(data, context?.externalClause());
            var global = GetContext(data, context?.globalClause());
            if (data.DataName == null)
            {
                if (data.IsExternal)
                    DiagnosticUtils.AddError(data,
                        "Data name must be specified for any entry containing the EXTERNAL clause", external);
                if (data.IsGlobal)
                   DiagnosticUtils.AddError(data,
                        "Data name must be specified for any entry containing the GLOBAL clause", global);
            }
            else
            {
                if (data.LevelNumber != null && data.IsExternal && data.LevelNumber.Value != 01)
                    DiagnosticUtils.AddError(data, "External is only allowed for level 01", external);

                if (data.LevelNumber != null &&
                    !((data.LevelNumber.Value >= 01 && data.LevelNumber.Value <= 49)
                      || data.Type == CodeElementType.DataRenamesEntry || data.LevelNumber.Value == 77 || data.Type == CodeElementType.DataConditionEntry))
                {
                    DiagnosticUtils.AddError(data,
                        "Data must be declared between level 01 to 49, or equals to 66, 77, 88",
                        context?.dataNameDefinition());
                }

                //Variable name in Cobol must contains at least one alphabetic char.
                if (!data.DataName.Name.Any(CobolChar.IsCobolAlphabeticChar))
                {
                    DiagnosticUtils.AddError(data, "Variable name must contain at least one alphabetic char.", context?.dataNameDefinition());
                }

                //Retrieve VALUE token through context
                var valueClauseContexts = context?.children.OfType<CodeElementsParser.ValueClauseContext>();
                Token valueToken = null;
                bool multipleContexts = false;
                if (valueClauseContexts != null)
                {
                    foreach (var valueClauseContext in valueClauseContexts)
                    {
                        if (valueToken != null)
                        {
                            multipleContexts = true;
                            break;
                        }

                        valueToken = (Token) valueClauseContext.Start;
                    }
                }

                if (valueToken != null)
                {
                    if (multipleContexts)
                    {
                        //This is a parsing error
                        DiagnosticUtils.AddError(data, "Invalid VALUE clause", valueToken);
                    }
                    //In Cobol reference format check that VALUE clause starts in Area B
                    else if (((CodeElementsLine) valueToken.TokensLine).ColumnsLayout == ColumnsLayout.CobolReferenceFormat && ValueTokenStartsOutsideAreaB())
                    {
                        DiagnosticUtils.AddError(data, "VALUE clause must start in area B", valueToken);
                    }

                    bool ValueTokenStartsOutsideAreaB()
                    {
                        return valueToken.Column < (int)CobolFormatAreas.Begin_B || valueToken.Column > (int)CobolFormatAreas.End_B;
                    }
                }
            }

            //Check Picture character string format and usage if any
            CheckPictureAndUsage(data, context);
        }

        public static void CheckPictureAndUsage([NotNull] CommonDataDescriptionAndDataRedefines codeElement, ParserRuleContextWithDiagnostics context)
        {
            // Picture checks
            if (codeElement.Picture != null)
            {
                /*
                 * Validate using PictureValidator
                 * We use the scan state of the first token in ANTLR context for this code element to retrieve special-names information,
                 * namely DecimalPointIsComma and the custom currency descriptors (if any).
                 */
                bool signIsSeparate = codeElement.SignIsSeparate?.Value ?? false;
                var specialNamesContext = (context?.Start as Token)?.TokensLine.ScanState.SpecialNames;
                bool decimalPointIsComma = specialNamesContext?.DecimalPointIsComma ?? false;
                var customCurrencyDescriptors = specialNamesContext?.CustomCurrencyDescriptors;
                var pictureValidator = new PictureValidator(codeElement.Picture.Value, signIsSeparate, decimalPointIsComma, customCurrencyDescriptors);
                var pictureValidationResult = pictureValidator.Validate(out var validationMessages);

                //Report validation errors as diagnostics
                if (!pictureValidationResult.IsValid)
                {
                    var pictureToken = codeElement.Picture.Token;
                    foreach (var validationMessage in validationMessages)
                    {
                        DiagnosticUtils.AddError(codeElement, validationMessage, pictureToken);
                    }
                }

                //Store validation result for future usages
                codeElement.PictureValidationResult = pictureValidationResult;
                if (codeElement.DataType == DataType.Unknown)
                {
                    codeElement.DataType = DataType.Create(pictureValidationResult);
                }
            }

            // Usage checks

            // Unsupported UTF-8 usage
            if (codeElement.Usage != null && codeElement.Usage.Value == DataUsage.UTF8)
            {
                DiagnosticUtils.AddError(codeElement, "USAGE UTF-8 is not supported.", codeElement.Usage.Token);
            }

            // Unsupported UTF-8 group usage
            if (codeElement.GroupUsage != null && codeElement.GroupUsage.Value == DataUsage.UTF8)
            {
                DiagnosticUtils.AddError(codeElement, "GROUP-USAGE UTF-8 is not supported.", codeElement.GroupUsage.Token);
            }

            //Unsupported dynamic-length items
            if (codeElement.HasDynamicLength != null && codeElement.HasDynamicLength.Value)
            {
                DiagnosticUtils.AddError(codeElement, "dynamic-length data items are not supported.", codeElement.HasDynamicLength.Token);
            }
        }

        public static void CheckRedefines(DataRedefinesEntry redefines, CodeElementsParser.DataDescriptionEntryContext context)
        {
            TypeDefinitionEntryChecker.CheckRedefines(redefines, context);
            CheckPictureAndUsage(redefines, context);
        }

        public static void CheckOccurs([NotNull] CommonDataDescriptionAndDataRedefines codeElement,
            [NotNull] CodeElementsParser.OccursClauseContext context,
            [NotNull] List<CodeElementsParser.DataNameReferenceContext> duplicateSortingKeysReferences)
        {
            // Create diagnostic for duplicate keys found by builder
            foreach (var duplicateSortingKeyReference in duplicateSortingKeysReferences)
            {
                DiagnosticUtils.AddError(codeElement, $"Sorting key '{duplicateSortingKeyReference.GetText()}' is already defined for this table.", duplicateSortingKeyReference);
            }
        }

        /// <summary>
        /// Return the first ParserRuleContext in a list.
        /// If there is more than one context in the parameter list, a diagnostic error is added to the CodeElement parameter.
        /// </summary>
        /// <typeparam name="T">ParserRuleContext subclass</typeparam>
        /// <param name="e">CodeElement in error if there is more than one context in contexts</param>
        /// <param name="contexts">List of ParserRuleContexts</param>
        /// <returns>First element of contexts if contexts is not null and of size > 0, null otherwise</returns>
        private static T GetContext<T>(CodeElement e, T[] contexts, bool checkErrors = true)
            where T : Antlr4.Runtime.ParserRuleContext
        {
            if (contexts == null) return null;
            if (contexts.Length < 1) return null;
            if (checkErrors)
            {
                for (int c = 1; c < contexts.Length; c++)
                    DiagnosticUtils.AddError(e, "Only one such clause allowed", contexts[c]);
            }
            return contexts[0];
        }
    }

    class DataConditionChecker
    {
        public static void OnCodeElement(DataConditionEntry data, CodeElementsParser.DataConditionEntryContext context)
        {
            if (data.Type != CodeElementType.DataConditionEntry)
                DiagnosticUtils.AddError(data, "Data conditions must be level 88", context?.levelNumber);
            if (data.DataName == null)
                DiagnosticUtils.AddError(data, "Data name must be specified for level-88 items", context?.levelNumber);
        }
    }

    class DataRenamesChecker
    {
        public static void OnCodeElement(DataRenamesEntry data, CodeElementsParser.DataRenamesEntryContext context)
        {
            if (data.LevelNumber?.Value != 66)
                //(source page 379 of ISO Cobol 2014)
                DiagnosticUtils.AddError(data, "RENAMES must be level 66", context?.levelNumber);
            if (data.DataName == null)
                //(source page 379 of ISO Cobol 2014)
                DiagnosticUtils.AddError(data, "Data name must be specified for level-66 items", context?.levelNumber);
            if (data.RenamesFromDataName.Equals(data.RenamesToDataName))
                //(source page 379 of ISO Cobol 2014)
                DiagnosticUtils.AddError(data,
                    "Renamed items can't be the same " + data.RenamesFromDataName + " and " + data.RenamesToDataName,
                    context);
        }
    }

    class AddStatementChecker
    {
        public static void OnCodeElement(AddStatement statement, CodeElementsParser.AddStatementContext context)
        {
            var givingStatement = statement as AddGivingStatement;
            if (givingStatement == null)
            {
                return; //not our job
            }
            if (givingStatement.Operand == null)
                DiagnosticUtils.AddError(givingStatement, "Required: <identifier> after TO", context?.addGiving());
        }
    }

    class CallStatementChecker
    {
        public static void OnCodeElement(CallStatement statement, CodeElementsParser.CallStatementContext c)
        {
            var context = c?.cobolCallStatement();
            if (context != null) //if null it's certainly a TcCallStatementContext
            {
                if (context.programNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariable() == null)
                    DiagnosticUtils.AddError(statement, "Empty CALL is not authorized", context.Start);

                // Check each parameter of the CALL
                CheckCallUsings(statement);

                if (context.callReturningParameter() != null && statement.OutputParameter == null)
                    DiagnosticUtils.AddError(statement, "CALL .. RETURNING: Missing identifier", context);
            }
        }

        /// <summary>
        /// Check the parameters of the CALL
        /// </summary>
        /// <param name="statement">CALL to check the parameters</param>
        private static void CheckCallUsings(CallStatement statement)
        {
            foreach (var inputParameter in statement.InputParameters)
            {
                var errorPosition = inputParameter.StorageAreaOrValue?.MainSymbolReference?.NameLiteral?.Token;

                // TODO#249 these checks should be done during semantic phase, after symbol type resolution
                // TODO#249 if input is a file name AND input.SendingMode.Value == SendingMode.ByContent OR ByValue
                bool isFunctionCallResult = inputParameter.StorageAreaOrValue != null &&
                                            inputParameter.StorageAreaOrValue.StorageArea is FunctionCallResult;

                //SpecialRegister if LENGTH OF, LINAGE-COUNTER, ...
                var specialRegister = inputParameter.StorageAreaOrValue != null
                    ? inputParameter.StorageAreaOrValue.StorageArea as StorageAreaPropertySpecialRegister
                    : null;

                if (isFunctionCallResult)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal function identifier", errorPosition);

                if (specialRegister != null && specialRegister.SpecialRegisterName.TokenType == TokenType.LINAGE_COUNTER)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LINAGE-COUNTER", errorPosition);


                if (inputParameter.SharingMode != null)
                {
                    //BY REFERENCE
                    if (inputParameter.SharingMode.Value == ParameterSharingMode.ByReference)
                    {
                        if (specialRegister != null && specialRegister.SpecialRegisterName.TokenType == TokenType.LENGTH)
                            DiagnosticUtils.AddError(statement,
                                "CALL .. USING: Illegal LENGTH OF in BY REFERENCE phrase", errorPosition);

                        if (inputParameter.StorageAreaOrValue != null && inputParameter.StorageAreaOrValue.IsLiteral)
                            DiagnosticUtils.AddError(statement,
                                "CALL .. USING: Illegal <literal> in BY REFERENCE phrase", errorPosition);
                    }

                    //BY VALUE
                    if (inputParameter.IsOmitted && inputParameter.SharingMode.Value == ParameterSharingMode.ByValue)
                    {
                        // Placing error on the token Omitted
                        DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal OMITTED in BY VALUE phrase", inputParameter.Omitted.Token);
                    }
                }
            }
        }
    }

    class CancelStatementChecker
    {
        public static void OnCodeElement(CancelStatement statement, CodeElementsParser.CancelStatementContext context)
        {
            foreach (var item in statement.Programs)
            {
                if (item == null) continue; //TODO#249
                if (item.SymbolReference == null) continue; // DO nothing
                if (string.IsNullOrWhiteSpace(item.SymbolReference.Name) || item.SymbolReference.Name.IsNumeric())
                {
                    // we should link this error to the specific context.identifierOrLiteral[i] context
                    // corresponding to statement.Items[i], but since refactor in #157 it's not trivial anymore
                    DiagnosticUtils.AddError(statement, "CANCEL: <program name> must be alphanumeric", context);
                }
            }
        }

    }

    class InspectConvertingChecker
    {
        public static void OnCodeElement(InspectConvertingStatement statement, CodeElementsParser.InspectStatementContext context)
        {
            var seen = new Dictionary<InspectStatement.StartCharacterPosition, bool>();
            foreach (var value in Enum.GetValues(typeof(InspectTallyingStatement.StartCharacterPosition)))
            {
                seen[(InspectTallyingStatement.StartCharacterPosition) value] = false;
            }
            for (int i = 0; i < statement.ReplacingConditions.Length; i++)
            {
                var position = statement.ReplacingConditions[i].StartCharacterPosition;
                if (seen[position.Value])
                {
                    string error = "INSPECT: Maximum one " + position.Token.SourceText +
                                   " phrase for any one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase";
                    if (context != null)
                        DiagnosticUtils.AddError(statement, error,
                            context.convertingPhrase().countingOrReplacingCondition()[i]);
                }
                seen[position.Value] = true;
            }
        }
    }

    class MergeUsingChecker
    {
        public static void OnCodeElement(MergeStatement statement, CodeElementsParser.MergeStatementContext context)
        {
            if (statement.InputFiles.Length == 1)
                DiagnosticUtils.AddError(statement, "MERGE: USING needs 2 filenames or more", context?.usingFilenames());
        }
    }

    class SearchStatementChecker
    {
        public static void OnCodeElement(SearchStatement statement, CodeElementsParser.SearchStatementContext context)
        {
            if (statement.TableToSearch == null) return; // syntax error
            if (statement.TableToSearch.StorageArea is DataOrConditionStorageArea &&
                ((DataOrConditionStorageArea) statement.TableToSearch.StorageArea).Subscripts.Length > 0)
                DiagnosticUtils.AddError(statement, "SEARCH: Illegal subscripted identifier", GetIdentifierContext(context));
            if (statement.TableToSearch.StorageArea?.ReferenceModifier != null)
                DiagnosticUtils.AddError(statement, "SEARCH: Illegal reference-modified identifier",
                    GetIdentifierContext(context));
        }

        private static RuleContext GetIdentifierContext(ParserRuleContext context)
        {
            var c = (CodeElementsParser.SearchStatementContext) context;
            if (c.serialSearch() != null) return c.serialSearch().variable1().identifier();
            if (c.binarySearch() != null) return c.binarySearch().variable1().identifier();
            return null;
        }
    }

    class SetStatementForAssignmentChecker
    {
        public static void OnCodeElement(SetStatementForAssignment set, CodeElementsParser.SetStatementForAssignmentContext context)
        {
            for (int i = 0; i < context?.dataOrIndexStorageArea().Length; i++)
            {
                if (i >= set.ReceivingStorageAreas.Length)
                {
                    var ctxt = context.dataOrIndexStorageArea()[i];
                    DiagnosticUtils.AddError(set, "Set: Receiving fields missing or type unknown before TO", ctxt);
                }
            }
            if (set.SendingVariable == null)
                DiagnosticUtils.AddError(set, "Set: Sending field missing or type unknown after TO",
                    context?.setSendingField());
        }
    }

    class SetStatementForIndexesChecker
    {
        public static void OnCodeElement(SetStatementForIndexes set, CodeElementsParser.SetStatementForIndexesContext context)
        {
            if (set.SendingVariable == null)
            {
                DiagnosticUtils.AddError(set, "Set xxx up/down by xxx: Sending field missing or type unknown", context);
            }
        }
    }

    class StartStatementChecker
    {
        public static void OnCodeElement(StartStatement statement, CodeElementsParser.StartStatementContext context)
        {
            if (context?.relationalOperator() != null)
                if (statement.RelationalOperator.SemanticOperator != RelationalOperatorSymbol.EqualTo &&
                    statement.RelationalOperator.SemanticOperator != RelationalOperatorSymbol.GreaterThan &&
                    statement.RelationalOperator.SemanticOperator != RelationalOperatorSymbol.GreaterThanOrEqualTo)
                    DiagnosticUtils.AddError(statement, "START: Illegal operator " + statement.RelationalOperator,
                        context.relationalOperator());
        }
    }

    class AlterStatementChecker
    {
        public static void OnCodeElement(AlterStatement statement, CodeElementsParser.AlterStatementContext context)
        {
            DiagnosticUtils.AddErrorWithNoRuleStack(statement, "ALTER should not be used", context, MessageCode.Warning);
        }
    }

    class CodeElementChecker
    {
        private const int MAX_NAME_LENGTH = 30;

        public static void OnCodeElement(CodeElement codeElement, TypeCobolOptions compilerOptions, bool isDebuggingModeEnabled)
        {
            // Check code element debug type
            if (compilerOptions.CheckCodeElementMixedDebugType.IsActive && isDebuggingModeEnabled)
            {
                if (codeElement.DebugMode == CodeElement.DebugType.Mix)
                {
                    var messageCode = compilerOptions.CheckCodeElementMixedDebugType.GetMessageCode();
                    DiagnosticUtils.AddError(codeElement, "In debugging mode, a statement cannot span across lines marked with debug and lines not marked debug.", messageCode);
                }
            }

            // Check name length, exclude typedefs as their names won't appear in generated Cobol.
            if (codeElement is INamedCodeElement namedCodeElement and not DataTypeDescriptionEntry)
            {
                var name = namedCodeElement.Name;
                if (name != null && name.Length > MAX_NAME_LENGTH)
                {
                    DiagnosticUtils.AddError(codeElement, $"The COBOL word '{name}' contains {name.Length} characters which is more than the allowed maximum of {MAX_NAME_LENGTH} characters.");
                }
            }
        }
    }

    class StopStatementChecker
    {
        public static void OnCodeElement(StopStatement statement, CodeElementsParser.StopStatementContext context)
        {
            if (statement.StopRun != null && statement.StopRun.Value)
            {
                DiagnosticUtils.AddError(statement, "GOBACK should be used instead of STOP RUN", ParseTreeUtils.GetFirstToken(context), 
                    null, MessageCode.Warning);
            }
        }
    }

    internal static class ReferenceModifierChecker
    {
        private static Diagnostic CreateDiagnostic(string message, IParseTree location)
        {
            var position = ParseTreeUtils.GetFirstToken(location).Position();
            return new Diagnostic(MessageCode.SyntaxErrorInParser, position, message);
        }

        public static void Check(ReferenceModifier referenceModifier, CodeElementsParser.ReferenceModifierContext context)
        {
            if (referenceModifier.LeftmostCharacterPosition == null)
            {
                context.AttachDiagnostic(CreateDiagnostic("Left-most position of a reference modifier is required.", context));
            }
            else
            {
                ValidateModifierValue(referenceModifier.LeftmostCharacterPosition);
            }

            if (referenceModifier.Length != null)
            {
                ValidateModifierValue(referenceModifier.Length);
            }

            void ValidateModifierValue(ArithmeticExpression arithmeticExpression)
            {
                switch (arithmeticExpression.NodeType)
                {
                    case ExpressionNodeType.ArithmeticOperation:
                        // Cannot do anything in that case
                        break;
                    case ExpressionNodeType.NumericVariable:
                        var numericVariableOperand = (NumericVariableOperand)arithmeticExpression;
                        var numericVariable = numericVariableOperand.NumericVariable;
                        // Only negative literals are erroneous
                        if (numericVariable?.Value?.Value <= 0)
                        {
                            context.AttachDiagnostic(CreateDiagnostic("Reference modifiers should be positive non-zero values.", context));
                        }
                        break;
                    default:
                        throw new InvalidOperationException("Unexpected expression type: " + arithmeticExpression.NodeType);
                }
            }
        }
    }

    /// <summary>
    /// Create diagnostics for language level restricted elements used in a lower level parsing context
    /// (typically TypeCobol syntax used in Cobol85 code).
    /// </summary>
    internal class UnsupportedLanguageLevelFeaturesChecker
    {
        private static Diagnostic CreateDiagnostic(string message, IParseTree location, CobolLanguageLevel minLevel)
        {
            var position = ParseTreeUtils.GetFirstToken(location).Position();
            return new Diagnostic(MessageCode.UnsupportedLanguageFeature, position, minLevel, message);
        }

        private static void AddError(CodeElement codeElement, string message, IParseTree location, CobolLanguageLevel minLevel = CobolLanguageLevel.TypeCobol)
        {
            if (codeElement.Diagnostics == null) codeElement.Diagnostics = new List<Diagnostic>();
            codeElement.Diagnostics.Add(CreateDiagnostic(message, location, minLevel));
        }

        private static void AddError(ParserRuleContextWithDiagnostics context, string message, IParseTree location = null, CobolLanguageLevel minLevel = CobolLanguageLevel.TypeCobol)
        {
            location = location ?? context;
            context.AttachDiagnostic(CreateDiagnostic(message, location, minLevel));
        }

        private readonly CobolLanguageLevel _targetLevel;

        public UnsupportedLanguageLevelFeaturesChecker(CobolLanguageLevel targetLevel)
        {
            _targetLevel = targetLevel;
        }

        public void Check(MoveSimpleStatement statement, CodeElementsParser.MoveSimpleContext context)
        {
            if (_targetLevel >= CobolLanguageLevel.TypeCobol) return;

            if (context.booleanValue() != null)
            {
                AddError(statement, "moving boolean values is not supported.", context.booleanValue());
            }

            //No need to check UNSAFE keyword, it will be picked up as a syntax error by ANTLR
        }

        public void Check(SetStatementForIndexes statement, CodeElementsParser.SetStatementForIndexesContext context)
        {
            if (_targetLevel >= CobolLanguageLevel.TypeCobol) return;

            if (context.variableOrExpression2()?.arithmeticExpression() != null)
            {
                AddError(statement, "using arithmetic expressions to manipulate indexes is not supported.", context.variableOrExpression2().arithmeticExpression());
            }
        }

        public void Check(ProcedureStyleCallStatement statement, CodeElementsParser.TcCallStatementContext context)
        {
            if (_targetLevel >= CobolLanguageLevel.TypeCobol) return;

            if (context.INPUT() != null)
            {
                AddErrorOnUnsupportedKeyword(context.INPUT());
            }

            if (context.OUTPUT() != null)
            {
                AddErrorOnUnsupportedKeyword(context.OUTPUT());
            }

            //No need to check IN-OUT keyword, it will be picked up as a syntax error by ANTLR

            void AddErrorOnUnsupportedKeyword(ITerminalNode unsupportedKeyword)
                => AddError(statement, $"'{unsupportedKeyword.GetText()}' keyword is not supported in a Cobol CALL.", unsupportedKeyword);
        }

        //dataDescriptionEntry ANTLR rule translates to DataDefinitionEntry CodeElement
        public void Check(DataDefinitionEntry entry, CodeElementsParser.DataDescriptionEntryContext context)
        {
            var minLevel = CobolLanguageLevel.TypeCobol;
            if (_targetLevel >= minLevel) return;

            //TC-only
            if (context.formalizedComment() != null)
            {
                AddError(entry, "formalized comments are not supported.", context.formalizedComment(), minLevel);
            }

            if (context.valueClauseWithBoolean() != null && context.valueClauseWithBoolean().Length > 0)
            {
                AddError(entry, "initial value for boolean data is not supported.", context.valueClauseWithBoolean()[0], minLevel);
            }

            /*
             * TYPEDEF : in Cobol85 TYPEDEF is not a keyword so entire clause is not parsed => no need to check here.
             * In Cobol2002 or Cobol2014, TYPEDEF is a keyword but STRICT, PUBLIC and PRIVATE remain user-defined words => no need to check here
             */

            minLevel = CobolLanguageLevel.Cobol2002;
            if (_targetLevel >= minLevel) return;

            //Cobol2002 and above only
            if (context.cobol2002TypeClause() != null && context.cobol2002TypeClause().Length > 0)
            {
                AddError(entry, "TYPE clause is not supported.", context.cobol2002TypeClause()[0], minLevel);
            }
        }

        public void Check(ProcedureDivisionHeader procedureDivisionHeader, CodeElementsParser.ProcedureDivisionHeaderContext context)
        {
            if (_targetLevel >= CobolLanguageLevel.TypeCobol) return;

            if (context.formalizedComment() != null)
            {
                AddError(procedureDivisionHeader, "formalized comments are not supported.", context.formalizedComment());
            }
        }

        public void Check(CodeElementsParser.FunctionIdentifierContext context)
        {
            if (_targetLevel >= CobolLanguageLevel.TypeCobol) return;

            //User-defined function call
            if (context.userDefinedFunctionCall() != null)
            {
                AddError(context.userDefinedFunctionCall().functionNameReference(), "calling user-defined function is not supported.");
            }

            //Use of 'FUNCTION func1()' instead of 'FUNCTION func1'
            //Do not check for user-defined function calls as they would already get an error
            if (context.intrinsicFunctionCall() != null)
            {
                var intrinsicFunctionCall = context.intrinsicFunctionCall();
                if (intrinsicFunctionCall.LeftParenthesisSeparator() != null
                    && intrinsicFunctionCall.RightParenthesisSeparator() != null
                    && intrinsicFunctionCall.intrinsicArgument().Length == 0)
                {
                    var name = intrinsicFunctionCall.IntrinsicFunctionName().GetText();
                    AddError(intrinsicFunctionCall, $"using empty brackets is not allowed, use 'FUNCTION {name}'.", intrinsicFunctionCall.IntrinsicFunctionName());
                }
            }
        }

        public void Check(CodeElement codeElement, CodeElementsParser.TcCodeElementContext context)
        {
            if (_targetLevel >= CobolLanguageLevel.TypeCobol) return;

            if (context.globalStorageSectionHeader() != null)
            {
                AddError(codeElement, "GLOBAL-STORAGE SECTION is not supported.", context.globalStorageSectionHeader());
            }

            if (context.libraryCopy() != null)
            {
                AddError(codeElement, "service include feature is not supported.", context.libraryCopy());
            }

            if (context.functionDeclarationHeader() != null)
            {
                AddError(codeElement, "defining custom functions/procedures is not supported.", context.functionDeclarationHeader());
            }

            //If a functionDeclarationEnd is present without any header, there will be a Cup parsing error so no need to check it here.
        }
    }

    static class AcceptStatementChecker
    {
        public static void OnCodeElement(AcceptStatement acceptStatement, CodeElementsParser.AcceptStatementContext context)
        {
            if (acceptStatement.ReceivingStorageArea == null)
            {
                DiagnosticUtils.AddError(acceptStatement, "Invalid ACCEPT statement, receiving area for data is required.", context);
            }
        }
    }

    class WhenConditionStatementChecker
    {
        public static void OnCodeElement(WhenCondition statement, CodeElementsParser.WhenConditionContext context)
        {
            var rhsExprs = context.comparisonRHSExpression();
            if (rhsExprs.Length == 0)
            {
                DiagnosticUtils.AddError(statement, "Missing condition in \"when\" clause", context);
            }
        }
    }

    class GotoConditionalStatementChecker
    {
        public static void OnCodeElement(GotoConditionalStatement statement, CodeElementsParser.GotoConditionalContext context)
        {
            if (statement.ProcedureNames.Length > 1 && statement.DependingOn == null)
                DiagnosticUtils.AddError(statement, "GO TO: Required only one <procedure name> or DEPENDING phrase", context);
            if (statement.ProcedureNames.Length < 1 && statement.DependingOn != null)
                DiagnosticUtils.AddError(statement, "Conditional GO TO: Required <procedure name>", context);
            if (statement.ProcedureNames.Length > 255)
                DiagnosticUtils.AddError(statement, "Conditional GO TO: Maximum 255 <procedure name> allowed", context);
        }
    }

    static class SpecialNamesParagraphChecker
    {
        public static void OnCodeElement(SpecialNamesParagraph paragraph, CodeElementsParser.SpecialNamesParagraphContext context, List<RuleContext> duplicateEnvironments, List<RuleContext> duplicateMnemonicsForEnvironment)
        {
            foreach (var environment in duplicateEnvironments)
            {
                DiagnosticUtils.AddError(paragraph, $"A duplicate '{environment.GetText()}' clause was found.", environment);
            }

            foreach (var mnemonic in duplicateMnemonicsForEnvironment)
            {
                DiagnosticUtils.AddError(paragraph, $"Mnemonic name '{mnemonic.GetText()}' was previously defined.", mnemonic);
            }
        }
    }

    static class RepositoryParagraphChecker
    {
        public static void OnCodeElement(RepositoryParagraph paragraph, CodeElementsParser.RepositoryParagraphContext context)
        {
            List<SymbolDefinitionOrReference> intrinsicFunctions = paragraph.IntrinsicFunctions;
            if (intrinsicFunctions != null)
            {
                foreach (var intrinsicFunction in intrinsicFunctions)
                {
                    var intrinsicFunctionName = intrinsicFunction.Name;
                    if (!CobolIntrinsicFunctions.IsAllowedInRepositoryParagraph(intrinsicFunctionName))
                    {
                        /*
                         * NameLiteral cannot be null here, otherwise intrinsicFuntionName would also be null
                         * and IsAllowedInRepositoryParagraph would then have returned true.
                         */
                        System.Diagnostics.Debug.Assert(intrinsicFunction.NameLiteral != null);

                        Token token = intrinsicFunction.NameLiteral.Token;
                        DiagnosticUtils.AddError(paragraph, $"\"{intrinsicFunctionName}\" was specified in the \"FUNCTION\" phrase of the \"REPOSITORY\" paragraph, but the keyword \"FUNCTION\" is always required for this function.", token);
                    }
                }
            }
        }
    }
    #endregion
}
