using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Antlr4.Runtime;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Tools;

namespace TypeCobol.Compiler.Diagnostics
{
    #region CodeElementCheckers

    class DataDescriptionChecker
    {
        public static void OnCodeElement(DataDescriptionEntry data,
            CodeElementsParser.DataDescriptionEntryContext context)
        {
            var external = GetContext(data, context?.externalClause());
            var global = GetContext(data, context?.globalClause());
            if (data.DataName == null)
            {
                if (!data.IsFiller)
                    DiagnosticUtils.AddError(data, "Data name or FILLER expected", context?.dataNameDefinition());
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
                      || data.LevelNumber.Value == 66 || data.LevelNumber.Value == 77 || data.LevelNumber.Value == 88))
                {
                    DiagnosticUtils.AddError(data,
                        "Data must be declared between level 01 to 49, or equals to 66, 77, 88",
                        context?.dataNameDefinition());
                }
            }

            CheckPicture(data);
        }

        
            
        /// <summary>
        /// Check if the picture of the CodeElement is valid.
        /// </summary>
        /// <param name="codeElement"></param>
        public static void CheckPicture([NotNull] CommonDataDescriptionAndDataRedefines codeElement)
        {
            if (codeElement.Picture == null) return;


            // if there is not the same number of '(' than of ')'
            if ((codeElement.Picture.Value.Split('(').Length - 1) != (codeElement.Picture.Value.Split(')').Length - 1))
            {
                DiagnosticUtils.AddError(codeElement, "missing '(' or ')'", codeElement.Picture.Token);
            }
            // if the first '(' is after first ')' OR last '(' is after last ')'
            else if (codeElement.Picture.Value.IndexOf("(", StringComparison.Ordinal) > codeElement.Picture.Value.IndexOf(")", StringComparison.Ordinal) ||
                     codeElement.Picture.Value.LastIndexOf("(", StringComparison.Ordinal) > codeElement.Picture.Value.LastIndexOf(")", StringComparison.Ordinal))
                DiagnosticUtils.AddError(codeElement, "missing '(' or ')'", codeElement.Picture.Token);
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
                        DiagnosticUtils.AddError(codeElement, m, codeElement.Picture.Token);
                    }
                }
            }
        }

        public static void CheckRedefines(DataRedefinesEntry redefines, CodeElementsParser.DataDescriptionEntryContext context)
        {
            TypeDefinitionEntryChecker.CheckRedefines(redefines, context);
            CheckPicture(redefines);
        }

        /// <summary>
        /// Return the first ParserRuleContext in a list.
        /// If there is more than one context in the parameter list, a diagnostic error is added to the CodeElement parameter.
        /// </summary>
        /// <typeparam name="T">ParserRuleContext subclass</typeparam>
        /// <param name="e">CodeElement in error if there is more than one context in contexts</param>
        /// <param name="contexts">List of ParserRuleContexts</param>
        /// <returns>First element of contexts if contexts is not null and of size > 0, null otherwise</returns>
        public static T GetContext<T>(CodeElement e, T[] contexts, bool checkErrors = true)
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
            if (data.LevelNumber?.Value != 88)
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
            if (context != null) //if null it's certainly a CallStatementContext
            {
                foreach (var call in context.callUsingParameters()) CheckCallUsings(statement, call);

                if (context.callReturningParameter() != null && statement.OutputParameter == null)
                    DiagnosticUtils.AddError(statement, "CALL .. RETURNING: Missing identifier", context);
            }

        }

        private static void CheckCallUsings(CallStatement statement, CodeElementsParser.CallUsingParametersContext context)
        {
            foreach (var input in statement.InputParameters)
            {
                // TODO#249 these checks should be done during semantic phase, after symbol type resolution
                // TODO#249 if input is a file name AND input.SendingMode.Value == SendingMode.ByContent OR ByValue
                //	DiagnosticUtils.AddError(statement, "CALL .. USING: <filename> only allowed in BY REFERENCE phrase", context);
                bool isFunctionCallResult = input.StorageAreaOrValue != null &&
                                            input.StorageAreaOrValue.StorageArea is FunctionCallResult;

                //SpecialRegister if LENGTH OF, LINAGE-COUNTER, ...
                var specialRegister = input.StorageAreaOrValue != null
                    ? input.StorageAreaOrValue.StorageArea as StorageAreaPropertySpecialRegister
                    : null;

                if (isFunctionCallResult)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal function identifier", context);

                if (specialRegister != null && specialRegister.SpecialRegisterName.TokenType == TokenType.LINAGE_COUNTER)
                    DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LINAGE-COUNTER", context);


                if (input.SharingMode != null)
                {
                    //BY REFERENCE
                    if (input.SharingMode.Value == ParameterSharingMode.ByReference)
                    {
                        if (specialRegister != null && specialRegister.SpecialRegisterName.TokenType == TokenType.LENGTH)
                            DiagnosticUtils.AddError(statement,
                                "CALL .. USING: Illegal LENGTH OF in BY REFERENCE phrase", context);

                        if (input.StorageAreaOrValue != null && input.StorageAreaOrValue.IsLiteral)
                            DiagnosticUtils.AddError(statement,
                                "CALL .. USING: Illegal <literal> in BY REFERENCE phrase", context);
                    }

                    //BY VALUE
                    if (input.IsOmitted && input.SharingMode.Value == ParameterSharingMode.ByValue)
                        DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal OMITTED in BY VALUE phrase", context);
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
                ((DataOrConditionStorageArea) statement.TableToSearch.StorageArea).Subscripts.Count > 0)
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
                DiagnosticUtils.AddError(set, "Set xxx up/down by xxx: Sending field missing or type unknown",
                    context?.variableOrExpression2());
            }
        }
    }

    class StartStatementChecker
    {
        public static void OnCodeElement(StartStatement statement, CodeElementsParser.StartStatementContext context)
        {
            if (context?.relationalOperator() != null)
                if (statement.RelationalOperator.Value != RelationalOperator.EqualTo &&
                    statement.RelationalOperator.Value != RelationalOperator.GreaterThan &&
                    statement.RelationalOperator.Value != RelationalOperator.GreaterThanOrEqualTo)
                    DiagnosticUtils.AddError(statement, "START: Illegal operator " + statement.RelationalOperator.Value,
                        context.relationalOperator());
        }
    }

    #endregion

}
