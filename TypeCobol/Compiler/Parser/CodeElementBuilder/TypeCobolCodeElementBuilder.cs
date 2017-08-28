using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Parser
{
    using System.Collections.Generic;
    using TypeCobol.Compiler.AntlrUtils;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.Parser.Generated;
    using TypeCobol.Compiler.Scanner;
    using System.Linq;


    internal partial class CodeElementBuilder : CodeElementsBaseListener
    {
        public override void EnterLibraryCopy(CodeElementsParser.LibraryCopyContext context)
        {
            var copy = new LibraryCopyCodeElement();
            if (context.qualifiedTextName() != null)
            {
                // TCRFUN_LIBRARY_COPY
                copy.Name = CobolWordsBuilder.CreateQualifiedTextName(context.qualifiedTextName()); //TODO#278 eww!
            }
            Context = context;
            CodeElement = copy;
        }

        public override void EnterFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context)
        {
            var type = FunctionType.Undefined;
            if (context.PROCEDURE() != null) type = FunctionType.Procedure;
            if (context.FUNCTION() != null) type = FunctionType.Function;

            //TCRFUN_DEFAULT_ACCESS_MODIFIER  rule is respected here. 
            //By default a function or procedure is private even if PRIVATE keyword is not given. 
            //If PUBLIC keyword is set, the function/procedure as to be set PUBLIC. 
            var visibility = context.PUBLIC() != null ? AccessModifier.Public : AccessModifier.Private;

            SymbolDefinition name = null;
            if (context.functionNameDefinition() != null)
            {
                name = CobolWordsBuilder.CreateFunctionNameDefinition(context.functionNameDefinition());
            }
            Context = context;
            CodeElement = new FunctionDeclarationHeader(name, visibility, type);
        }

        public override void EnterInputPhrase(CodeElementsParser.InputPhraseContext context)
        {
            var ce = (FunctionDeclarationHeader) CodeElement;
            ce.Input = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.Input,
                ParseTreeUtils.GetTokenFromTerminalNode(context.INPUT()));
            ce.Profile.InputParameters = CreateParameters(context.parameterDescription());
        }

        public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context)
        {
            var ce = (FunctionDeclarationHeader) CodeElement;
            ce.Output = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.Output,
                ParseTreeUtils.GetTokenFromTerminalNode(context.OUTPUT()));
            ce.Profile.OutputParameters = CreateParameters(context.parameterDescription());
        }

        public override void EnterInoutPhrase(CodeElementsParser.InoutPhraseContext context)
        {
            var ce = (FunctionDeclarationHeader) CodeElement;
            ce.Inout = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.InOut,
                ParseTreeUtils.GetTokenFromTerminalNode(context.IN_OUT()));
            ce.Profile.InoutParameters = CreateParameters(context.parameterDescription());
        }

        public override void EnterFunctionReturningPhrase(CodeElementsParser.FunctionReturningPhraseContext context)
        {
            var ce = (FunctionDeclarationHeader) CodeElement;
            ce.Returning = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.Returning,
                ParseTreeUtils.GetTokenFromTerminalNode(context.RETURNING()));
            if (context.parameterDescription().functionDataParameter() != null)
            {
                var entry = CreateFunctionDataParameter(context.parameterDescription().functionDataParameter());
                ce.Profile.ReturningParameter = entry;
            }
        }

        private IList<ParameterDescriptionEntry> CreateParameters(
            CodeElementsParser.ParameterDescriptionContext[] contexts)
        {
            var parameters = new List<ParameterDescriptionEntry>();
            foreach (var context in contexts)
            {
                if (context.functionDataParameter() != null)
                {
                    var data = CreateFunctionDataParameter(context.functionDataParameter());
                    parameters.Add(data);
                }
                else if (context.functionConditionParameter() != null)
                {
                    var condition = CreateFunctionConditionParameter(context.functionConditionParameter());
                    if (parameters.Count < 1)
                    {
                        var data = CreateFunctionDataParameter(condition);
                        parameters.Add(data);
                    }
                    else
                    {
                        var parameter = parameters[parameters.Count - 1];
                        if (parameter.DataConditions == null) parameter.DataConditions = new List<DataConditionEntry>();
                        parameter.DataConditions.Add(condition);
                    }
                }
            }
            return parameters;
        }

        private ParameterDescriptionEntry CreateFunctionDataParameter(DataConditionEntry condition)
        {
            var data = new ParameterDescriptionEntry();
            data.LevelNumber = condition.LevelNumber;
            data.DataName = condition.DataName;
            data.DataType = DataType.Unknown;
            return data;
        }

        public ParameterDescriptionEntry CreateFunctionDataParameter(
            CodeElementsParser.FunctionDataParameterContext context)
        {
            var parameter = new ParameterDescriptionEntry();
            parameter.LevelNumber = new GeneratedIntegerValue(1);
            parameter.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
            if (context.pictureClause() != null)
            {
                parameter.Picture =
                    CobolWordsBuilder.CreateAlphanumericValue(context.pictureClause().pictureCharacterString);
                parameter.DataType = DataType.Create(parameter.Picture.Value);
            }
            else if (context.cobol2002TypeClause() != null)
            {
                parameter.UserDefinedDataType =
                    CobolWordsBuilder.CreateQualifiedDataTypeReference(context.cobol2002TypeClause());
                parameter.DataType = DataType.CreateCustom(parameter.UserDefinedDataType.Name);
            } else if (context.POINTER() != null) {
                parameter.Usage = CreateDataUsageProperty(DataUsage.Pointer, context.POINTER());
            }

            if (context.blankWhenZeroClause() != null)
            {
                var blankClauseContext = context.blankWhenZeroClause();
                Token zeroToken;
                if (blankClauseContext.ZERO() != null)
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZERO());
                }
                else if (blankClauseContext.ZEROS() != null)
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZEROS());
                }
                else
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZEROES());
                }
                parameter.IsBlankWhenZero = new SyntaxProperty<bool>(true, zeroToken);
            }

            if (context.justifiedClause() != null)
            {
                var justifiedClauseContext = context.justifiedClause();
                Token justifiedToken = null;
                if (justifiedClauseContext.JUSTIFIED() != null)
                {
                    justifiedToken = ParseTreeUtils.GetFirstToken(justifiedClauseContext.JUSTIFIED());
                }
                else
                {
                    justifiedToken = ParseTreeUtils.GetFirstToken(justifiedClauseContext.JUST());
                }
                parameter.IsJustified = new SyntaxProperty<bool>(true, justifiedToken);
            }

            if (context.synchronizedClause() != null)
            {
                var synchronizedClauseContext = context.synchronizedClause();
                if (synchronizedClauseContext.SYNCHRONIZED() != null)
                {
                    parameter.IsSynchronized = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNCHRONIZED()));
                }
                else
                {
                    parameter.IsSynchronized = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNC()));
                }
            }

            if (context.groupUsageClause() != null)
            {
                var groupUsageClauseContext = context.groupUsageClause();
                parameter.IsJustified = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(groupUsageClauseContext.NATIONAL()));
            }

            //No occurs clause because we only allow level 01

//            if (context.occursClause() != null && context.occursClause().Length > 0)
//            {
//                var occursClauseContext = context.occursClause()[0];
//                if (occursClauseContext.minNumberOfOccurences != null)
//                {
//                    parameter.MinOccurencesCount = CobolWordsBuilder.CreateIntegerValue(occursClauseContext.minNumberOfOccurences);
//                }
//                if (occursClauseContext.maxNumberOfOccurences != null)
//                {
//                    parameter.MaxOccurencesCount = CobolWordsBuilder.CreateIntegerValue(occursClauseContext.maxNumberOfOccurences);
//                }
//                if (parameter.MinOccurencesCount == null && parameter.MaxOccurencesCount != null)
//                {
//                    parameter.MinOccurencesCount = parameter.MaxOccurencesCount;
//                }
//                if (occursClauseContext.UNBOUNDED() != null)
//                {
//                    parameter.HasUnboundedNumberOfOccurences = new SyntaxProperty<bool>(true,
//                        ParseTreeUtils.GetFirstToken(occursClauseContext.UNBOUNDED()));
//                }
//                if (occursClauseContext.varNumberOfOccurences != null)
//                {
//                    parameter.OccursDependingOn = CobolExpressionsBuilder.CreateNumericVariable(occursClauseContext.varNumberOfOccurences);
//                }
//                if (occursClauseContext.tableSortingKeys() != null && occursClauseContext.tableSortingKeys().Length > 0)
//                {
//                    int keysCount = 0;
//                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
//                    {
//                        keysCount += tableSortingKeysContext.dataNameReference().Length;
//                    }
//                    parameter.TableSortingKeys = new TableSortingKey[keysCount];
//                    int keyIndex = 0;
//                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
//                    {
//                        SyntaxProperty<SortDirection> sortDirection = null;
//                        if (tableSortingKeysContext.ASCENDING() != null)
//                        {
//                            sortDirection = new SyntaxProperty<SortDirection>(SortDirection.Ascending,
//                                ParseTreeUtils.GetFirstToken(tableSortingKeysContext.ASCENDING()));
//                        }
//                        else
//                        {
//                            sortDirection = new SyntaxProperty<SortDirection>(SortDirection.Descending,
//                                ParseTreeUtils.GetFirstToken(tableSortingKeysContext.DESCENDING()));
//                        }
//                        foreach (var dataNameReference in tableSortingKeysContext.dataNameReference())
//                        {
//                            SymbolReference sortKey = CobolWordsBuilder.CreateDataNameReference(dataNameReference);
//                            parameter.TableSortingKeys[keyIndex] = new TableSortingKey(sortKey, sortDirection);
//                            keyIndex++;
//                        }
//                    }
//                }
//                if (occursClauseContext.indexNameDefinition() != null && occursClauseContext.indexNameDefinition().Length > 0)
//                {
//                    parameter.Indexes = new SymbolDefinition[occursClauseContext.indexNameDefinition().Length];
//                    for (int i = 0; i < occursClauseContext.indexNameDefinition().Length; i++)
//                    {
//                        var indexNameDefinition = occursClauseContext.indexNameDefinition()[i];
//                        parameter.Indexes[i] = CobolWordsBuilder.CreateIndexNameDefinition(indexNameDefinition);
//                    }
//                }
//            }

            if (context.signClause() != null)
            {
                var signClauseContext = context.signClause();
                if (signClauseContext.LEADING() != null)
                {
                    parameter.SignPosition = new SyntaxProperty<SignPosition>(SignPosition.Leading,
                        ParseTreeUtils.GetFirstToken(signClauseContext.LEADING()));
                }
                else
                {
                    parameter.SignPosition = new SyntaxProperty<SignPosition>(SignPosition.Trailing,
                        ParseTreeUtils.GetFirstToken(signClauseContext.TRAILING()));
                }
                if (signClauseContext.SEPARATE() != null)
                {
                    parameter.SignIsSeparate = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(signClauseContext.SEPARATE()));
                }
            }

            //As POINTER can already be defined in Usage property, we don't want to overwrite it
            if (parameter.Usage == null && context.tcfuncParameterUsageClause() != null)
            {
                parameter.Usage = CreateTCFuncParameterUsageClause(context.tcfuncParameterUsageClause());

            }

            if (context.valueClause() != null)
            {
                var valueClauseContext = context.valueClause();
                parameter.InitialValue = CobolWordsBuilder.CreateValue(valueClauseContext.value2());
            }

            return parameter;
        }

        private SyntaxProperty<DataUsage> CreateTCFuncParameterUsageClause(CodeElementsParser.TcfuncParameterUsageClauseContext c) {
            return CreateDataUsageProperty(DataUsage.Binary, c.BINARY()) ??
                   CreateDataUsageProperty(DataUsage.Binary, c.COMP()) ??
                   CreateDataUsageProperty(DataUsage.Binary, c.COMPUTATIONAL()) ??
                   CreateDataUsageProperty(DataUsage.Binary, c.COMP_4()) ??
                   CreateDataUsageProperty(DataUsage.Binary, c.COMPUTATIONAL_4()) ??
                   CreateDataUsageProperty(DataUsage.FloatingPoint, c.COMP_1()) ??
                   CreateDataUsageProperty(DataUsage.FloatingPoint, c.COMPUTATIONAL_1()) ??
                   CreateDataUsageProperty(DataUsage.LongFloatingPoint, c.COMP_2()) ??
                   CreateDataUsageProperty(DataUsage.LongFloatingPoint, c.COMPUTATIONAL_2()) ??
                   CreateDataUsageProperty(DataUsage.PackedDecimal, c.PACKED_DECIMAL()) ??
                   CreateDataUsageProperty(DataUsage.PackedDecimal, c.COMP_3()) ??
                   CreateDataUsageProperty(DataUsage.PackedDecimal, c.COMPUTATIONAL_3()) ??
                   CreateDataUsageProperty(DataUsage.NativeBinary, c.COMP_5()) ??
                   CreateDataUsageProperty(DataUsage.NativeBinary, c.COMPUTATIONAL_5()) ??
                   CreateDataUsageProperty(DataUsage.Display, c.DISPLAY()) ??
                   CreateDataUsageProperty(DataUsage.DBCS, c.DISPLAY_1()) ??
                   CreateDataUsageProperty(DataUsage.Index, c.INDEX()) ??
                   CreateDataUsageProperty(DataUsage.National, c.NATIONAL());
        }

        private DataConditionEntry CreateFunctionConditionParameter(
            CodeElementsParser.FunctionConditionParameterContext context)
        {
            var parameter = new DataConditionEntry();
            parameter.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber);
            parameter.DataName = CobolWordsBuilder.CreateConditionNameDefinition(context.conditionNameDefinition());
            SetConditionValues(parameter, context.valueClauseForCondition());
            return parameter;
        }

        public override void ExitFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context)
        {
            // Register call parameters (shared storage areas) information at the CodeElement level
            var function = (FunctionDeclarationHeader) CodeElement;
            var target = new CallTarget() {Name = function.FunctionName};
            int parametersCount = function.Profile.InputParameters.Count
                                  + function.Profile.InoutParameters.Count
                                  + function.Profile.OutputParameters.Count
                                  + (function.Profile.ReturningParameter != null ? 1 : 0);
            target.Parameters = new CallTargetParameter[parametersCount];
            int i = 0;
            foreach (var param in function.Profile.InputParameters)
            {
                target.Parameters[i++] = CreateCallTargetParameter(param);
            }
            foreach (var param in function.Profile.OutputParameters)
            {
                target.Parameters[i++] = CreateCallTargetParameter(param);
            }
            foreach (var param in function.Profile.InoutParameters)
            {
                target.Parameters[i++] = CreateCallTargetParameter(param);
            }
            if (function.Profile.ReturningParameter != null)
            {
                target.Parameters[i++] = CreateCallTargetParameter(function.Profile.ReturningParameter);
            }
            function.CallTarget = target;

            Context = context;
            CodeElement = function;
        }

        private static CallTargetParameter CreateCallTargetParameter(ParameterDescriptionEntry param)
        {
            var symbolReference = new SymbolReference(param.DataName);
            var storageArea = new DataOrConditionStorageArea(symbolReference);
            var callParameter = new CallTargetParameter {StorageArea = storageArea};
            return callParameter;
        }

        public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context)
        {
            Context = context;
            CodeElement = new FunctionDeclarationEnd();
        }


        ////////////////////
        // PROCEDURE CALL //
        ////////////////////

        public override void EnterTcCallStatement(CodeElementsParser.TcCallStatementContext context)
        {
            var cbCallProc = context.procedurePointerOrFunctionPointerVariableOrfunctionNameReference;

            // Register call parameters (shared storage areas) information at the CodeElement level
            CallSite callSite = null;
            ProcedureStyleCallStatement statement = null;
            Context = context;

            //Incomplete CallStatement, create an empty CodeElement and return
            if (cbCallProc == null)
            {
                CodeElement = new ProcedureStyleCallStatement();
                return;
            }


            //Here ambiguousSymbolReference with either CandidatesType:
            // - ProgramNameOrProgramEntry
            // - data, condition, UPSISwitch, TCFunctionName
            var ambiguousSymbolReference = CobolExpressionsBuilder.CreateProcedurePointerOrFunctionPointerVariableOrTCFunctionProcedure(cbCallProc);
          

            //If (inputs, inouts ou outputs).Count > 0, then it's a procedure call
            if (context.callOutputParameter().Length > 0 || context.callInputParameter().Length > 0 || context.callInoutParameter().Length > 0)
            {
                callSite = new CallSite();
                #region Setup Input Output Inout CallSitesParameters

                var inputs = new List<CallSiteParameter>();
                var inouts = new List<CallSiteParameter>();
                var outputs = new List<CallSiteParameter>();

                SyntaxProperty<ParameterSharingMode> mode = null;
                foreach (var p in context.callInputParameter())
                {
                    CreateSharingMode(p, ref mode); // TCRFUN_INPUT_BY
                    inputs.Add(new CallSiteParameter
                    {
                        SharingMode = mode,
                        StorageAreaOrValue =
                            CobolExpressionsBuilder.CreateSharedVariableOrFileName(p.sharedVariableOrFileName()),
                    });
                }

                foreach (var p in context.callInoutParameter())
                {
                    inouts.Add(new CallSiteParameter
                    {
                        // TCRFUN_CALL_INOUT_AND_OUTPUT_BY_REFERENCE
                        SharingMode =
                            new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, null),
                        StorageAreaOrValue =
                            new Variable(CobolExpressionsBuilder.CreateSharedStorageArea(p.sharedStorageArea1())),
                    });
                }

                foreach (var p in context.callOutputParameter())
                {
                    outputs.Add(new CallSiteParameter
                    {
                        // TCRFUN_CALL_INOUT_AND_OUTPUT_BY_REFERENCE
                        SharingMode =
                            new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, null),
                        StorageAreaOrValue =
                            new Variable(CobolExpressionsBuilder.CreateSharedStorageArea(p.sharedStorageArea1())),
                    });
                }

                int parametersCount = inputs.Count + outputs.Count + inouts.Count;
                callSite.Parameters = new CallSiteParameter[parametersCount];
                int i = 0;

                //Add inputs to global callsites parameters
                if (inputs.Count > 0)
                {
                    foreach (var param in inputs)
                    {
                        callSite.Parameters[i] = param;
                        i++;
                    }
                }

                //Add outputs to global callsites parameters
                if (outputs.Count > 0)
                {
                    foreach (var param in outputs)
                    {
                        callSite.Parameters[i] = param;
                        i++;
                    }
                }

                //Add inouts to global callsites parameters
                if (inouts.Count > 0)
                {
                    foreach (var param in inouts)
                    {
                        callSite.Parameters[i] = param;
                        i++;
                    }
                }

                #endregion

                //Check Type or CandidatesTypes to see if a TCFunctionName is possible
                if (ambiguousSymbolReference.MainSymbolReference != null &&
                    ambiguousSymbolReference.MainSymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName))
                {
                    SymbolReferenceVariable TCFunctionNameRefVariable;

                    if (ambiguousSymbolReference.MainSymbolReference.IsQualifiedReference)
                    {
                        var nonAmbiguousHead =
                            new SymbolReference(
                                (ambiguousSymbolReference.MainSymbolReference as TypeCobolQualifiedSymbolReference).Head
                                .NameLiteral, SymbolType.TCFunctionName);
                        var nonAmbiguousTail =
                            new SymbolReference(
                                (ambiguousSymbolReference.MainSymbolReference as TypeCobolQualifiedSymbolReference).Tail
                                .NameLiteral, SymbolType.ProgramName);

                        TypeCobolQualifiedSymbolReference newQualifiedSymbolReferece =
                            new TypeCobolQualifiedSymbolReference(nonAmbiguousHead, nonAmbiguousTail);
                        TCFunctionNameRefVariable = new SymbolReferenceVariable(StorageDataType.MethodName,
                            newQualifiedSymbolReferece);
                    }
                    else
                    {
                        //If so, create ProcedureStyleCallStatement with a ProcedureCall and fix SymbolReference so it's not ambiguous
                        var nonAmbiguousSymbolRef =
                            new SymbolReference(ambiguousSymbolReference.MainSymbolReference.NameLiteral,
                                SymbolType.TCFunctionName);
                        TCFunctionNameRefVariable = new SymbolReferenceVariable(StorageDataType.MethodName,
                            nonAmbiguousSymbolRef);
                    }

                    //CobolExpressionsBuilder store every StorageArea created into storageAreaReads and then after
                    //storageAreaReads is set to the CodeElement
                    //We must remove it as TCFunctionNameRefVariable doesn't contains a StorageArea
                    if (ambiguousSymbolReference.StorageArea != null)
                    {
                        CobolExpressionsBuilder.storageAreaReads.Remove(ambiguousSymbolReference.StorageArea);
                    }

                    statement =
                        new ProcedureStyleCallStatement(new ProcedureCall(TCFunctionNameRefVariable.MainSymbolReference,
                            inputs, inouts, outputs))
                        {
                            ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction =
                                TCFunctionNameRefVariable.MainSymbolReference
                        };

                    callSite.CallTarget = TCFunctionNameRefVariable.MainSymbolReference;
                }
                else
                {
                    //else it's an error
                    statement =
                        new ProcedureStyleCallStatement(new ProcedureCall(ambiguousSymbolReference.MainSymbolReference,
                            inputs, inouts, outputs))
                        {
                            ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction =
                                ambiguousSymbolReference.MainSymbolReference,
                        };
                    statement.Diagnostics.Add(new Diagnostic(MessageCode.ImplementationError, context.Start.Column,
                        context.Stop.Column, context.Start.Line, "A call with arguments is not a TCFunctionName"));
                    callSite.CallTarget = ambiguousSymbolReference.MainSymbolReference;
                }
            }
            else
            {
                if (ambiguousSymbolReference.MainSymbolReference != null &&
                         ambiguousSymbolReference.MainSymbolReference.IsOrCanBeOfType(SymbolType.DataName, SymbolType.TCFunctionName)) {


                    //TODO think about uniformity of CandidateTypes inside QualifiedReference.
                    //Maybe just define CandidatesTypes for the Head...
                    if (ambiguousSymbolReference.MainSymbolReference.IsQualifiedReference) {
                        var qualifiedSymbolReference = (QualifiedSymbolReference) ambiguousSymbolReference.MainSymbolReference;
                        if (qualifiedSymbolReference.Head.IsAmbiguous) {
                            ((AmbiguousSymbolReference) qualifiedSymbolReference.Head).CandidateTypes = new[]{SymbolType.DataName, SymbolType.TCFunctionName};
                        }
                        AmbiguousSymbolReference.ApplyCandidatesTypes(qualifiedSymbolReference.Tail, new[] {SymbolType.DataName, SymbolType.ProgramName});

                    } else {
                        ((AmbiguousSymbolReference) ambiguousSymbolReference.MainSymbolReference).CandidateTypes = new[] {SymbolType.DataName, SymbolType.TCFunctionName};
                    }

                    statement = new ProcedureStyleCallStatement(new ProcedureCall(ambiguousSymbolReference.MainSymbolReference,
                            null, null, null))
                        {
                            ProcdurePointerOrTCProcedureFunction = ambiguousSymbolReference.MainSymbolReference
                        };
                    callSite = new CallSite();
                    callSite.CallTarget = statement.ProcdurePointerOrTCProcedureFunction;
                } else { //else, it's an error
                    statement =
                        new ProcedureStyleCallStatement(new ProcedureCall(ambiguousSymbolReference.MainSymbolReference,
                            null, null, null))
                        {
                            ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction =
                                ambiguousSymbolReference.MainSymbolReference,
                        };
                    statement.Diagnostics.Add(new Diagnostic(MessageCode.SyntaxErrorInParser, context.Start.Column,
                        context.Stop.Column, context.Start.Line, "Error in detecting Procedure Call type"));
                    callSite = new CallSite();
                    callSite.CallTarget = statement.ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction;
                }
            }


            if (callSite != null) {
                if (statement.CallSites == null) statement.CallSites = new List<CallSite>();
                statement.CallSites.Add(callSite);
            }

            CodeElement = statement;
        }

        private void CreateSharingMode(CodeElementsParser.CallInputParameterContext parameter,
            ref SyntaxProperty<ParameterSharingMode> mode)
        {
            if (parameter.REFERENCE() != null)
            {
                mode = CobolStatementsBuilder.CreateSyntaxProperty(ParameterSharingMode.ByReference,
                    parameter.REFERENCE());
            }
            else if (parameter.CONTENT() != null)
            {
                mode = CobolStatementsBuilder.CreateSyntaxProperty(ParameterSharingMode.ByContent, parameter.CONTENT());
            }
            else if (parameter.VALUE() != null)
            {
                mode = CobolStatementsBuilder.CreateSyntaxProperty(ParameterSharingMode.ByValue, parameter.VALUE());
            }
            else
            {
                var by = ParameterSharingMode.ByReference;
                if (mode != null) by = mode.Value;
                mode = new SyntaxProperty<ParameterSharingMode>(by, null);
            }
        }
    }
}