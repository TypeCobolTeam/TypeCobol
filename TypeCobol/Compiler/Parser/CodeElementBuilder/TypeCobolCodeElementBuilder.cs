namespace TypeCobol.Compiler.Parser {

	using System.Collections.Generic;
	using TypeCobol.Compiler.AntlrUtils;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Parser.Generated;
	using TypeCobol.Compiler.Scanner;



internal partial class CodeElementBuilder: CodeElementsBaseListener {

	public override void EnterLibraryCopy(CodeElementsParser.LibraryCopyContext context) {
		var copy = new LibraryCopyCodeElement();
		if (context.UserDefinedWord() != null) { // TCRFUN_LIBRARY_COPY
			var value = CobolWordsBuilder.CreateAlphanumericValue(context.UserDefinedWord());
			copy.Name = new SymbolDefinition(value, SymbolType.FileName);//TODO#278 eww!
		}
		Context = context;
		CodeElement = copy;
	}

	public override void EnterFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context) {
		var visibility = context.PUBLIC() != null ? AccessModifier.Public : AccessModifier.Private;
		SymbolDefinition name = null;
		if (context.functionNameDefinition() != null) {
			name = CobolWordsBuilder.CreateFunctionNameDefinition(context.functionNameDefinition());
        }
		Context = context;
		CodeElement = new FunctionDeclarationHeader(name, visibility);
	}
	public override void EnterInputPhrase(CodeElementsParser.InputPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Input = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.Input, ParseTreeUtils.GetTokenFromTerminalNode(context.INPUT()));
		ce.Profile.InputParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Output = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.Output, ParseTreeUtils.GetTokenFromTerminalNode(context.OUTPUT()));
		ce.Profile.OutputParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterInoutPhrase(CodeElementsParser.InoutPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Inout = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.InOut, ParseTreeUtils.GetTokenFromTerminalNode(context.INOUT()));
		ce.Profile.InoutParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterFunctionReturningPhrase(CodeElementsParser.FunctionReturningPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Returning = new SyntaxProperty<ParameterPassingDirection>(ParameterPassingDirection.Returning, ParseTreeUtils.GetTokenFromTerminalNode(context.RETURNING()));
		if (context.parameterDescription().functionDataParameter() != null) {
			var entry = CreateFunctionDataParameter(context.parameterDescription().functionDataParameter());
			ce.Profile.ReturningParameter = entry;
		}
	}

	private IList<ParameterDescriptionEntry> CreateParameters(CodeElementsParser.ParameterDescriptionContext[] contexts) {
		var parameters = new List<ParameterDescriptionEntry>();
		foreach(var context in contexts) {
			if (context.functionDataParameter() != null) {
				var data = CreateFunctionDataParameter(context.functionDataParameter());
				parameters.Add(data);
			} else
			if (context.functionConditionParameter() != null) {
				var condition = CreateFunctionConditionParameter(context.functionConditionParameter());
				if (parameters.Count < 1) {
					var data = CreateFunctionDataParameter(condition);
					parameters.Add(data);
				} else {
                     var parameter = parameters[parameters.Count - 1];
                        if (parameter.DataConditions == null) parameter.DataConditions = new List<DataConditionEntry>();
                        parameter.DataConditions.Add(condition);
				}
			}
		}
		return parameters;
	}
	private ParameterDescriptionEntry CreateFunctionDataParameter(DataConditionEntry condition) {
		var data = new ParameterDescriptionEntry();
		data.LevelNumber = condition.LevelNumber;
		data.DataName    = condition.DataName;
		data.DataType    = DataType.Unknown;
		return data;
	}
	public ParameterDescriptionEntry CreateFunctionDataParameter(CodeElementsParser.FunctionDataParameterContext context) {
		var parameter = new ParameterDescriptionEntry();
		parameter.LevelNumber = new GeneratedIntegerValue(1);
		parameter.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
		if (context.pictureClause() != null) {
			parameter.Picture = CobolWordsBuilder.CreateAlphanumericValue(context.pictureClause().pictureCharacterString);
			parameter.DataType = DataType.Create(parameter.Picture.Value);
		} else if(context.cobol2002TypeClause() != null) {
			parameter.UserDefinedDataType = CobolWordsBuilder.CreateDataTypeNameReference(context.cobol2002TypeClause().dataTypeNameReference());
			parameter.DataType = DataType.CreateCustom(parameter.UserDefinedDataType.Name);
		}
		//TODO#245: subphrases
		return parameter;
	}
	private DataConditionEntry CreateFunctionConditionParameter(CodeElementsParser.FunctionConditionParameterContext context) {
		var parameter = new DataConditionEntry();
		parameter.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber().integerValue());
		parameter.DataName = CobolWordsBuilder.CreateConditionNameDefinition(context.conditionNameDefinition());
		SetConditionValues(parameter, context.valueClauseForCondition());
		return parameter;
	}
        
    public override void ExitFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context)
    {
        // Register call parameters (shared storage areas) information at the CodeElement level
        var functionDeclarationHeader = (FunctionDeclarationHeader)CodeElement;
        var callTarget = new CallTarget() { Name = functionDeclarationHeader.FunctionName };
        int parametersCount =
            (functionDeclarationHeader.Profile.InputParameters != null ? functionDeclarationHeader.Profile.InputParameters.Count : 0)
            + (functionDeclarationHeader.Profile.OutputParameters != null ? functionDeclarationHeader.Profile.OutputParameters.Count : 0)
            + (functionDeclarationHeader.Profile.InoutParameters != null ? functionDeclarationHeader.Profile.InoutParameters.Count : 0)
            + (functionDeclarationHeader.Profile.ReturningParameter != null ? 1 : 0);
        callTarget.Parameters = new CallTargetParameter[parametersCount];
        int i = 0;
        if (functionDeclarationHeader.Profile.InputParameters != null && functionDeclarationHeader.Profile.InputParameters.Count > 0)
        {
            foreach (var param in functionDeclarationHeader.Profile.InputParameters)
            {
                callTarget.Parameters[i] = CreateCallTargetParameter(param);
                i++;
            }
        }
        if (functionDeclarationHeader.Profile.OutputParameters != null && functionDeclarationHeader.Profile.OutputParameters.Count > 0)
        {
            foreach (var param in functionDeclarationHeader.Profile.OutputParameters)
            {
                callTarget.Parameters[i] = CreateCallTargetParameter(param);
                i++;
            }
        }
        if (functionDeclarationHeader.Profile.InoutParameters != null && functionDeclarationHeader.Profile.InoutParameters.Count > 0)
        {
            foreach (var param in functionDeclarationHeader.Profile.InoutParameters)
            {
                callTarget.Parameters[i] = CreateCallTargetParameter(param);
                i++;
            }
        }
        if (functionDeclarationHeader.Profile.ReturningParameter != null)
        {
            callTarget.Parameters[i] = CreateCallTargetParameter(functionDeclarationHeader.Profile.ReturningParameter);
        }
        functionDeclarationHeader.CallTarget = callTarget;
    }
    private static CallTargetParameter CreateCallTargetParameter(ParameterDescriptionEntry param)
    {
        var symbolReference = new SymbolReference(param.DataName);
        var storageArea = new DataOrConditionStorageArea(symbolReference);
        var callParameter = new CallTargetParameter { StorageArea = storageArea };
        return callParameter;
    }

    public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context) {
		Context = context;
		CodeElement = new FunctionDeclarationEnd();
	}



	  ////////////////////
	 // PROCEDURE CALL //
	////////////////////

	public override void EnterTcCallStatement(CodeElementsParser.TcCallStatementContext context) {
		var name = CobolWordsBuilder.CreateFunctionNameReference(context.functionNameReference());
		var inputs = new List<CallSiteParameter>();
		foreach(var p in context.callInputParameter()) {
			inputs.Add(new CallSiteParameter {
					SharingMode = CreateSharingMode(p), // TCRFUN_CALL_INPUT_BY
					StorageAreaOrValue = CobolExpressionsBuilder.CreateSharedVariableOrFileName(p.sharedVariableOrFileName()),
				});
		}
		var inouts = new List<CallSiteParameter>();
		foreach(var p in context.callInoutParameter()) {
			inouts.Add(new CallSiteParameter { // TCRFUN_CALL_INOUT_AND_OUTPUT_BY_REFERENCE
					SharingMode = new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, null),
					StorageAreaOrValue = new Variable(CobolExpressionsBuilder.CreateSharedStorageArea(p.sharedStorageArea1())),
				});
		}
		var outputs = new List<CallSiteParameter>();
		foreach(var p in context.callOutputParameter()) {
			outputs.Add(new CallSiteParameter { // TCRFUN_CALL_INOUT_AND_OUTPUT_BY_REFERENCE
					SharingMode = new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, null),
					StorageAreaOrValue = new Variable(CobolExpressionsBuilder.CreateSharedStorageArea(p.sharedStorageArea1())),
				});
		}
		Context = context;
		CodeElement = new ProcedureStyleCallStatement(new ProcedureCall(name, inputs,inouts,outputs));
	}

	private SyntaxProperty<ParameterSharingMode> CreateSharingMode(CodeElementsParser.CallInputParameterContext parameter) {
		if (parameter.REFERENCE() != null) return CobolStatementsBuilder.CreateSyntaxProperty(ParameterSharingMode.ByReference, parameter.REFERENCE());
		if (parameter.CONTENT()   != null) return CobolStatementsBuilder.CreateSyntaxProperty(ParameterSharingMode.ByContent,   parameter.CONTENT());
		if (parameter.VALUE()     != null) return CobolStatementsBuilder.CreateSyntaxProperty(ParameterSharingMode.ByValue,     parameter.VALUE());
		return new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, null); // TCRFUN_CALL_INPUT_BY
	}

}

}
