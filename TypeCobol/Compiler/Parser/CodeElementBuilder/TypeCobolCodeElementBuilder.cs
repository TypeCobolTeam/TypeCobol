﻿namespace TypeCobol.Compiler.Parser {

	using System.Collections.Generic;
	using TypeCobol.Compiler.AntlrUtils;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.CodeElements.Functions;
	using TypeCobol.Compiler.Parser.Generated;
	using TypeCobol.Compiler.Scanner;



internal partial class CodeElementBuilder: CodeElementsBaseListener {

	public override void EnterFunctionDeclarationHeader(CodeElementsParser.FunctionDeclarationHeaderContext context) {
		var visibility = context.PUBLIC() != null ? AccessModifier.Public : AccessModifier.Private;
		QualifiedName name = null;
		if (context.UserDefinedWord() != null) {
			var token = ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord());
			name = new URI(token.Text);
		}
		Context = context;
		CodeElement = new FunctionDeclarationHeader(name, visibility);
	}
	public override void EnterInputPhrase(CodeElementsParser.InputPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Input = new SyntaxProperty<Passing.Mode>(Passing.Mode.Input, ParseTreeUtils.GetTokenFromTerminalNode(context.INPUT()));
		ce.Profile.InputParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Output = new SyntaxProperty<Passing.Mode>(Passing.Mode.Output, ParseTreeUtils.GetTokenFromTerminalNode(context.OUTPUT()));
		ce.Profile.OutputParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterInoutPhrase(CodeElementsParser.InoutPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Inout = new SyntaxProperty<Passing.Mode>(Passing.Mode.Inout, ParseTreeUtils.GetTokenFromTerminalNode(context.INOUT()));
		ce.Profile.InoutParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterFunctionReturningPhrase(CodeElementsParser.FunctionReturningPhraseContext context) {
		var ce = (FunctionDeclarationHeader)CodeElement;
		ce.Returning = new SyntaxProperty<Passing.Mode>(Passing.Mode.Returning, ParseTreeUtils.GetTokenFromTerminalNode(context.RETURNING()));
		if (context.parameterDescription().functionDataParameter() != null) {
			var entry = CreateFunctionDataParameter(context.parameterDescription().functionDataParameter());
			ce.Profile.ReturningParameter = new ParameterDescription(entry);
		}
	}

	private IList<ParameterDescription> CreateParameters(CodeElementsParser.ParameterDescriptionContext[] contexts) {
		var parameters = new List<ParameterDescription>();
		foreach(var context in contexts) {
			if (context.functionDataParameter() != null) {
				var data = CreateFunctionDataParameter(context.functionDataParameter());
				parameters.Add(new ParameterDescription(data));
			} else
			if (context.functionConditionParameter() != null) {
				var condition = CreateFunctionConditionParameter(context.functionConditionParameter());
				if (parameters.Count < 1) {
					var data = CreateFunctionDataParameter(condition);
					parameters.Add(new ParameterDescription(data));
				} else {
					parameters[parameters.Count-1].Add(new TypeCobol.Compiler.Nodes.DataCondition(condition));
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
		} else {
			parameter.CustomType = CobolWordsBuilder.CreateAlphanumericValue(context.cobol2002TypeClause());
			parameter.DataType = DataType.CreateCustom(parameter.CustomType.Value);
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



	public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context) {
		Context = context;
		CodeElement = new FunctionDeclarationEnd();
	}

}

}
