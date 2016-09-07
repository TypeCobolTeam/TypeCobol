namespace TypeCobol.Compiler.Parser {

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
		var ce = GetFunctionProfile();
		ce.Input = new SyntaxProperty<Passing.Mode>(Passing.Mode.Input, ParseTreeUtils.GetTokenFromTerminalNode(context.INPUT()));
		ce.Profile.InputParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context) {
		var ce = GetFunctionProfile();
		ce.Output = new SyntaxProperty<Passing.Mode>(Passing.Mode.Output, ParseTreeUtils.GetTokenFromTerminalNode(context.OUTPUT()));
		ce.Profile.OutputParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterInoutPhrase(CodeElementsParser.InoutPhraseContext context) {
		var ce = GetFunctionProfile();
		ce.Inout = new SyntaxProperty<Passing.Mode>(Passing.Mode.Inout, ParseTreeUtils.GetTokenFromTerminalNode(context.INOUT()));
		ce.Profile.InoutParameters = CreateParameters(context.parameterDescription());
	}
	public override void EnterFunctionReturningPhrase(CodeElementsParser.FunctionReturningPhraseContext context) {
		var ce = GetFunctionProfile();
		ce.Returning = new SyntaxProperty<Passing.Mode>(Passing.Mode.Returning, ParseTreeUtils.GetTokenFromTerminalNode(context.RETURNING()));
		ce.Profile.ReturningParameter = CreateParameter(context.parameterDescription());
	}

	private IList<ParameterDescription> CreateParameters(CodeElementsParser.ParameterDescriptionContext[] contexts) {
		var parameters = new List<ParameterDescription>();
		foreach(var context in contexts) {
			var parameter = CreateParameter(context);
			if (parameter != null) parameters.Add(parameter);
		}
		return parameters;
	}
	private ParameterDescription CreateParameter(CodeElementsParser.ParameterDescriptionContext context) {
		if (context.functionDataParameter() != null)
			return CreateFunctionDataParameter(context.functionDataParameter());
		if (context.functionConditionParameter() != null)
//			throw new System.NotImplementedException("TODO#249: level-88 parameters");
			System.Console.WriteLine("TODO#249: level-88 parameters");
		return null;
	}
	public ParameterDescription CreateFunctionDataParameter(CodeElementsParser.FunctionDataParameterContext context) {
		var parameter = new ParameterDescription();
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

	private string CreatePicture(CodeElementsParser.Cobol2002TypeClauseContext context) {
		if (context == null) return null;
		Token token = null;
		if (context.DATE() != null) token = ParseTreeUtils.GetTokenFromTerminalNode(context.DATE());
		if (context.UserDefinedWord() != null) token = ParseTreeUtils.GetTokenFromTerminalNode(context.UserDefinedWord());
		if (token == null) return null;
		return "TYPE:"+token.Text.ToUpper();
	}



	private FunctionDeclarationProfile GetFunctionProfile() {
		if (CodeElement is ProcedureDivisionHeader)
			CodeElement = new FunctionDeclarationProfile((ProcedureDivisionHeader)CodeElement);
		return CodeElement as FunctionDeclarationProfile;
	}

	public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context) {
		Context = context;
		CodeElement = new FunctionDeclarationEnd();
	}

}

}
