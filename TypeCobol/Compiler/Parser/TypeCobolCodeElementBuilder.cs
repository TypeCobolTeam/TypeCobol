using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
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
			var profile = new FunctionDeclarationProfile(CodeElement as ProcedureDivisionHeader);
			profile.InputParameters = CobolStatementsBuilder.CreateInputParameters(context.programInputParameters());
			CodeElement = profile;
		}
		public override void EnterOutputPhrase(CodeElementsParser.OutputPhraseContext context) {
			var div = CodeElement as ProcedureDivisionHeader;
			if (div != null) CodeElement = new FunctionDeclarationProfile(div);
			var profile = (FunctionDeclarationProfile)CodeElement;
			foreach(var output in context.storageArea2())
				profile.OutputParameters.Add(CobolExpressionsBuilder.CreateStorageArea(output));
		}
		public override void EnterFunctionDeclarationEnd(CodeElementsParser.FunctionDeclarationEndContext context) {
			Context = context;
			CodeElement = new FunctionDeclarationEnd();
		}

    }
}
