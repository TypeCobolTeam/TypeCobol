using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics {

	class TypeDefinitionChecker: CodeElementListener {

		public IList<System.Type> GetCodeElements() {
			return new List<System.Type>() { typeof(TypeDefinition), typeof(DataDescriptionEntry), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext context) {
			if (!(e is TypeDefinition)) return;
			if (e is DataDescriptionEntry)
				OnDataDescriptionEntry(e as DataDescriptionEntry, context as CodeElementsParser.DataDescriptionEntryContext);
		}

		private void OnDataDescriptionEntry(DataDescriptionEntry data, CodeElementsParser.DataDescriptionEntryContext context) {
			if (!data.IsTypeDefinition) return;
			if (data.LevelNumber != 1) {
				string message = "TYPEDEF clause can only be specified for level 01 entries";
				DiagnosticUtils.AddError(data, message, context.tcExtTypedefClause());
			}
			if (data.Picture != null && (data.DataType != null && data.DataType.IsStrong)) {
				string message = "Elementary TYPEDEF cannot be STRONG";
				string rulestack = new RuleStackBuilder().GetRuleStack(context.tcExtTypedefClause());
				DiagnosticUtils.AddError(data, message, ParseTreeUtils.GetFirstToken(context.tcExtTypedefClause().STRONG()), rulestack);
			}
			if (data.RedefinesDataName != null) {
				string message = "REDEFINES clause cannot be specified with TYPEDEF clause";
				DiagnosticUtils.AddError(data, message, context.redefinesClause());
			}
			if (data.IsExternal) {
				string message = "EXTERNAL clause cannot be specified with TYPEDEF clause";
				foreach(var external in context.externalClause())
					DiagnosticUtils.AddError(data, message, external);
			}
		}
	}
}