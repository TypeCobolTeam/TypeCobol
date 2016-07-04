using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics {

	class DataDescriptionChecker: CodeElementListener {

		public IList<Type> GetCodeElements() {
			return new List<Type> { typeof(DataDescriptionEntry) };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var data = e as DataDescriptionEntry;
			if (c is CodeElementsParser.DataDescriptionEntryContext)
				OnDataDescriptionEntry(data, c as CodeElementsParser.DataDescriptionEntryContext);
			if (c is CodeElementsParser.DataConditionEntryContext)
				OnDataConditionEntry(data, c as CodeElementsParser.DataConditionEntryContext);
			if (c is CodeElementsParser.DataRenamesEntryContext)
				OnDataRenamesEntry(data, (CodeElementsParser.DataRenamesEntryContext) c);
		}

		private void OnDataDescriptionEntry(DataDescriptionEntry data, CodeElementsParser.DataDescriptionEntryContext context) {
			var picture   = GetContext(data, context.pictureClause());
			var blank     = GetContext(data, context.blankWhenZeroClause());
			var external  = GetContext(data, context.externalClause());
			var global    = GetContext(data, context.globalClause());
			var justified = GetContext(data, context.justifiedClause());
			var sync      = GetContext(data, context.synchronizedClause());
			var group     = GetContext(data, context.groupUsageClause());
			var usage     = GetContext(data, context.usageClause());
			var sign      = GetContext(data, context.signClause());
			var occurs    = GetContext(data, context.occursClause());
			var value     = GetContext(data, context.valueClause());


			if (data.Name == null)
			{
				if (data.IsExternal)
					DiagnosticUtils.AddError(data, "Data name must be specified for any entry containing the EXTERNAL clause",
						external);
				if (data.IsGlobal)
					DiagnosticUtils.AddError(data, "Data name must be specified for any entry containing the GLOBAL clause",
						global);
			} else { 
				if (data.IsExternal && data.LevelNumber != 01)
				{
					DiagnosticUtils.AddError(data, "External is only allowed for level 01", external);
				}
			}
		}
		private void OnDataConditionEntry(DataDescriptionEntry data, CodeElementsParser.DataConditionEntryContext context) {
			if (data.LevelNumber != 88)
				DiagnosticUtils.AddError(data, "Data conditions must be level 88", context.levelNumber());
			if (data.Name == null && !data.IsFiller)
				DiagnosticUtils.AddError(data, "Data name must be specified for level-88 items", context.levelNumber());
		}
		private void OnDataRenamesEntry(DataDescriptionEntry data, CodeElementsParser.DataRenamesEntryContext context) {


			if (data.LevelNumber != 66)
			{
				//(source page 379 of ISO Cobol 2014) 
				DiagnosticUtils.AddError(data, "RENAMES must be level 66", context.levelNumber());
			}
			if (data.Name == null && !data.IsFiller)
			{
				//(source page 379 of ISO Cobol 2014) 
				DiagnosticUtils.AddError(data, "Data name must be specified for level-66 items", context.levelNumber());
			}
			if (data.RenamesFromDataName.Equals(data.RenamesToDataName))
			{
				//(source page 379 of ISO Cobol 2014) 
				DiagnosticUtils.AddError(data, "Renamed items can't be the same " + data.RenamesFromDataName + " and " + data.RenamesToDataName, context);
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
		public static T GetContext<T>(CodeElement e, T[] contexts, bool checkErrors = true) where T: Antlr4.Runtime.ParserRuleContext {
			if (contexts == null) return null;
			if (contexts.Length < 1) return null;
			if (checkErrors) {
				for (int c = 1; c < contexts.Length; c++)
					DiagnosticUtils.AddError(e, "Only one such clause allowed", contexts[c]);
			}
			return contexts[0];
		}
	}

	class AddStatementChecker: CodeElementListener
	{
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(AddStatement), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var statement = e as AddStatement;
			var context = c as CodeElementsParser.AddStatementFormat2Context;
			if (context == null) return; //we only check format 2
			if (context.GIVING() == null)
				DiagnosticUtils.AddError(statement, "Required: <identifier> after TO", context.identifierOrNumericLiteralTmp());
		}
	}

	class CallStatementChecker: CodeElementListener
	{
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(CallStatement), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var statement = e as CallStatement;
			var context = c as CodeElementsParser.CallStatementContext;

			//foreach (var call in context.callBy()) CheckCallUsings(statement, call);

			if (context.callReturning() != null && statement.Returning == null)
				DiagnosticUtils.AddError(statement, "CALL .. RETURNING: Missing identifier", context.callReturning());
		}

		private void CheckCallUsings(CallStatement statement, CodeElementsParser.CallByContext context){
			foreach(var e in statement.Usings) {
				if (e.Identifier != null) {
					if (e.Identifier as FunctionReference != null)
						DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal function identifier", context);
					if (e.Identifier as LinageCounter != null)
						DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LINAGE COUNTER", context);
					if (e.UsingMode == CallStatement.Using.Mode.REFERENCE && e.Identifier as Length != null)
						DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LENGTH OF in BY REFERENCE phrase", context);
					//TODO what about special registers ?
				}
				if (e.Literal != null) {
					if (e.UsingMode == CallStatement.Using.Mode.REFERENCE)
						DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal <literal> in BY REFERENCE phrase", context);
				}
				if (e.Filename != null) {
					if (e.UsingMode == CallStatement.Using.Mode.CONTENT || e.UsingMode == CallStatement.Using.Mode.VALUE)
						DiagnosticUtils.AddError(statement, "CALL .. USING: <filename> only allowed in BY REFERENCE phrase", context);
				}
			}
		}
	}

	class CancelStatementChecker: CodeElementListener
	{
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(CancelStatement), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext ctxt) {
			var statement = e as CancelStatement;
			var context = ctxt as CodeElementsParser.CancelStatementContext;

			foreach (var item in statement.Items)
			{
				var literal = item as TypeCobol.Compiler.CodeElements.Expressions.Literal;
				if (literal != null && (literal.Value is double || literal.Value is long)) {
					// we should link this error to the specific context.identifierOrLiteral[i] context
					// corresponding to statement.Items[i], but since refactor in #157 it's not trivial anymore
					DiagnosticUtils.AddError(statement, "CANCEL: <literal> must be alphanumeric", context);
				}
			}
		}
	}

	class SetStatementChecker: CodeElementListener
	{
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(TypeCobol.Compiler.CodeElements.SetStatementForIndex), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var sa = e as TypeCobol.Compiler.CodeElements.SetStatementForAssignation;
			if (sa != null) {
				var ca = c as CodeElementsParser.SetStatementForAssignationContext;
				for (int i = 0; i < ca.identifier().Length; i++) {
					if (i >= sa.Receiving.Count) {
						var ctxt = ca.identifier()[i];
						DiagnosticUtils.AddError(sa, "Set: Receiving fields missing or type unknown before TO", ctxt);
					}
				}
				if (sa.Sending == null) {
					DiagnosticUtils.AddError(sa, "Set: Sending field missing or type unknown after TO", ca.setStatementForAssignationSending());
				}
			}
			var si = e as TypeCobol.Compiler.CodeElements.SetStatementForIndex;
			if (si != null) {
				if (si.SendingField == null) {
					var ci = c as CodeElementsParser.SetStatementForIndexesContext;
					DiagnosticUtils.AddError(si, "Set xxx up/down by xxx: Sending field missing or type unknown", ci);
				}
			}
		}
	}

	class StartStatementChecker: CodeElementListener
	{
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(StartStatement), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var statement = e as StartStatement;
			var context = c as CodeElementsParser.StartStatementContext;
			if (context.relationalOperator() != null)
				if (statement.Operator != '=' && statement.Operator != '>' && statement.Operator != '≥')
					DiagnosticUtils.AddError(statement, "START: Illegal operator "+statement.Operator, context.relationalOperator());
		}
	}

	class StopStatementChecker: CodeElementListener
	{
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(StopStatement), };
		}
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var statement = e as StopStatement;
			var context = c as CodeElementsParser.StopStatementContext;
			if (context.literal() != null)
				if (statement.Literal != null && statement.Literal.All)
					DiagnosticUtils.AddError(statement, "STOP: Illegal ALL", context.literal());
		}
	}



	class DeclarationChecker: NodeListener {
		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(TypeCobol.Compiler.CodeModel.IdentifierUser), };
		}
		public void OnNode(Node node, ParserRuleContext c, Program program) {
			var element = node.CodeElement as TypeCobol.Compiler.CodeModel.IdentifierUser;
			foreach (var identifier in element.Identifiers) {
				CheckIdentifier(node.CodeElement, program.SymbolTable, identifier);
			}
		}
		private static void CheckIdentifier(CodeElement e, SymbolTable table, Identifier identifier) {
			var found = table.Get(identifier.Name);
			if (found.Count < 1) {
				if (table.GetFunction(identifier.Name) == null)
					DiagnosticUtils.AddError(e, "Symbol "+identifier.Name+" is not referenced");
			}
			if (found.Count > 1)
				DiagnosticUtils.AddError(e, "Ambiguous reference to symbol "+identifier.Name);

			var fun = identifier as FunctionReference;
			if (fun != null) foreach(var param in fun.Parameters) CheckExpression(e, table, param.Value as Expression);

			foreach(var def in found)
				foreach(var error in checkSubscripting(identifier.Name, def))
					DiagnosticUtils.AddError(e, error);
		}
		private static void CheckExpression(CodeElement e, SymbolTable table, Expression expression) {
			if (expression == null) return;
			if (expression is Identifier) CheckIdentifier(e, table, expression as Identifier);
			if (expression is ArithmeticOperation) {
				var aerith = expression as ArithmeticOperation;
				CheckExpression(e, table, aerith.left);
				CheckExpression(e, table, aerith.right);
			}
		}

		private static IEnumerable<string> checkSubscripting(QualifiedName qname, DataDescriptionEntry data) {
			var errors = new List<string>();
			if (qname is Subscripted) {
				var sname = qname as Subscripted;
				for(int i=qname.Count-1; i>=0; i--) {
					string name = qname[i];
					var subscript = sname[name];
					if (subscript != null) {
						if (!data.IsTableOccurence) {
							errors.Add(name+" must not be subscripted.");
						} else
						if (subscript.IsJustAnOffset) {
							int offset = int.Parse(subscript.offset.ToString());
							if (offset > data.MaxOccurencesCount)
								errors.Add(name+" subscripting out of bounds: "+offset+" > max="+data.MaxOccurencesCount);
						}//else TODO: check if subscript.dataname subscripting is okay too
					} else {
						if (data.IsTableOccurence) {
							errors.Add(name+" must be subscripted.");
						}
					}
					data = data.TopLevel;
				}
			}
			return errors;
		}
	}

	class WriteOperationsChecker: NodeListener {

		public IList<Type> GetCodeElements() {
			return new List<Type>() { typeof(TypeCobol.Compiler.CodeModel.SymbolWriter), };
		}
		public void OnNode(Node node, ParserRuleContext c, Program program) {
			var element = node.CodeElement as TypeCobol.Compiler.CodeModel.SymbolWriter;
			if (element== null || element.IsUnsafe) return; // nothing to do
			var table = program.SymbolTable;
			foreach(var pair in element.Symbols) {
				if (pair.Item1 == null) continue; // no sending item
				if (pair.Item2 == null) continue; // no receiving item
				DataType sending = pair.Item1.Item2;
				if (sending == null) { // unknown sending item type
					if (pair.Item1.Item1 == null) continue; // ?no sending item
					var ls = table.Get(pair.Item1.Item1);
					if (ls.Count != 1) continue; // ambiguity or not referenced; not my job
					sending = ls[0].DataType;
				}
				var lr = table.Get(pair.Item2);
				if (lr.Count != 1) continue; // ambiguity or not referenced; not my job
				var receiving = lr[0];
				if (receiving.DataType != sending && receiving.DataType.IsStrong) {
					string message = "Can't write non typed "+sending+" to strongly typed variable "+receiving.Name+":"+receiving.DataType+" (use unsafe keyword for that)";
					DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
				}
				CheckNesting(node.CodeElement, receiving);
			}
		}
		private bool CheckNesting(CodeElement e, DataDescriptionEntry data) {
			foreach(var sub in data.Subordinates) {
				if (sub.DataType != null && !sub.DataType.IsNestable) {
					DiagnosticUtils.AddError(e, "Group contains type "+sub.DataType.Name+" variables");
					return false;
				} else if (!CheckNesting(e, sub)) return false;
			}
			return true;
		}
	}

}
