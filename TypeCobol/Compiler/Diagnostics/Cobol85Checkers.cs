using JetBrains.Annotations;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Diagnostics {

    using Antlr4.Runtime;
    using System;
    using System.Collections.Generic;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.CodeModel;
    using TypeCobol.Compiler.Nodes;
    using TypeCobol.Compiler.Parser;
    using TypeCobol.Compiler.Parser.Generated;
    using TypeCobol.Tools;
    using System.Text.RegularExpressions;

    public class Cobol85CompleteASTChecker : AbstractAstVisitor
    {
        private Node CurrentNode {get;set;}
        public override bool BeginNode(Node node)
        {
            CurrentNode = node;

            CodeElement codeElement = node.CodeElement;
            if (codeElement != null && codeElement.StorageAreaReads != null)
            {
                foreach (var storageAreaRead in codeElement.StorageAreaReads)
                {
                    CheckVariable(node, storageAreaRead);
                }
            }
            if (codeElement != null && codeElement.StorageAreaWrites != null)
            {
                foreach (var storageAreaWrite in codeElement.StorageAreaWrites)
                {
                    CheckVariable(node, storageAreaWrite);
                }
            }

            

            FunctionCallChecker.OnNode(node);
            TypedDeclarationChecker.OnNode(node);
            RenamesChecker.OnNode(node);
            ReadOnlyPropertiesChecker.OnNode(node);
            return true;
        }



        public override bool BeginCodeElement(CodeElement codeElement) {
            //This checker is only for Node after the full AST has been created
            return false;
        }

        public override bool Visit(PerformProcedure performProcedureNode) {
            SectionOrParagraphUsageChecker.CheckReferenceToParagraphOrSection(performProcedureNode);
            return true;
        }

        public override bool Visit(Paragraph paragraph) {
            SectionOrParagraphUsageChecker.CheckParagraph(paragraph);
            return true;
        }

        public override bool Visit(ProcedureDivision procedureDivision) {
            LibraryChecker.CheckLibrary(procedureDivision);
            return true;
        }

        public override bool Visit(Section section) {
            SectionOrParagraphUsageChecker.CheckSection(section);
            return true;
        }

        public override bool Visit(TypeDefinition typeDefinition) {
            //Cobol 2002 rule
            //TODO need to clarify if we have 1 visitor per LanguageLevel
            //For performance reason it seems better to have only one here
            TypeDefinitionChecker.CheckTypeDefinition(typeDefinition);
            return true;
        }


        public override bool VisitVariableWriter(VariableWriter variableWriter) {
            WriteTypeConsistencyChecker.OnNode(variableWriter, CurrentNode);
            return true;
        }

        public override bool Visit(DataDefinition dataDefinition)
        {
            if(dataDefinition.CodeElement is CommonDataDescriptionAndDataRedefines)
            {
                CheckPicture((dataDefinition.CodeElement as CommonDataDescriptionAndDataRedefines));
            }
            return true;
        }

        public static void CheckPicture(CommonDataDescriptionAndDataRedefines codeElement)
        {
            if (codeElement.Picture != null)
            {
                foreach (Match match in Regex.Matches(codeElement.Picture.Value, @"\(([^)]*)\)"))
                {
                    try //Try catch is here beacause of the risk to parse a non numerical value
                    {
                        int value = int.Parse(match.Value, System.Globalization.NumberStyles.AllowParentheses);
                    }
                    catch (Exception)
                    {
                        var m = "Given value is not correct : " + match.Value + " expected numerical value only";
                        DiagnosticUtils.AddError(codeElement, m);
                    }
                }
            }
        }

        private void CheckVariable(Node node, VariableBase variable)
        {
            if (variable.StorageArea != null)
            {
                CheckVariable(node, variable.StorageArea);
            }
        }

        private void CheckVariable(Node node, StorageArea storageArea) {
            if (!storageArea.NeedDeclaration)
            {
                return;
            }
            var area = storageArea.GetStorageAreaThatNeedDeclaration;

            if (area.SymbolReference == null) return;
            //Do not handle TCFunctionName, it'll be done by TypeCobolChecker
            if (area.SymbolReference.IsOrCanBeOfType(SymbolType.TCFunctionName)) return;

            var found = node.SymbolTable.GetVariable(area);
            if (found.Count < 1)
                if (node.SymbolTable.GetFunction(area).Count < 1)
                    DiagnosticUtils.AddError(node.CodeElement, "Symbol " + area + " is not referenced");
            if (found.Count > 1) DiagnosticUtils.AddError(node.CodeElement, "Ambiguous reference to symbol " + area);

        }
        }



    class DataDescriptionChecker: CodeElementListener {

		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var data = e as DataDescriptionEntry;
		    if (data == null) {
		        return; //not our job
		    }
			var context = c as CodeElementsParser.DataDescriptionEntryContext;
			var external  = GetContext(data, context.externalClause());
			var global    = GetContext(data, context.globalClause());
			if (data.DataName == null) {
				if (!data.IsFiller)
					DiagnosticUtils.AddError(data, "Data name or FILLER expected", context.dataNameDefinition());
				if (data.IsExternal)
					DiagnosticUtils.AddError(data, "Data name must be specified for any entry containing the EXTERNAL clause", external);
				if (data.IsGlobal)
					DiagnosticUtils.AddError(data, "Data name must be specified for any entry containing the GLOBAL clause", global);
			} else {
				if (data.IsExternal && data.LevelNumber.Value != 01)
					DiagnosticUtils.AddError(data, "External is only allowed for level 01", external);
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

	class DataConditionChecker: CodeElementListener {

		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var data = e as DataConditionEntry;
            if (data == null) {
                return; //not our job
            }
            var context = c as CodeElementsParser.DataConditionEntryContext;
			if (data.LevelNumber.Value != 88)
				DiagnosticUtils.AddError(data, "Data conditions must be level 88", context.levelNumber);
			if (data.DataName == null)
				DiagnosticUtils.AddError(data, "Data name must be specified for level-88 items", context.levelNumber);
		}
	}

	class DataRenamesChecker: CodeElementListener {

		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var data = e as DataRenamesEntry;
            if (data == null) {
                return; //not our job
            }
            var context = c as CodeElementsParser.DataConditionEntryContext;
			if (data.LevelNumber.Value != 66)
				//(source page 379 of ISO Cobol 2014)
				DiagnosticUtils.AddError(data, "RENAMES must be level 66", context.levelNumber);
			if (data.DataName == null)
				//(source page 379 of ISO Cobol 2014)
				DiagnosticUtils.AddError(data, "Data name must be specified for level-66 items", context.levelNumber);
			if (data.RenamesFromDataName.Equals(data.RenamesToDataName))
				//(source page 379 of ISO Cobol 2014)
				DiagnosticUtils.AddError(data, "Renamed items can't be the same " + data.RenamesFromDataName + " and " + data.RenamesToDataName, context);
		}
	}

	class AddStatementChecker: CodeElementListener {
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var statement = e as AddGivingStatement;
            if (statement == null) {
                return; //not our job
            }
            var context = c as CodeElementsParser.AddStatementContext;
			if (statement.Operand == null)
				DiagnosticUtils.AddError(statement, "Required: <identifier> after TO", context.addGiving());
		}
	}

    class CallStatementChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var statement = e as CallStatement;
        if (statement == null) {
            return; //not our job
        }
	    var context = (c as CodeElementsParser.CallStatementContext).cobolCallStatement();

        if (context != null) //if null it's certainly a CallStatementContext
        {
            foreach (var call in context.callUsingParameters()) CheckCallUsings(statement, call);

            if (context.callReturningParameter() != null && statement.OutputParameter == null)
                DiagnosticUtils.AddError(statement, "CALL .. RETURNING: Missing identifier", context);
        }
	
	}

	private void CheckCallUsings(CallStatement statement, CodeElementsParser.CallUsingParametersContext context) {
		foreach(var input in statement.InputParameters) {
			// TODO#249 these checks should be done during semantic phase, after symbol type resolution
			// TODO#249 if input is a file name AND input.SendingMode.Value == SendingMode.ByContent OR ByValue
			//	DiagnosticUtils.AddError(statement, "CALL .. USING: <filename> only allowed in BY REFERENCE phrase", context);
            bool isFunctionCallResult = input.StorageAreaOrValue != null && input.StorageAreaOrValue.StorageArea is FunctionCallResult;

            //SpecialRegister if LENGTH OF, LINAGE-COUNTER, ...
            var specialRegister = input.StorageAreaOrValue != null ? input.StorageAreaOrValue.StorageArea as StorageAreaPropertySpecialRegister : null;

            if (isFunctionCallResult)
				DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal function identifier", context);

			if (specialRegister != null && specialRegister.SpecialRegisterName.TokenType == TokenType.LINAGE_COUNTER)
				DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LINAGE-COUNTER", context);


		    if (input.SharingMode != null) {
                //BY REFERENCE
		        if (input.SharingMode.Value == ParameterSharingMode.ByReference) {
		            if (specialRegister != null && specialRegister.SpecialRegisterName.TokenType == TokenType.LENGTH)
		                DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal LENGTH OF in BY REFERENCE phrase", context);

		            if (input.StorageAreaOrValue != null && input.StorageAreaOrValue.IsLiteral)
		                DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal <literal> in BY REFERENCE phrase", context);
		        }

		        //BY VALUE
		        if (input.IsOmitted && input.SharingMode.Value == ParameterSharingMode.ByValue)
					DiagnosticUtils.AddError(statement, "CALL .. USING: Illegal OMITTED in BY VALUE phrase", context);
			}
		}
	}
}

	class CancelStatementChecker: CodeElementListener
	{
		public void OnCodeElement(CodeElement e, ParserRuleContext ctxt) {
			var statement = e as CancelStatement;
            if (statement == null) {
                return; //not our job
            }
			var context = ctxt as CodeElementsParser.CancelStatementContext;

			foreach (var item in statement.Programs) {
				if (item == null) continue;//TODO#249
				if (item.SymbolReference == null) continue;// DO nothing
				if (string.IsNullOrWhiteSpace(item.SymbolReference.Name) || item.SymbolReference.Name.IsNumeric()) {
					// we should link this error to the specific context.identifierOrLiteral[i] context
					// corresponding to statement.Items[i], but since refactor in #157 it's not trivial anymore
					DiagnosticUtils.AddError(statement, "CANCEL: <program name> must be alphanumeric", context);
				}
			}
		}

	}

    class InspectConvertingChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var statement = e as InspectConvertingStatement;
        if (statement == null) {
            return; //not our job
        }
		var context = c as CodeElementsParser.InspectStatementContext;
		var seen = new Dictionary<InspectStatement.StartCharacterPosition,bool>();
		foreach(var value in Enum.GetValues(typeof(InspectTallyingStatement.StartCharacterPosition))) {
			seen[(InspectTallyingStatement.StartCharacterPosition)value] = false;
		}
		for(int i=0; i < statement.ReplacingConditions.Length; i++) {
			var position = statement.ReplacingConditions[i].StartCharacterPosition;
			if (seen[position.Value]) {
				string error = "INSPECT: Maximum one "+position.Token.SourceText+" phrase for any one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase";
				DiagnosticUtils.AddError(statement, error, context.convertingPhrase().countingOrReplacingCondition()[i]);
			}
			seen[position.Value] = true;
		}
	}
}


    class MergeUsingChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var statement = e as MergeStatement;
        if (statement == null) {
            return; //not our job
        }
		var context = c as CodeElementsParser.MergeStatementContext;
		if (statement.InputFiles.Length == 1)
			DiagnosticUtils.AddError(statement, "MERGE: USING needs 2 filenames or more", context.usingFilenames());
	}
}

    class MoveSimpleChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var statement = e as MoveSimpleStatement;
        if (statement == null) {
            return; //not our job
        }
		var moveStatementContext = c as CodeElementsParser.MoveStatementContext;
	    if (moveStatementContext != null)
	    {
	        var moveSimpleContext = moveStatementContext.moveSimple();
	        if (moveSimpleContext != null)
	        {
	            if (statement.StorageAreaWrites != null) {
	                for (int i = 0; i < statement.StorageAreaWrites.Count; i++) {
	                    var receiver = statement.StorageAreaWrites[i].StorageArea;
	                    if (receiver is FunctionCallResult)
	                        DiagnosticUtils.AddError(statement, "MOVE: illegal <function call> after TO", moveSimpleContext.storageArea1()[i]);
	                }
	            }
            }
        }
    }
}

    class SearchStatementChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var statement = e as SearchStatement;
        if (statement == null) {
            return; //not our job
        }
		if (statement.TableToSearch == null) return; // syntax error
		if (statement.TableToSearch.StorageArea is DataOrConditionStorageArea && ((DataOrConditionStorageArea)statement.TableToSearch.StorageArea).Subscripts.Count > 0)
			DiagnosticUtils.AddError(statement, "SEARCH: Illegal subscripted identifier", GetIdentifierContext(c));
		if (statement.TableToSearch.StorageArea.ReferenceModifier != null)
			DiagnosticUtils.AddError(statement, "SEARCH: Illegal reference-modified identifier", GetIdentifierContext(c));
	}
	private static RuleContext GetIdentifierContext(ParserRuleContext context) {
		var c = (CodeElementsParser.SearchStatementContext)context;
		if (c.serialSearch() != null) return c.serialSearch().variable1().identifier();
		if (c.binarySearch() != null) return c.binarySearch().variable1().identifier();
		return null;
	}
}

    class SetStatementForAssignmentChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var set = e as SetStatementForAssignment;
        if (set == null) {
            return; //not our job
        }
		var context = c as CodeElementsParser.SetStatementForAssignmentContext;
		for (int i = 0; i < context.dataOrIndexStorageArea().Length; i++) {
			if (i >= set.ReceivingStorageAreas.Length) {
				var ctxt = context.dataOrIndexStorageArea()[i];
				DiagnosticUtils.AddError(set, "Set: Receiving fields missing or type unknown before TO", ctxt);
			}
		}
		if (set.SendingVariable == null)
			DiagnosticUtils.AddError(set, "Set: Sending field missing or type unknown after TO", context.setSendingField());
	}
}

    class SetStatementForIndexesChecker: CodeElementListener {
	public void OnCodeElement(CodeElement e, ParserRuleContext c) {
		var set = e as SetStatementForIndexes;
        if (set == null) {
            return; //not our job
        }
		if (set.SendingVariable == null) {
			var context = c as CodeElementsParser.SetStatementForIndexesContext;
			DiagnosticUtils.AddError(set, "Set xxx up/down by xxx: Sending field missing or type unknown", context.integerVariable1());
		}
	}
}

	class StartStatementChecker: CodeElementListener
	{
		public void OnCodeElement(CodeElement e, ParserRuleContext c) {
			var statement = e as StartStatement;
            if (statement == null) {
                return; //not our job
            }
			var context = c as CodeElementsParser.StartStatementContext;
			if (context.relationalOperator() != null)
				if (statement.RelationalOperator.Value != RelationalOperator.EqualTo && statement.RelationalOperator.Value != RelationalOperator.GreaterThan && statement.RelationalOperator.Value != RelationalOperator.GreaterThanOrEqualTo)
					DiagnosticUtils.AddError(statement, "START: Illegal operator "+statement.RelationalOperator.Value, context.relationalOperator());
		}
	}



    class SectionOrParagraphUsageChecker {

	    public static void CheckReferenceToParagraphOrSection(PerformProcedure perform) {
		    var performCE = (PerformProcedureStatement) perform.CodeElement;
		    SymbolReference symbol;
		    symbol = ResolveProcedureName(perform.SymbolTable, performCE.Procedure as AmbiguousSymbolReference, performCE);
		    if (symbol != null) performCE.Procedure = symbol;
		    symbol = ResolveProcedureName(perform.SymbolTable, performCE.ThroughProcedure as AmbiguousSymbolReference, performCE);
		    if (symbol != null) performCE.ThroughProcedure = symbol;
	    }
	    /// <summary>Disambiguate between section and paragraph names</summary>
	    /// <param name="table">Symbol table used for name resolution</param>
	    /// <param name="symbol">Symbol to disambiguate</param>
	    /// <param name="ce">Original CodeElement ; error diagnostics will be added to it if name resolution fails</param>
	    /// <returns>symbol as a SymbolReference whith a SymbolType properly set</returns>
	    private static SymbolReference ResolveProcedureName(SymbolTable table, SymbolReference symbol, CodeElement ce) {
		    if (symbol == null) return null;

		    SymbolReference sname = null, pname = null;
		    var sfound = table.GetSection(symbol.Name);
		    if (sfound.Count > 0) sname = new SymbolReference(symbol.NameLiteral, SymbolType.SectionName);
		    var pfound = table.GetParagraph(symbol.Name);
		    if (pfound.Count > 0) pname = new SymbolReference(symbol.NameLiteral, SymbolType.ParagraphName);

		    if (pname == null) {
			    if (sname == null) {
				    DiagnosticUtils.AddError(ce, "Symbol "+symbol.Name+" is not referenced");
			    } else {
				    if (sfound.Count > 1) DiagnosticUtils.AddError(ce, "Ambiguous reference to section "+symbol.Name);
				    return sname;
			    }
		    } else {
			    if (sname == null) {
				    if (pfound.Count > 1) DiagnosticUtils.AddError(ce, "Ambiguous reference to paragraph "+symbol.Name);
				    return pname;
			    } else {
				    DiagnosticUtils.AddError(ce, "Ambiguous reference to procedure "+symbol.Name);
			    }
		    }
		    return null;
	    }

        protected static void Check<T>(T node, [NotNull] IList<T> found) where T : Node
        {
            if (found.Count > 1) DiagnosticUtils.AddError(node.CodeElement, "Symbol \'" + node.Name + "\' already declared");
        }

        public static void CheckSection(Section section)
        {
            Check(section, section.SymbolTable.GetSection(section.Name));
        }

        public static void CheckParagraph(Paragraph paragraph)
        {
            Check(paragraph, paragraph.SymbolTable.GetParagraph(paragraph.Name));
        }
		    }


    class WriteTypeConsistencyChecker {

	    public static void OnNode(VariableWriter variableWriter, Node node) {
	        if (variableWriter == null) {
                    return; //not our job
            }
	        var variables = variableWriter.VariablesWritten;
	        foreach(var variable in variables) CheckVariable(node, variable.Key, variable.Value);
	    }

        /// <param name="wname">Receiving item; must be found and its type known</param>
        /// <param name="sent">Sending item; must be found and its type known</param>
        private static void CheckVariable(Node node, QualifiedName wname, object sent) {
		if (sent == null || wname == null) return;// I need both items
		var wsymbol = GetSymbol(node.SymbolTable, wname);
		if (wsymbol == null) return;// receiving symbol name unresolved
		var receiving = GetTypeDefinition(node.SymbolTable, wsymbol);
		if (receiving == null) return;// cannot find receiving type

		DataType sending = null;
		var sname = sent as QualifiedName;
		if (sname != null) {
			var ssymbol = GetSymbol(node.SymbolTable, sname);
			if (ssymbol == null) return;// sending symbol name unresolved
			sending = GetTypeDefinition(node.SymbolTable, ssymbol);
			if (sending == null) return;// cannot find sending type
		} else {
			bool? sbool = sent as bool?;
			if (sbool != null) sending = DataType.Boolean;
			double? sdouble = sent as double?;
			if (sdouble != null) sending = DataType.Numeric;
			string sstring = sent as string;
			if (sstring != null) sending = DataType.Alphanumeric;
		}
		if (sending != receiving) {
			var IsUnsafe = ((VariableWriter)node).IsUnsafe;
			if (receiving.RestrictionLevel > RestrictionLevel.WEAK) {
                    if (!IsUnsafe)
                    {
                        string message = string.Format("Cannot write {0} to {1} typed variable {2}:{3}."
                                                      , sending, receiving.RestrictionLevel == RestrictionLevel.STRONG ? "strongly" : "strictly"
                                                      , wname.Head, receiving);
                        DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
                    }
			} else {
				if (IsUnsafe) {
					string message = "Useless UNSAFE with non strongly typed receiver.";
					DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SyntaxWarningInParser);
				}
			}
		}
	}
	private static DataDefinition GetSymbol(SymbolTable table, SymbolReference symbolReference) {
		var found = table.GetVariable(symbolReference);
		if (found.Count != 1) return null;// symbol undeclared or ambiguous -> not my job
		return found[0];
	}
    private static DataDefinition GetSymbol(SymbolTable table, QualifiedName qualifiedName) {
		var found = table.GetVariable(qualifiedName);
		if (found.Count != 1) return null;// symbol undeclared or ambiguous -> not my job
		return found[0];
	}

    //TODO move this method to DataDefinition
	private static DataType GetTypeDefinition(SymbolTable table, Node symbol) {
		var data = symbol as DataDefinition;
		if (data != null) {
		    var dataCondition = data as DataCondition;
		    if (dataCondition != null)
				return dataCondition.CodeElement().DataType;

			DataDescriptionEntry entry;
			if (data.CodeElement is DataDescriptionEntry) {
				entry = (DataDescriptionEntry)data.CodeElement;
			} else
			if (data.CodeElement is DataRedefinesEntry) {
				var redefines = (DataRedefinesEntry)data.CodeElement;
			    var node = GetSymbol(table, redefines.RedefinesDataName);
			    if (node is DataDescription) {
			        entry = (DataDescriptionEntry) node.CodeElement;
			    } else {
                    entry = GetDataDescriptionEntry(table, redefines);
			    }
			} else throw new NotImplementedException(data.CodeElement.GetType().Name);
		    if (entry == null) {
		        return null;
		    }
			if (entry.UserDefinedDataType == null) return entry.DataType;//not a custom type
		}
        ITypedNode typed = symbol as ITypedNode;
		if (typed == null) return null;// symbol untyped
		var types = table.GetType(typed);
		if (types.Count != 1) return null;// symbol type not found or ambiguous
		return types[0].DataType;
	}

        /// <summary>
        /// Quick and dirty method, this checker need to be refactored
        /// </summary>
        /// <param name="table"></param>
        /// <param name="dataRedefinesEntry"></param>
        /// <returns></returns>
        private static DataDescriptionEntry GetDataDescriptionEntry(SymbolTable table, DataRedefinesEntry dataRedefinesEntry) {
            var node = GetSymbol(table, dataRedefinesEntry.RedefinesDataName);
            if (node == null) {
                return null;
            }
            if (node is DataDescription) {
                return (DataDescriptionEntry)node.CodeElement;
            }
            if (node is DataRedefines) {
                return GetDataDescriptionEntry(table, (DataRedefinesEntry) node.CodeElement);
            }
            throw new NotImplementedException(node.Name);
        }

}



}
