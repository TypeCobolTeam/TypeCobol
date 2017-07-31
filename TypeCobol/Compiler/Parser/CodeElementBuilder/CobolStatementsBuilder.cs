using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
	internal class CobolStatementsBuilder
	{
		public CobolStatementsBuilder(CobolWordsBuilder cobolWordsBuilder, CobolExpressionsBuilder cobolExpressionsBuilder)
		{
			CobolWordsBuilder = cobolWordsBuilder;
			CobolExpressionsBuilder = cobolExpressionsBuilder;
		}

		private CobolWordsBuilder CobolWordsBuilder { get; set; }
		private CobolExpressionsBuilder CobolExpressionsBuilder { get; set; }

		  ///////////////////////////////
		 // PROCEDURE DIVISION HEADER //
		///////////////////////////////

		internal IList<CallTargetParameter> CreateInputParameters(CodeElementsParser.ProgramInputParametersContext[] contexts) {
			if (contexts == null) return null;
			IList<CallTargetParameter> inputParameters = new List<CallTargetParameter>();
			foreach (var context in contexts) {
				SyntaxProperty<ParameterSharingMode> receivingMode = CreateReceivingMode(context);
				foreach (var storageAreaContext in context.sharedStorageArea2()) {
					var inputParameter = new CallTargetParameter {
						SharingMode = receivingMode,
						StorageArea = CobolExpressionsBuilder.CreateSharedStorageArea(storageAreaContext)
					};
					inputParameters.Add(inputParameter);
				}
			}
			return inputParameters;
		}
		private SyntaxProperty<ParameterSharingMode> CreateReceivingMode(CodeElementsParser.ProgramInputParametersContext context) {
			if (context.REFERENCE() != null) return new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, ParseTreeUtils.GetFirstToken(context.REFERENCE()));
			if (context.VALUE() != null) return new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByValue, ParseTreeUtils.GetFirstToken(context.VALUE()));
			return null;
		}

		  //////////////////////
		 // ACCEPT STATEMENT //
		//////////////////////

		internal AcceptFromInputDeviceStatement CreateAcceptDataTransferStatement(CodeElementsParser.AcceptDataTransferContext context) {
			var statement = new AcceptFromInputDeviceStatement();
			statement.ReceivingStorageArea = CobolExpressionsBuilder.CreateAlphanumericStorageArea(context.alphanumericStorageArea());
			statement.ReceivingStorageArea.DataSourceType = DataSourceType.ReadFromInputDevice;
			if (context.mnemonicForEnvironmentNameReferenceOrEnvironmentName() != null) {
				statement.InputDevice = CobolWordsBuilder.CreateMnemonicForEnvironmentNameReferenceOrEnvironmentName(context.mnemonicForEnvironmentNameReferenceOrEnvironmentName());
			}
			return statement;
		}

		internal AcceptFromSystemDateStatement CreateAcceptSystemDateTime(CodeElementsParser.AcceptSystemDateTimeContext context)
		{
			var statement = new AcceptFromSystemDateStatement();

			statement.ReceivingStorageArea = CobolExpressionsBuilder.CreateAlphanumericStorageArea(context.alphanumericStorageArea());
			statement.ReceivingStorageArea.DataSourceType = DataSourceType.ReadFromSystemCall;
			if (context.YYYYMMDD() != null)
			{
				statement.SystemDateFormat = new SyntaxProperty<SystemDateFormat>(SystemDateFormat.DATE_YYYYMMDD,
					ParseTreeUtils.GetFirstToken(context.YYYYMMDD()));
			}
			else if (context.DATE() != null)
			{
				statement.SystemDateFormat = new SyntaxProperty<SystemDateFormat>(SystemDateFormat.DATE_YYMMDD,
					ParseTreeUtils.GetFirstToken(context.DATE()));
			}
			else if (context.YYYYDDD() != null)
			{
				statement.SystemDateFormat = new SyntaxProperty<SystemDateFormat>(SystemDateFormat.DAY_YYYYDDD,
					ParseTreeUtils.GetFirstToken(context.YYYYDDD()));
			}
			else if (context.DAY() != null)
			{
				statement.SystemDateFormat = new SyntaxProperty<SystemDateFormat>(SystemDateFormat.DAY_YYDDD,
					ParseTreeUtils.GetFirstToken(context.DAY()));
			}
			else if (context.DAY_OF_WEEK() != null)
			{
				statement.SystemDateFormat = new SyntaxProperty<SystemDateFormat>(SystemDateFormat.DAY_OF_WEEK,
					ParseTreeUtils.GetFirstToken(context.DAY_OF_WEEK()));
			}
			else if (context.TIME() != null)
			{
				statement.SystemDateFormat = new SyntaxProperty<SystemDateFormat>(SystemDateFormat.TIME,
					ParseTreeUtils.GetFirstToken(context.TIME()));
			}

			return statement;
		}

		  ///////////////////
		 // ADD STATEMENT //
		///////////////////

		internal CodeElement CreateAddStatement(CodeElementsParser.AddSimpleContext context) {
			var statement = new AddSimpleStatement();
			statement.VariablesTogether = BuildObjectArrayFromParserRules(context.numericVariable3(), ctx => CobolExpressionsBuilder.CreateNumericVariable(ctx));
			statement.SendingAndReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		private RoundedResult CreateRoundedResult(CodeElementsParser.NumericStorageAreaRoundedContext context) {
			var roundedResult = new RoundedResult();
			roundedResult.ReceivingStorageArea = CobolExpressionsBuilder.CreateNumericStorageArea(context.numericStorageArea());
			roundedResult.Rounded = CreateSyntaxProperty(true, context.ROUNDED());
			return roundedResult;
		}

		internal CodeElement CreateAddGivingStatement(CodeElementsParser.AddGivingContext context) {
			var statement = new AddGivingStatement();
			statement.VariablesTogether = BuildObjectArrayFromParserRules(context.numericVariable3(), ctx => CobolExpressionsBuilder.CreateNumericVariable(ctx));
			statement.Operand = CobolExpressionsBuilder.CreateNumericVariable(context.toOperand);
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		internal CodeElement CreateAddCorrespondingStatement(CodeElementsParser.AddCorrespondingContext context) {
			var statement = new AddCorrespondingStatement();
			statement.GroupItem = CobolExpressionsBuilder.CreateDataItemReference(context.groupItem);
			statement.SendingAndReceivingGroupItem = CobolExpressionsBuilder.CreateDataItemReference(context.toGroupItem);
			statement.Rounded = CreateSyntaxProperty(true, context.ROUNDED());

            // Collect storage area read/writes at the code element level
            if (statement.GroupItem != null && statement.SendingAndReceivingGroupItem != null)
            {
                CobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact = new GroupCorrespondingImpact()
                {
                    SendingGroupItem = statement.GroupItem,
                    ReceivingGroupItem = statement.SendingAndReceivingGroupItem,
                    ReceivingGroupIsAlsoSending = true
                };
            }
			return statement;
		}

		  /////////////////////
		 // ALTER STATEMENT //
		/////////////////////

		internal CodeElement CreateAlterStatement(CodeElementsParser.AlterStatementContext context) {
			var statement = new AlterStatement();
			int alterInstructionsCount = context.procedureName().Length / 2;
			statement.AlterGotoInstructions = new AlterGotoInstruction[alterInstructionsCount];
			for(int i = 0; i < alterInstructionsCount; i++) {
				var alterInstruction = new AlterGotoInstruction();
				alterInstruction.AlteredProcedure = CobolWordsBuilder.CreateProcedureName(context.procedureName()[2*i]);
				alterInstruction.NewTargetProcedure= CobolWordsBuilder.CreateProcedureName(context.procedureName()[2*i + 1]);                    
				statement.AlterGotoInstructions[i] = alterInstruction;
			}
			return statement;
		}

		  ////////////////////
		 // CALL STATEMENT //
		////////////////////
		internal CallStatement CreateCallStatement(CodeElementsParser.CobolCallStatementContext context) {
			var statement = new CallStatement();

            statement.ProgramOrProgramEntryOrProcedureOrFunction = 
				CobolExpressionsBuilder.CreateProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariable(context.programNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariable());
			statement.InputParameters = new List<CallSiteParameter>();
			if (context.callUsingParameters() != null) {
				SyntaxProperty<ParameterSharingMode> sendingMode = new SyntaxProperty<ParameterSharingMode>(ParameterSharingMode.ByReference, null);
				foreach (var inputs in context.callUsingParameters()) {
					if (inputs.REFERENCE() != null) {
						sendingMode = CreateSyntaxProperty(ParameterSharingMode.ByReference, inputs.REFERENCE());
					} else
					if (inputs.CONTENT() != null) {
						sendingMode = CreateSyntaxProperty(ParameterSharingMode.ByContent, inputs.CONTENT());
					} else
					if (inputs.VALUE() != null) {
						sendingMode = CreateSyntaxProperty(ParameterSharingMode.ByValue, inputs.VALUE());
					}
					foreach (var variable in inputs.variableOrFileNameOrOmitted()) {
						var inputParameter = new CallSiteParameter { SharingMode = sendingMode };
						if (variable.sharedVariableOrFileName() != null) {
							inputParameter.StorageAreaOrValue = CobolExpressionsBuilder.CreateSharedVariableOrFileName(variable.sharedVariableOrFileName());
						} else
						if (variable.OMITTED() != null) {
							inputParameter.Omitted = CreateSyntaxProperty(true, variable.OMITTED());
						}
						statement.InputParameters.Add(inputParameter);
					}
				}
			}
			if (context.callReturningParameter() != null) {
				var storageArea = CobolExpressionsBuilder.CreateSharedStorageArea(context.callReturningParameter().sharedStorageArea1());
				if (storageArea != null)
				{
					statement.OutputParameter = new CallSiteParameter() { StorageAreaOrValue = new Variable(storageArea) };
				}
			}

			// Register call parameters (shared storage areas) information at the CodeElement level
			var callSite = new CallSite() { CallTarget = statement.ProgramOrProgramEntryOrProcedureOrFunction.SymbolReference };
			int parametersCount =
				(statement.InputParameters != null ? statement.InputParameters.Count : 0)
				+ (statement.OutputParameter != null ? 1 : 0);
			callSite.Parameters = new CallSiteParameter[parametersCount];
			int i = 0;
			if (statement.InputParameters != null && statement.InputParameters.Count > 0) {
				foreach (var param in statement.InputParameters) {
					callSite.Parameters[i] = param;
					i++;
				}
			}
			if (statement.OutputParameter != null) {
				callSite.Parameters[i] = statement.OutputParameter;
			}
			if (statement.CallSites == null) statement.CallSites = new List<CallSite>();
			statement.CallSites.Add(callSite);

			return statement;
		}
		
		  //////////////////////
		 // CANCEL STATEMENT //
		//////////////////////

		internal CodeElement CreateCancelStatement(CodeElementsParser.CancelStatementContext context) {
			var statement = new CancelStatement();
			statement.Programs = BuildObjectArrayFromParserRules(context.programNameVariable(), ctx => CobolExpressionsBuilder.CreateProgramNameVariable(ctx));
			return statement;
		}

		  /////////////////////
		 // CLOSE STATEMENT //
		/////////////////////

		internal CodeElement CreateCloseStatement(CodeElementsParser.CloseStatementContext context) {
			var statement = new CloseStatement();
			statement.CloseFileInstructions = BuildObjectArrayFromParserRules(context.closeFileDirective(), ctx => CreateCloseFileInstruction(ctx));
			return statement;
		}       

		private CloseFileInstruction CreateCloseFileInstruction(CodeElementsParser.CloseFileDirectiveContext context) {
			var instruction = new CloseFileInstruction();
			instruction.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
			instruction.IsReelUnit = CreateSyntaxProperty(true, context.REEL());
			if(instruction.IsReelUnit == null) {
				instruction.IsReelUnit = CreateSyntaxProperty(true, context.UNIT());
			}
			instruction.IsForRemoval = CreateSyntaxProperty(true, context.REMOVAL());
			instruction.IsWithNoRewind = CreateSyntaxProperty(true, context.REWIND());
			instruction.IsWithLock = CreateSyntaxProperty(true, context.LOCK());
			return instruction;
		}

		  ///////////////////////
		 // COMPUTE STATEMENT //
		///////////////////////

		internal CodeElement CreateComputeStatement(CodeElementsParser.ComputeStatementContext context) {
			var statement = new ComputeStatement();
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			statement.ArithmeticExpression = CobolExpressionsBuilder.CreateArithmeticExpression(context.arithmeticExpression());
			return statement;
		}

		  //////////////////////
		 // DELETE STATEMENT //
		//////////////////////

		internal CodeElement CreateDeleteStatement(CodeElementsParser.DeleteStatementContext context) {
			var statement = new DeleteStatement();
			statement.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
			return statement;
		}

		//////////////////////
		// DISPLAY STATEMENT //
		//////////////////////

		internal CodeElement CreateDisplayStatement(CodeElementsParser.DisplayStatementContext context)
		{
			var statement = new DisplayStatement();

			statement.Variables = BuildObjectArrrayWithNoNullFromParserRules(context.variable4(),
				ctx => CobolExpressionsBuilder.CreateVariable(ctx));

			if(context.uponOutputDevice() != null)
			{
				statement.OutputDeviceName = CobolWordsBuilder.CreateMnemonicForEnvironmentNameReferenceOrEnvironmentName(
					context.uponOutputDevice().mnemonicForEnvironmentNameReferenceOrEnvironmentName());
			}
			if (context.withNoAdvancing() != null)
			{
				statement.WithNoAdvancing = CreateSyntaxProperty(true, context.withNoAdvancing().ADVANCING());
			}

			return statement;
		}

		  //////////////////////
		 // DIVIDE STATEMENT //
		//////////////////////

		internal CodeElement CreateDivideStatement(CodeElementsParser.DivideSimpleContext context) {
			var statement = new DivideSimpleStatement();
			statement.Divisor = CobolExpressionsBuilder.CreateNumericVariable(context.divisor);
			statement.SendingAndReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		internal CodeElement CreateDivideGivingStatement(CodeElementsParser.DivideGivingContext context) {
			var statement = new DivideGivingStatement();
			if(context.divisor1 != null) {
				statement.Divisor = CobolExpressionsBuilder.CreateNumericVariable(context.divisor1);
			}
			else if (context.divisor2 != null) {
				statement.Divisor = CobolExpressionsBuilder.CreateNumericVariable(context.divisor2);
			}
			if (context.dividend1 != null) {
				statement.Dividend = CobolExpressionsBuilder.CreateNumericVariable(context.dividend1);
			}
			else if (context.dividend2 != null) {
				statement.Dividend = CobolExpressionsBuilder.CreateNumericVariable(context.dividend2);
			}
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		internal CodeElement CreateDivideRemainderStatement(CodeElementsParser.DivideRemainderContext context) {
			var statement = new DivideRemainderStatement();
			if (context.divisor1 != null) {
				statement.Divisor = CobolExpressionsBuilder.CreateNumericVariable(context.divisor1);
			}
			else if (context.divisor2 != null) {
				statement.Divisor = CobolExpressionsBuilder.CreateNumericVariable(context.divisor2);
			}
			if (context.dividend1 != null) {
				statement.Dividend = CobolExpressionsBuilder.CreateNumericVariable(context.dividend1);
			}
			else if (context.dividend2 != null) {
				statement.Dividend = CobolExpressionsBuilder.CreateNumericVariable(context.dividend2);
			}
			statement.Quotient = CreateRoundedResult(context.numericStorageAreaRounded());
			statement.Remainder = CobolExpressionsBuilder.CreateNumericStorageArea(context.numericStorageArea());
			return statement;
		}

		  /////////////////////
		 // ENTRY STATEMENT //
		/////////////////////

		internal CodeElement CreateEntryStatement(CodeElementsParser.EntryStatementContext context) {
			var statement = new EntryStatement();
			statement.ProgramEntry = CobolWordsBuilder.CreateProgramEntryDefinition(context.programEntryDefinition());
			statement.InputParameters = CreateInputParameters(context.programInputParameters());
			return statement;
		}

		  ////////////////////////
		 // EVALUATE STATEMENT //
		////////////////////////

		internal CodeElement CreateEvaluateStatement(CodeElementsParser.EvaluateStatementContext context) {
			var statement = new EvaluateStatement();
			statement.SelectionSubjects = BuildObjectArrayFromParserRules(context.comparisonLHSExpression(), ctx => CreateEvaluateSelectionSubject(ctx));
			return statement;
		}

		private EvaluateSelectionSubject CreateEvaluateSelectionSubject(CodeElementsParser.ComparisonLHSExpressionContext context) {
			var selectionSubject = new EvaluateSelectionSubject();
			if (context.variableOrExpression2() != null) {
				selectionSubject.AlphanumericComparisonVariable = CobolExpressionsBuilder.CreateVariableOrExpression(context.variableOrExpression2());
			} else
			if (context.booleanValueOrExpression() != null) {
				selectionSubject.BooleanComparisonVariable = CobolExpressionsBuilder.CreateBooleanValueOrExpression(context.booleanValueOrExpression());
			}
			return selectionSubject;
		}

		  ////////////////////
		 // EXEC STATEMENT //
		////////////////////

		internal CodeElement CreateExecStatement(CodeElementsParser.ExecStatementContext context) {
			var statement = new ExecStatement();
			statement.ExecTranslatorName = CobolWordsBuilder.CreateExecTranslatorName(context.execTranslatorName());
			statement.CodeLines = BuildObjectArrayFromParserRules(context.alphanumericValue8(), ctx => CobolWordsBuilder.CreateAlphanumericValue(ctx));
			return statement;
		}

		  ////////////////////
		 // GOTO STATEMENT //
		////////////////////

		internal CodeElement CreateGotoStatement(CodeElementsParser.GotoSimpleContext context) {
			var statement = new GotoSimpleStatement();
			statement.ProcedureName = CobolWordsBuilder.CreateProcedureName(context.procedureName());
			return statement;
		}

		internal CodeElement CreateGotoConditionalStatement(CodeElementsParser.GotoConditionalContext context) {
			var statement = new GotoConditionalStatement();
			statement.ProcedureNames = BuildObjectArrayFromParserRules(context.procedureName(), ctx => CobolWordsBuilder.CreateProcedureName(ctx));
			statement.DependingOn = CobolExpressionsBuilder.CreateVariable(context.variable1());
			if (statement.ProcedureNames.Length > 1 && statement.DependingOn == null)
				DiagnosticUtils.AddError(statement, "GO TO: Required only one <procedure name> or DEPENDING phrase", context);
			if (statement.ProcedureNames.Length < 1 && statement.DependingOn != null)
				DiagnosticUtils.AddError(statement, "Conditional GO TO: Required <procedure name>", context);
			if (statement.ProcedureNames.Length > 255)
				DiagnosticUtils.AddError(statement, "Conditional GO TO: Maximum 255 <procedure name> allowed", context);
			return statement;
		}

		  //////////////////
		 // IF STATEMENT //
		//////////////////

		internal CodeElement CreateIfStatement(CodeElementsParser.IfStatementContext context) {
			var statement = new IfStatement();
			statement.Condition = CobolExpressionsBuilder.CreateConditionalExpression(context.conditionalExpression());
			return statement;
		}

		//////////////////////////
		// INITIALIZE STATEMENT //
		//////////////////////////

		internal InitializeStatement CreateInitializeStatement(CodeElementsParser.InitializeStatementContext context) {
			var statement = new InitializeStatement();
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.storageArea1(), ctx => CobolExpressionsBuilder.CreateStorageArea(ctx));
			statement.ReplacingInstructions = BuildObjectArrayFromParserRules(context.initializeReplacingDirective(), ctx => CreateInitializeReplacingInstruction(ctx));
			return statement;
		}

		private InitializeReplacingInstruction CreateInitializeReplacingInstruction(CodeElementsParser.InitializeReplacingDirectiveContext context)
		{
			var replacingInstruction = new InitializeReplacingInstruction();

			if (context.dataCategory() != null)
			{
				if (context.dataCategory().ALPHABETIC() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.ALPHABETIC,
						context.dataCategory().ALPHABETIC());
				}
				else if (context.dataCategory().ALPHANUMERIC() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.ALPHANUMERIC,
						context.dataCategory().ALPHANUMERIC());
				}
				else if (context.dataCategory().ALPHANUMERIC_EDITED() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.ALPHANUMERIC_EDITED,
						context.dataCategory().ALPHANUMERIC_EDITED());
				}
				else if (context.dataCategory().NATIONAL() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.NATIONAL,
						context.dataCategory().NATIONAL());
				}
				else if (context.dataCategory().NATIONAL_EDITED() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.NATIONAL_EDITED,
						context.dataCategory().NATIONAL_EDITED());
				}
				else if (context.dataCategory().NUMERIC() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.NUMERIC,
						context.dataCategory().NUMERIC());
				}
				else if (context.dataCategory().NUMERIC_EDITED() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.NUMERIC_EDITED,
						context.dataCategory().NUMERIC_EDITED());
				}
				else if (context.dataCategory().DBCS() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.DBCS,
						context.dataCategory().DBCS());
				}
				else if (context.dataCategory().EGCS() != null)
				{
					replacingInstruction.ReplaceDataCategory = CreateSyntaxProperty(InitializeDataCategory.EGCS,
						context.dataCategory().EGCS());
				}
			}

			replacingInstruction.BySendingVariable = CobolExpressionsBuilder.CreateVariable(context.variable6());

			return replacingInstruction;
		}

		///////////////////////
		// INSPECT STATEMENT //
		///////////////////////

		internal CodeElement CreateInspectStatement(CodeElementsParser.InspectStatementContext context)
		{
			if (context.convertingPhrase() != null) {
				var statement = new InspectConvertingStatement();
				statement.InspectedItem = CobolExpressionsBuilder.CreateAlphanumericStorageArea(context.alphanumericStorageArea());
				statement.SearchedCharacterString = CobolExpressionsBuilder.CreateAlphanumericVariable(context.convertingPhrase().searchedCharacterString);
				statement.ReplacingCharacterString = CobolExpressionsBuilder.CreateAlphanumericVariable(context.convertingPhrase().replacingCharacterString);
				statement.ReplacingConditions = BuildObjectArrayFromParserRules(context.convertingPhrase().countingOrReplacingCondition(), ctx => CreateCountingOrReplacingCondition(ctx));
				return statement;
			} else {
				InspectTallyingStatement statement = null;
				if(context.replacingPhrase() != null)
				{
					statement = new InspectReplacingStatement();
				}
				else
				{
					statement = new InspectTallyingStatement();
				}
				
				statement.InspectedItem = CobolExpressionsBuilder.CreateAlphanumericStorageArea(context.alphanumericStorageArea());

				if(context.tallyingPhrase() != null)
				{
					statement.TallyingInstructions = BuildObjectArrayFromParserRules(context.tallyingPhrase().inspectTallyingOperation(), ctx => CreateInspectTallyingInstruction(ctx));
				}
				if(context.replacingPhrase() != null)
				{
					InspectReplacingStatement replacingStatement = statement as InspectReplacingStatement;

					replacingStatement.ReplaceAllCharactersOperations = BuildObjectArrayFromParserRules(context.replacingPhrase().replaceAllCharacters(), ctx => CreateReplaceAllCharactersOperation(ctx));
					replacingStatement.ReplaceCharacterStringsOperations = BuildObjectArrayFromParserRules(context.replacingPhrase().replaceCharacterStrings(), ctx => CreateReplaceCharacterStringsOperation(ctx));
				}

				return statement;
			}
		}

		private InspectTallyingStatement.InspectTallyingInstruction CreateInspectTallyingInstruction(CodeElementsParser.InspectTallyingOperationContext context) {
			var instruction = new InspectTallyingStatement.InspectTallyingInstruction();
			instruction.CountField = CobolExpressionsBuilder.CreateNumericStorageArea(context.numericStorageArea());
			instruction.CountAllCharactersOperations = BuildObjectArrayFromParserRules(context.countAllCharacters(), ctx => CreateCountAllCharactersOperation(ctx));
			instruction.CountCharacterStringsOperations = BuildObjectArrayFromParserRules(context.countCharacterStrings(), ctx => CreateCountCharacterStringsOperation(ctx));
			return instruction;
		}

		private InspectTallyingStatement.InspectTallyingInstruction.CountAllCharactersOperation CreateCountAllCharactersOperation(CodeElementsParser.CountAllCharactersContext context)
		{
			var operation = new InspectTallyingStatement.InspectTallyingInstruction.CountAllCharactersOperation();
			operation.CountingConditions = BuildObjectArrayFromParserRules(context.countingOrReplacingCondition(), ctx => CreateCountingOrReplacingCondition(ctx));
			return operation;
		}
		
		private InspectStatement.CountingOrReplacingCondition CreateCountingOrReplacingCondition(CodeElementsParser.CountingOrReplacingConditionContext context) {
			var condition = new InspectStatement.CountingOrReplacingCondition();
			if(context.BEFORE() != null) {
				condition.StartCharacterPosition = CreateSyntaxProperty(InspectTallyingStatement.StartCharacterPosition.Before, context.BEFORE());
			} else
			if(context.AFTER() != null) {
				condition.StartCharacterPosition = CreateSyntaxProperty(InspectTallyingStatement.StartCharacterPosition.After, context.AFTER());
			}
			if(context.INITIAL() != null) {
				condition.InitialOccurence = CreateSyntaxProperty(true, context.INITIAL());
			}
			condition.Delimiter = CobolExpressionsBuilder.CreateAlphanumericVariable(context.alphanumericVariable1());
			return condition;
		}

		private InspectTallyingStatement.InspectTallyingInstruction.CountCharacterStringsOperation CreateCountCharacterStringsOperation(CodeElementsParser.CountCharacterStringsContext context) {
			var operation = new InspectTallyingStatement.InspectTallyingInstruction.CountCharacterStringsOperation();
			if(context.ALL() != null) {
				operation.CharacterStringsSelection = CreateSyntaxProperty(InspectTallyingStatement.CharacterStringsSelection.All, context.ALL());
			} else
			if(context.LEADING() != null) {
				operation.CharacterStringsSelection = CreateSyntaxProperty(InspectTallyingStatement.CharacterStringsSelection.Leading, context.LEADING());
			}
			operation.CharacterStringPatterns = BuildObjectArrayFromParserRules(context.countCharacterStringPattern(), ctx => CreateCountCharacterStringPattern(ctx));
			return operation;
		}

		private InspectTallyingStatement.InspectTallyingInstruction.CountCharacterStringPattern CreateCountCharacterStringPattern(CodeElementsParser.CountCharacterStringPatternContext context) {
			var pattern = new InspectTallyingStatement.InspectTallyingInstruction.CountCharacterStringPattern();
			pattern.SearchedCharacterString = CobolExpressionsBuilder.CreateAlphanumericVariable(context.alphanumericVariable1());
			pattern.CountingConditions = BuildObjectArrayFromParserRules(context.countingOrReplacingCondition(), ctx => CreateCountingOrReplacingCondition(ctx));
			return pattern;
		}

		private InspectReplacingStatement.ReplaceAllCharactersOperation CreateReplaceAllCharactersOperation(CodeElementsParser.ReplaceAllCharactersContext context) {
			var operation = new InspectReplacingStatement.ReplaceAllCharactersOperation();
			operation.ReplacingCharacterString = CobolExpressionsBuilder.CreateAlphanumericVariable(context.alphanumericVariable2());
			operation.ReplacingConditions = BuildObjectArrayFromParserRules(context.countingOrReplacingCondition(), ctx => CreateCountingOrReplacingCondition(ctx));
			return operation;
		}

		private InspectReplacingStatement.ReplaceCharacterStringsOperation CreateReplaceCharacterStringsOperation(CodeElementsParser.ReplaceCharacterStringsContext context) {
			var operation = new InspectReplacingStatement.ReplaceCharacterStringsOperation();
			if (context.ALL() != null) {
				operation.CharacterStringsSelection = CreateSyntaxProperty(InspectTallyingStatement.CharacterStringsSelection.All, context.ALL());
			} else
			if (context.LEADING() != null) {
				operation.CharacterStringsSelection = CreateSyntaxProperty(InspectTallyingStatement.CharacterStringsSelection.Leading, context.LEADING());
			} else
			if (context.FIRST() != null) {
				operation.CharacterStringsSelection = CreateSyntaxProperty(InspectTallyingStatement.CharacterStringsSelection.First, context.FIRST());
			}
			return operation;
		}

		//////////////////////
		// INVOKE STATEMENT //
		////////////////////// 

		internal InvokeStatement CreateInvokeStatement(CodeElementsParser.InvokeStatementContext context)
		{
			var statement = new InvokeStatement();
			
			if(context.classNameOrObjectReferenceVariable() != null)
			{
				statement.ClassNameOrObjectReference = 
					CobolExpressionsBuilder.CreateClassNameOrObjectReferenceVariable(context.classNameOrObjectReferenceVariable());
			}
			else if(context.selfObjectIdentifier() != null)
			{
				statement.SelfOjectIdentifier = CreateSyntaxProperty(true,
					context.selfObjectIdentifier().SELF());
			}
			else if(context.superObjectIdentifier() != null)
			{
				statement.SuperObjectIdentifier = CreateSyntaxProperty(true,
					context.superObjectIdentifier().SUPER());
			}

			if(context.methodNameVariable() != null)
			{
				statement.MethodName = 
					CobolExpressionsBuilder.CreateMethodNameVariable(context.methodNameVariable());
			}
			else if(context.NEW() != null)
			{
				statement.ConstructorMethod = CreateSyntaxProperty(true,
					context.NEW());
			}

			if(context.invokeInputParameter() != null && context.invokeInputParameter().Length > 0)
			{
				statement.InputParameters = new List<CallSiteParameter>();
				foreach(var parameterContext in context.invokeInputParameter())
				{
					foreach(var variableContext in parameterContext.sharedVariable3())
					{
						statement.InputParameters.Add(new CallSiteParameter() { StorageAreaOrValue =
                            CobolExpressionsBuilder.CreateSharedVariable(variableContext) });
					}
				}
			}

			if(context.invokeOutputParameter() != null)
			{
                var storageArea = CobolExpressionsBuilder.CreateSharedStorageArea(context.invokeOutputParameter().sharedStorageArea1());
                if (storageArea != null)
                {
                    statement.OutputParameter = new CallSiteParameter() { StorageAreaOrValue = new Variable(storageArea) };
                }
			}

            //if (IdentifierUtils.IsReferenceModified(statement.Returning))
            //    DiagnosticUtils.AddError(statement, "INVOKE: Illegal <identifier> reference modification", context.invokeReturning().identifier());

            // Register call parameters (shared storage areas) information at the CodeElement level
            var callSite = new CallSite() { CallTarget = statement.MethodName != null ? statement.MethodName.SymbolReference : null }; // TO DO : ConstructorMethod
            int parametersCount =
                (statement.InputParameters != null ? statement.InputParameters.Count : 0)
                + (statement.OutputParameter != null ? 1 : 0);
            callSite.Parameters = new CallSiteParameter[parametersCount];
            int i = 0;
            if (statement.InputParameters != null && statement.InputParameters.Count > 0)
            {
                foreach (var param in statement.InputParameters)
                {
                    callSite.Parameters[i] = param;
                    i++;
                }
            }
            if (statement.OutputParameter != null)
            {
                callSite.Parameters[i] = statement.OutputParameter;
            }
            if (statement.CallSites == null) statement.CallSites = new List<CallSite>();
            statement.CallSites.Add(callSite);        

			return statement;
		}

		/////////////////////
		// MERGE STATEMENT //
		/////////////////////

		internal MergeStatement CreateMergeStatement(CodeElementsParser.MergeStatementContext context) {
			var statement = new MergeStatement();
			statement.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
			statement.SortingKeys = CreateSortingKeys(context.onAscendingDescendingKey());
			if (context.collatingSequence() != null) {
				statement.CollatingSequence = CobolWordsBuilder.CreateAlphabetNameReference(
					context.collatingSequence().alphabetNameReference());
			}
			if (context.usingFilenames() != null) {
				statement.InputFiles = BuildObjectArrayFromParserRules(context.usingFilenames().fileNameReference(), ctx => CobolWordsBuilder.CreateFileNameReference(ctx));
			}
			if (context.givingFilenames() != null) {
				statement.OutputFiles = BuildObjectArrayFromParserRules(context.givingFilenames().fileNameReference(), ctx => CobolWordsBuilder.CreateFileNameReference(ctx));
			} else
			if (context.outputProcedure() != null) {
				if (context.outputProcedure().procedureName() != null) {
					statement.OutputProcedure = CobolWordsBuilder.CreateProcedureName(context.outputProcedure().procedureName());
				} else
				if(context.outputProcedure().proceduresRange() != null) {
					statement.OutputProcedure = CobolWordsBuilder.CreateProcedureName(context.outputProcedure().proceduresRange().startProcedure);
					statement.ThroughOutputProcedure = CobolWordsBuilder.CreateProcedureName(context.outputProcedure().proceduresRange().endProcedure);
				}
			}
			return statement;
		}

		private IList<SortingKey> CreateSortingKeys(CodeElementsParser.OnAscendingDescendingKeyContext[] contexts) {
			if (contexts != null && contexts.Length > 0) {
				var sortingKeys = new List<SortingKey>(1);
				foreach(var context in contexts) {
					SyntaxProperty<SortingDirection> sortingDirection = null;
					if (context.ASCENDING() != null) {
						sortingDirection = CreateSyntaxProperty(SortingDirection.Ascending, context.ASCENDING());
					} else
					if (context.DESCENDING() != null) {
						sortingDirection = CreateSyntaxProperty(SortingDirection.Descending, context.DESCENDING());
					}
					foreach(var dataContext in context.qualifiedDataName()) {
						var sortingKey = new SortingKey();
						sortingKey.Direction = sortingDirection;
						sortingKey.DataItem = CobolWordsBuilder.CreateQualifiedDataName(dataContext);
						sortingKeys.Add(sortingKey);
					}
				}
				return sortingKeys;
			}
			return null;
		}

		  ////////////////////
		 // MOVE STATEMENT //
		////////////////////

		internal MoveSimpleStatement CreateMoveStatement(CodeElementsParser.MoveSimpleContext context) {
		    var statement = new MoveSimpleStatement(CobolExpressionsBuilder.CreateVariable(context.variable7()),
						BuildObjectArrayFromParserRules(context.storageArea1(), ctx => CobolExpressionsBuilder.CreateStorageArea(ctx)),
// [TYPECOBOL]
						CobolWordsBuilder.CreateBooleanValue(context.booleanValue()));
			if (context.UNSAFE() != null) statement.Unsafe = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.UNSAFE()));
// [/TYPECOBOL]
			return statement;
		}

		internal MoveCorrespondingStatement CreateMoveStatement(CodeElementsParser.MoveCorrespondingContext context) {
			var statement = new MoveCorrespondingStatement();
			statement.FromGroupItem = CobolExpressionsBuilder.CreateDataItemReference(context.fromGroupItem);
			statement.ToGroupItem = CobolExpressionsBuilder.CreateDataItemReference(context.toGroupItem);
// [TYPECOBOL]
			if (context.UNSAFE() != null) statement.Unsafe = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.UNSAFE()));
            // [/TYPECOBOL]

            // Collect storage area read/writes at the code element level
            if (statement.FromGroupItem != null && statement.ToGroupItem != null)
            {
                CobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact = new GroupCorrespondingImpact()
                {
                    SendingGroupItem = statement.FromGroupItem,
                    ReceivingGroupItem = statement.ToGroupItem,
                    ReceivingGroupIsAlsoSending = false
                };
            }
            return statement;
		}

		  ////////////////////////
		 // MULTIPLY STATEMENT //
		////////////////////////

		internal CodeElement CreateMultiplyStatement(CodeElementsParser.MultiplySimpleContext context) {
			var statement = new MultiplySimpleStatement();
			statement.Operand = CobolExpressionsBuilder.CreateNumericVariable(context.numericVariable3());
			statement.SendingAndReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		internal CodeElement CreateMultiplyGivingStatement(CodeElementsParser.MultiplyGivingContext context) {
			var statement = new MultiplyGivingStatement();
			statement.Operand = CobolExpressionsBuilder.CreateNumericVariable(context.numericVariable3()[0]);
			statement.ByOperand = CobolExpressionsBuilder.CreateNumericVariable(context.byOperand);
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		  ////////////////////
		 // OPEN STATEMENT //
		////////////////////

		internal CodeElement CreateOpenStatement(CodeElementsParser.OpenStatementContext context) {
			var statement = new OpenStatement();
			statement.OpenFileInstructions = new List<OpenFileInstruction>(1);
			if(context.openInput() != null && context.openInput().Length > 0) {
				foreach(var inputCtx in context.openInput()) {
					SyntaxProperty<OpenMode> openMode = CreateSyntaxProperty(OpenMode.INPUT, inputCtx.INPUT());
					CreateFileInstructions(statement.OpenFileInstructions, inputCtx.fileNameWithNoRewindOrReversed(), openMode);
				}
			}
			if (context.openOutput() != null && context.openOutput().Length > 0) {
				foreach (var outputCtx in context.openOutput()) {
					SyntaxProperty<OpenMode> openMode = CreateSyntaxProperty(OpenMode.OUTPUT, outputCtx.OUTPUT());
					CreateFileInstructions(statement.OpenFileInstructions, outputCtx.fileNameWithNoRewindOrReversed(), openMode);
				}
			}
			if (context.openIO() != null && context.openIO().Length > 0) {
				foreach (var ioCtx in context.openIO()) {
					SyntaxProperty<OpenMode> openMode = CreateSyntaxProperty(OpenMode.IO, ioCtx.I_O());
					CreateFileInstructions(statement.OpenFileInstructions, ioCtx.fileNameReference(), openMode);
				}
			}
			if (context.openExtend() != null && context.openExtend().Length > 0) {
				foreach (var extendCtx in context.openExtend()) {
					SyntaxProperty<OpenMode> openMode = CreateSyntaxProperty(OpenMode.EXTEND, extendCtx.EXTEND());
					CreateFileInstructions(statement.OpenFileInstructions, extendCtx.fileNameReference(), openMode);
				}
			}
			return statement;
		}

		private void CreateFileInstructions(IList<OpenFileInstruction> openFileInstructions, CodeElementsParser.FileNameReferenceContext[] fileNameReferenceContexts, SyntaxProperty<OpenMode> openMode) {
			foreach (var fileNameReferenceCtx in fileNameReferenceContexts) {
				var openFileInstruction = new OpenFileInstruction();
				openFileInstruction.OpenMode = openMode;
				openFileInstruction.FileName = CobolWordsBuilder.CreateFileNameReference(fileNameReferenceCtx);
				openFileInstructions.Add(openFileInstruction);
			}
		}

		private void CreateFileInstructions(IList<OpenFileInstruction> openFileInstructions, CodeElementsParser.FileNameWithNoRewindOrReversedContext[] fileWithPropsContexts, SyntaxProperty<OpenMode> openMode) {
			foreach (var fileWithPropsCtx in fileWithPropsContexts) {
				var openFileInstruction = new OpenFileInstruction();
				openFileInstruction.OpenMode = openMode;
				openFileInstruction.FileName = CobolWordsBuilder.CreateFileNameReference(fileWithPropsCtx.fileNameReference());
				if (fileWithPropsCtx.REWIND() != null) {
					openFileInstruction.IsWithNoRewind = CreateSyntaxProperty(true, fileWithPropsCtx.REWIND());
				}
				else
				if (fileWithPropsCtx.REVERSED() != null) {
					openFileInstruction.IsReversed = CreateSyntaxProperty(true, fileWithPropsCtx.REVERSED());
				}
				openFileInstructions.Add(openFileInstruction);
			}
		}

		///////////////////////
		// PERFORM STATEMENT //
		///////////////////////

		internal CodeElement CreatePerformStatement(CodeElementsParser.PerformStatementContext context)
		{
			var statement = new PerformStatement();

			CreatePerformStatementIteration(statement, context.performTimesPhrase(), context.performUntilPhrase(), context.performVaryingPhrase());

			return statement;
		}

		internal CodeElement CreatePerformProcedureStatement(CodeElementsParser.PerformStatementContext context)
		{
			var statement = new PerformProcedureStatement();

			if(context.procedureName() != null)
			{
				statement.Procedure = CobolWordsBuilder.CreateProcedureName(context.procedureName());
			}
			else if(context.proceduresRange() != null)
			{
				statement.Procedure = CobolWordsBuilder.CreateProcedureName(context.proceduresRange().startProcedure);
				statement.ThroughProcedure = CobolWordsBuilder.CreateProcedureName(context.proceduresRange().endProcedure);
			}
			CreatePerformStatementIteration(statement, context.performTimesPhrase(), context.performUntilPhrase(), context.performVaryingPhrase());

			return statement;
		}

		private void CreatePerformStatementIteration(PerformStatement statement, CodeElementsParser.PerformTimesPhraseContext timesCtx, CodeElementsParser.PerformUntilPhraseContext untilCtx, CodeElementsParser.PerformVaryingPhraseContext varyingCtx)
		{
			if(timesCtx != null)
			{
				statement.IterationType = CreateSyntaxProperty(PerformIterationType.Times,
					timesCtx.TIMES());
				statement.TimesIterationCount = CobolExpressionsBuilder.CreateNumericVariable(
					timesCtx.numericVariable3());
			}
			else if(untilCtx != null)
			{
				statement.IterationType = CreateSyntaxProperty(PerformIterationType.Until,
					untilCtx.UNTIL());
				if(untilCtx.conditionTestTime() != null)
				{
					CreateConditionTestTime(statement, untilCtx.conditionTestTime());
				}
				statement.UntilTerminationCondition = CobolExpressionsBuilder.CreateConditionalExpression(
					untilCtx.conditionalExpression());
			}
			else if(varyingCtx != null)
			{
				statement.IterationType = CreateSyntaxProperty(PerformIterationType.Varying,
					varyingCtx.VARYING());
				if (varyingCtx.conditionTestTime() != null)
				{
					CreateConditionTestTime(statement, varyingCtx.conditionTestTime());
				}
				statement.VaryingLoopDescriptions = BuildObjectArrayFromParserRules(varyingCtx.loopVariableDescription(), ctx => CreatePerformLoopDescription(ctx));
			}
		}

		private void CreateConditionTestTime(PerformStatement statement, CodeElementsParser.ConditionTestTimeContext conditionTestTimeCtx)
		{
			if (conditionTestTimeCtx.BEFORE() != null)
			{
				statement.TerminationConditionTestTime = CreateSyntaxProperty(
					TerminationConditionTestTime.BeforeIteration,
					conditionTestTimeCtx.BEFORE());
			}
			else if (conditionTestTimeCtx.AFTER() != null)
			{
				statement.TerminationConditionTestTime = CreateSyntaxProperty(
					TerminationConditionTestTime.AfterIteration,
					conditionTestTimeCtx.AFTER());
			}
		}

		private PerformLoopDescription CreatePerformLoopDescription(CodeElementsParser.LoopVariableDescriptionContext context)
		{
			var loop = new PerformLoopDescription();

			loop.LoopVariable = CobolExpressionsBuilder.CreateDataOrIndexStorageArea(
				context.loopVariable);
			loop.InitialValue = CobolExpressionsBuilder.CreateNumericVariableOrIndex(
				context.initialValue);
			loop.Increment = CobolExpressionsBuilder.CreateNumericVariable(
				context.increment);
			loop.TerminationCondition = CobolExpressionsBuilder.CreateConditionalExpression(
				context.conditionalExpression());

			return loop;
		}

		  ////////////////////
		 // READ STATEMENT //
		////////////////////

		internal CodeElement CreateReadStatement(CodeElementsParser.ReadStatementContext context) {
			var statement = new ReadStatement();
            statement.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
		    statement.KeyDataItem = CobolWordsBuilder.CreateQualifiedDataName(context.qualifiedDataName());
		    statement.ReadNextRecord = CreateSyntaxProperty(true, context.NEXT());
			statement.ReceivingStorageArea = CobolExpressionsBuilder.CreateStorageArea(context.storageArea1());
			return statement;
		}

		  ///////////////////////
		 // RELEASE STATEMENT //
		///////////////////////

		internal CodeElement CreateReleaseStatement(CodeElementsParser.ReleaseStatementContext context) {
			var statement = new ReleaseStatement();
			// !! TODO !! RecordName should be of type ReceivingStorageArea, because
			// it can be the target of a MOVE when FROM is used
			statement.RecordName = CobolWordsBuilder.CreateRecordName(context.recordName());
			statement.FromVariable = CobolExpressionsBuilder.CreateVariable(context.variable1());
			return statement;
		}
		
		  //////////////////////
		 // RETURN STATEMENT //
		//////////////////////

		internal ReturnStatement CreateReturnStatement(Generated.CodeElementsParser.ReturnStatementContext context) {
			var statement = new ReturnStatement();
			statement.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
			statement.IntoStorageArea = CobolExpressionsBuilder.CreateStorageArea(context.storageArea1());
			return statement;
		}
		
		  ///////////////////////
		 // REWRITE STATEMENT //
		///////////////////////

		internal CodeElement CreateRewriteStatement(CodeElementsParser.RewriteStatementContext context) {
			var statement = new RewriteStatement();
			statement.RecordName = CobolWordsBuilder.CreateRecordName(context.recordName());
			if (context.sendingField != null) {
				statement.FromVariable = CobolExpressionsBuilder.CreateVariable(context.sendingField);
			}
			return statement;
		}
		
		  //////////////////////
		 // SEARCH STATEMENT //
		//////////////////////

		internal CodeElement CreateSerialSearchStatement(CodeElementsParser.SerialSearchContext context) {
			var statement = new SearchSerialStatement();
			statement.TableToSearch = CobolExpressionsBuilder.CreateVariable(context.variable1());
			if(context.dataOrIndexStorageArea() != null) {
				statement.VaryingSearchIndex = CobolExpressionsBuilder.CreateDataOrIndexStorageArea(context.dataOrIndexStorageArea());
			}
			return statement;
		}

		internal CodeElement CreateBinarySearchStatement(CodeElementsParser.BinarySearchContext context) {
			var statement = new SearchBinaryStatement();
			statement.TableToSearch = CobolExpressionsBuilder.CreateVariable(context.variable1());
			return statement;
		}

		  ///////////////////
		 // SET STATEMENT //
		///////////////////

		internal CodeElement CreateSetStatementForAssignment(CodeElementsParser.SetStatementForAssignmentContext context) {
			var statement = new SetStatementForAssignment();
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.dataOrIndexStorageArea(), ctx => CobolExpressionsBuilder.CreateDataOrIndexStorageArea(ctx));
			statement.SendingVariable = CreateSendingVariable(context.setSendingField());
			return statement;
		}

		private SetSendingVariable CreateSendingVariable(CodeElementsParser.SetSendingFieldContext context) {
			var variable = new SetSendingVariable();
			if(context.integerVariableOrIndex1() != null) {
				variable.IntegerVariableOrIndex = CobolExpressionsBuilder.CreateIntegerVariableOrIndex(
					context.integerVariableOrIndex1());
			} else
			if(context.nullPointerValue() != null) {
				variable.NullPointerValue = CobolWordsBuilder.CreateNullPointerValue(context.nullPointerValue());
			} else
			if(context.programNameOrProgramEntryVariable() != null) {
				variable.ProgramNameOrProgramEntryVariable = CobolExpressionsBuilder.CreateProgramNameOrProgramEntryVariable(context.programNameOrProgramEntryVariable());
			} else
			if(context.selfObjectIdentifier() != null) {
				variable.SelfObjectIdentifier = ParseTreeUtils.GetFirstToken(context.selfObjectIdentifier());
			}
			return variable;
		}

		internal CodeElement CreateSetStatementForIndexes(CodeElementsParser.SetStatementForIndexesContext context) {
			var statement = new SetStatementForIndexes();
			statement.ReceivingIndexes = BuildObjectArrayFromParserRules(context.indexStorageArea(), ctx => CobolExpressionsBuilder.CreateIndexStorageArea(ctx));
			if(context.UP() != null) {
				statement.IncrementDirection = CreateSyntaxProperty(IndexIncrementDirection.Up, context.UP());
			} else
			if (context.DOWN() != null) {
				statement.IncrementDirection = CreateSyntaxProperty(IndexIncrementDirection.Down, context.DOWN());
			}
			statement.SendingVariable = CobolExpressionsBuilder.CreateIntegerVariable(context.integerVariable1());
			return statement;
		}

		internal CodeElement CreateSetStatementForSwitches(CodeElementsParser.SetStatementForSwitchesContext context) {
			var statement = new SetStatementForSwitches();
			if (context.setSwitchPosition() != null && context.setSwitchPosition().Length > 0) {
				statement.SetUPSISwitchInstructions = new List<SetUPSISwitchInstruction>();
				SyntaxProperty<UPSISwitchPosition> upsiSwitchPosition = null;
				foreach (var setSwitchContext in context.setSwitchPosition()) {
					if (setSwitchContext.ON() != null) {
						upsiSwitchPosition = CreateSyntaxProperty<UPSISwitchPosition>(UPSISwitchPosition.On, setSwitchContext.ON());
					} else
					if (setSwitchContext.OFF() != null) {
						upsiSwitchPosition = CreateSyntaxProperty<UPSISwitchPosition>(UPSISwitchPosition.Off, setSwitchContext.OFF());
					}

					foreach (var mnemonicContext in setSwitchContext.mnemonicForUPSISwitchNameReference()) {
						var setSwitchInstruction = new SetUPSISwitchInstruction();
						setSwitchInstruction.MnemonicForUPSISwitchName = CobolWordsBuilder.CreateMnemonicForUPSISwitchNameReference(mnemonicContext);
						setSwitchInstruction.SwitchPosition = upsiSwitchPosition;
						statement.SetUPSISwitchInstructions.Add(setSwitchInstruction);
					}
				}
			}
			return statement;
		}

		internal CodeElement CreateSetStatementForConditions(CodeElementsParser.SetStatementForConditionsContext context) {
			var statement = new SetStatementForConditions();
			statement.Conditions = BuildObjectArrayFromParserRules(context.conditionStorageArea(), ctx => CobolExpressionsBuilder.CreateConditionStorageArea(ctx));
            if (context.TRUE()  != null) statement.SendingValue = CobolWordsBuilder.CreateBooleanValue(context.TRUE());
			if (context.FALSE() != null) statement.SendingValue = CobolWordsBuilder.CreateBooleanValue(context.FALSE());
            return statement;
		}
		
		  ////////////////////
		 // SORT STATEMENT //
		////////////////////

		internal SortStatement CreateSortStatement(CodeElementsParser.SortStatementContext context)
		{
			var statement = new SortStatement();

			statement.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
			statement.SortingKeys = CreateSortingKeys(context.onAscendingDescendingKey());
			if(context.DUPLICATES() != null)
			{
				statement.WithDuplicates = CreateSyntaxProperty(true, context.DUPLICATES());
			}
			if (context.collatingSequence() != null)
			{
				statement.CollatingSequence = CobolWordsBuilder.CreateAlphabetNameReference(
					context.collatingSequence().alphabetNameReference());
			}

			if (context.usingFilenames() != null)
			{
				statement.InputFiles = BuildObjectArrayFromParserRules(context.usingFilenames().fileNameReference(), ctx => CobolWordsBuilder.CreateFileNameReference(ctx));
				//if (statement.Using.Count == 1)
				//    DiagnosticUtils.AddError(statement, "MERGE: USING <filename> <filename>+", context.usingFilenames());
			}
			else if (context.inputProcedure() != null)
			{
				if (context.inputProcedure().procedureName() != null)
				{
					statement.InputProcedure = CobolWordsBuilder.CreateProcedureName(context.inputProcedure().procedureName());
				}
				else if (context.inputProcedure().proceduresRange() != null)
				{
					statement.InputProcedure = CobolWordsBuilder.CreateProcedureName(context.inputProcedure().proceduresRange().startProcedure);
					statement.ThroughInputProcedure = CobolWordsBuilder.CreateProcedureName(context.inputProcedure().proceduresRange().endProcedure);
				}
			}

			if (context.givingFilenames() != null)
			{
				statement.OutputFiles = BuildObjectArrayFromParserRules(context.givingFilenames().fileNameReference(), ctx => CobolWordsBuilder.CreateFileNameReference(ctx));
			}
			else if (context.outputProcedure() != null)
			{
				if (context.outputProcedure().procedureName() != null)
				{
					statement.OutputProcedure = CobolWordsBuilder.CreateProcedureName(context.outputProcedure().procedureName());
				}
				else if (context.outputProcedure().proceduresRange() != null)
				{
					statement.OutputProcedure = CobolWordsBuilder.CreateProcedureName(context.outputProcedure().proceduresRange().startProcedure);
					statement.ThroughOutputProcedure = CobolWordsBuilder.CreateProcedureName(context.outputProcedure().proceduresRange().endProcedure);
				}
			}

			return statement;
		}

        /////////////////////
        // START STATEMENT //
        /////////////////////

        internal CodeElement CreateStartStatement(CodeElementsParser.StartStatementContext context)
        {
            var statement = new StartStatement();

            statement.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());
            if (context.relationalOperator() != null)
            {
                statement.RelationalOperator = CobolExpressionsBuilder.CreateRelationalOperator(context.relationalOperator());
                statement.KeyValue = CobolExpressionsBuilder.CreateVariable(context.variable1());
            }

            return statement;
        }

        ////////////////////
        // STOP STATEMENT //
        ////////////////////

        internal CodeElement CreateStopStatement(CodeElementsParser.StopStatementContext context)
        {
            var statement = new StopStatement();

            if (context.RUN() != null)
            {
                statement.StopRun = CreateSyntaxProperty(true,
                    context.RUN());
            }
            if (context.messageToOperator() != null)
            {
                if (context.messageToOperator().numericValue() != null)
                {
                    statement.ReturnCode = CobolWordsBuilder.CreateNumericValue(context.messageToOperator().numericValue());
                }
                else if (context.messageToOperator().alphanumericValue3() != null)
                {
                    statement.ReturnMessage = CobolWordsBuilder.CreateAlphanumericValue(context.messageToOperator().alphanumericValue3());
                }
                else if(context.messageToOperator().nullFigurativeConstant() != null)
                {
                    if (context.messageToOperator().nullFigurativeConstant().NULL() != null)
                    {
                        statement.ReturnNull = CreateSyntaxProperty(true, context.messageToOperator().nullFigurativeConstant().NULL());
                    }
                    if (context.messageToOperator().nullFigurativeConstant().NULLS() != null)
                    {
                        statement.ReturnNull = CreateSyntaxProperty(true, context.messageToOperator().nullFigurativeConstant().NULLS());
                    }
                }
            }

            return statement;
        }

        //////////////////////
        // STRING STATEMENT //
        //////////////////////

        internal CodeElement CreateStringStatement(CodeElementsParser.StringStatementContext context) {
			var statement = new StringStatement();

            statement.StringContentsToConcatenate = BuildObjectArrayFromParserRules(context.contentToConcatenate(), ctx => CreateStringContentToConcatenate(ctx));
            statement.ReceivingField = CobolExpressionsBuilder.CreateStorageArea(context.receivingField);
			if (context.characterPositionInReceivingField != null)
            {
				statement.CharacterPositionInReceivingField = CobolExpressionsBuilder.CreateStorageArea(context.characterPositionInReceivingField);
			}

			return statement;
		}

        private StringStatement.ContentToConcatenate CreateStringContentToConcatenate(CodeElementsParser.ContentToConcatenateContext context)
        {
            var stringContent = new StringStatement.ContentToConcatenate();
            stringContent.SendingFields = BuildObjectArrayFromParserRules(context.variable6(), ctx => CobolExpressionsBuilder.CreateVariable(ctx));
            if(context.SIZE() != null)
            {
                stringContent.IsDelimitedbySize = CreateSyntaxProperty(true, context.SIZE());
            }
            else
            {
                stringContent.DelimiterCharacters = CobolExpressionsBuilder.CreateVariable(context.delimiterCharacters);
            }
            return stringContent;
        }

        ////////////////////////
        // SUBTRACT STATEMENT //
        ////////////////////////

        internal CodeElement CreateSubtractStatement(CodeElementsParser.SubtractSimpleContext context) {
			var statement = new SubtractSimpleStatement();
			statement.VariablesTogether = BuildObjectArrayFromParserRules(context.numericVariable3(), ctx => CobolExpressionsBuilder.CreateNumericVariable(ctx));
			statement.SendingAndReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		internal CodeElement CreateSubtractGivingStatement(CodeElementsParser.SubtractGivingContext context) {
			var statement = new SubtractGivingStatement();
			statement.VariablesTogether = BuildObjectArrayFromParserRules(context.numericVariable3(), ctx => CobolExpressionsBuilder.CreateNumericVariable(ctx));
			statement.Operand = CobolExpressionsBuilder.CreateNumericVariable(context.fromOperand);
			statement.ReceivingStorageAreas = BuildObjectArrayFromParserRules(context.numericStorageAreaRounded(), ctx => CreateRoundedResult(ctx));
			return statement;
		}

		internal CodeElement CreateSubtractCorrespondingStatement(CodeElementsParser.SubtractCorrespondingContext context) {
			var statement = new SubtractCorrespondingStatement();
			statement.GroupItem = CobolExpressionsBuilder.CreateDataItemReference(context.groupItem);
			statement.SendingAndReceivingGroupItem = CobolExpressionsBuilder.CreateDataItemReference(context.fromGroupItem);
			statement.Rounded = CreateSyntaxProperty(true, context.ROUNDED());

            // Collect storage area read/writes at the code element level
            if (statement.GroupItem != null && statement.SendingAndReceivingGroupItem != null)
            {
                CobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact = new GroupCorrespondingImpact()
                {
                    SendingGroupItem = statement.GroupItem,
                    ReceivingGroupItem = statement.SendingAndReceivingGroupItem,
                    ReceivingGroupIsAlsoSending = true
                };
            }
            return statement;
		}

		  ////////////////////////
		 // UNSTRING STATEMENT //
		////////////////////////

		internal CodeElement CreateUnstringStatement(CodeElementsParser.UnstringStatementContext context)
        {
			var statement = new UnstringStatement();
			statement.SendingField = CobolExpressionsBuilder.CreateVariable(context.variable1());
            if (context.unstringDelimiter() != null)
            {
                statement.Delimiters = BuildObjectArrayFromParserRules(context.unstringDelimiter(), ctx => CreateUnstringDelimiter(ctx));
            }
            statement.ReceivingFields = BuildObjectArrayFromParserRules(context.unstringReceivingFields(), ctx => CreateUnstringReceivingFields(ctx));
            if (context.relativeCharacterPositionDuringProcessing != null)
            {
                statement.CharacterPositionsExaminedInSendingField = CobolExpressionsBuilder.CreateStorageArea(context.relativeCharacterPositionDuringProcessing);
            }
            if (context.incrementByNumberOfDelimitedFields != null)
            {
                statement.NumberOfDelimitedFieldsProcessed = CobolExpressionsBuilder.CreateStorageArea(context.incrementByNumberOfDelimitedFields);
            }
			return statement;
		}
               
        private UnstringStatement.Delimiter CreateUnstringDelimiter(CodeElementsParser.UnstringDelimiterContext context)
        {
            var delimiter = new UnstringStatement.Delimiter();
            if (context.ALL() != null)
            {
                delimiter.All = CreateSyntaxProperty(true, context.ALL());
            }
            delimiter.DelimiterCharacters = CobolExpressionsBuilder.CreateVariable(context.delimiterCharacters);
            return delimiter;
        }

        private UnstringStatement.Receiving CreateUnstringReceivingFields(CodeElementsParser.UnstringReceivingFieldsContext context)
        {
            var receivingField = new UnstringStatement.Receiving();
            receivingField.ReceivingField = CobolExpressionsBuilder.CreateStorageArea(context.receivingField);
            if (context.associatedDelimiter != null)
            {
                receivingField.DelimiterReceivingField = CobolExpressionsBuilder.CreateStorageArea(context.associatedDelimiter);
            }
            if (context.charsTransferredCount != null)
            {
                receivingField.CharTransferredCount = CobolExpressionsBuilder.CreateStorageArea(context.charsTransferredCount);
            }
            return receivingField;
        }

        /////////////////////////////
        // WHEN EVALUATE CONDITION //
        /////////////////////////////

        internal CodeElement CreateWhenCondition(CodeElementsParser.WhenConditionContext context) {
			var statement = new WhenCondition();
			statement.SelectionObjects = BuildObjectArrayFromParserRules(context.comparisonRHSExpression(), ctx => CreateEvaluateSelectionObject(ctx));
			return statement;
		}

		private EvaluateSelectionObject CreateEvaluateSelectionObject(CodeElementsParser.ComparisonRHSExpressionContext context) {
			var selectionObject = new EvaluateSelectionObject();
			if(context.ANY() != null) {
				selectionObject.IsAny = CreateSyntaxProperty(true, context.ANY());
			} else
			if (context.booleanValueOrExpression() != null) {
				selectionObject.BooleanComparisonVariable = CobolExpressionsBuilder.CreateBooleanValueOrExpression(context.booleanValueOrExpression());
			} else {
				selectionObject.InvertAlphanumericComparison = CreateSyntaxProperty(true, context.NOT());
				if (context.variableOrExpression2() != null) {
					selectionObject.AlphanumericComparisonVariable = CobolExpressionsBuilder.CreateVariableOrExpression(context.variableOrExpression2());
				} else
				if(context.alphanumericExpressionsRange() != null) {
					selectionObject.AlphanumericComparisonVariable = CobolExpressionsBuilder.CreateVariableOrExpression(context.alphanumericExpressionsRange().startExpression);
					selectionObject.AlphanumericComparisonVariable2 = CobolExpressionsBuilder.CreateVariableOrExpression(context.alphanumericExpressionsRange().endExpression);
				}
			}
			return selectionObject;
		}

		  ///////////////////////////
		 // WHEN SEARCH CONDITION //
		///////////////////////////

		internal CodeElement CreateWhenSearchCondition(CodeElementsParser.WhenSearchConditionContext context) {
			var condition = new WhenSearchCondition();
			condition.Condition = CobolExpressionsBuilder.CreateConditionalExpression(context.conditionalExpression());
			return condition;
		}

		  /////////////////////
		 // WRITE STATEMENT //
		/////////////////////

		internal CodeElement CreateWriteStatement(CodeElementsParser.WriteStatementContext context) {
			var statement = new WriteStatement();
			statement.RecordName = CobolWordsBuilder.CreateRecordName(context.recordName());
            if (context.sendingField != null)
            {
                statement.FromSendingField = CobolExpressionsBuilder.CreateVariable(context.sendingField);
            }
            if (context.BEFORE() != null)
            {
                statement.WriteBeforeAdvancing = CreateSyntaxProperty(true, context.BEFORE());
            }
            if (context.AFTER() != null)
            {
                statement.WriteAfterAdvancing = CreateSyntaxProperty(true, context.AFTER());
            }
            if (context.numberOfLines != null)
            {
                statement.ByNumberOfLines = CobolExpressionsBuilder.CreateIntegerVariable(context.numberOfLines);
            }
            if (context.mnemonicForEnvironmentNameReference() != null)
            {
                statement.ByMnemonicForEnvironmentName = CobolWordsBuilder.CreateMnemonicForEnvironmentNameReference(context.mnemonicForEnvironmentNameReference());
            }
            if (context.PAGE() != null)
            {
                statement.ByLogicalPage = CreateSyntaxProperty(true, context.PAGE());
            }
			return statement;
		}

		  ////////////////////////////
		 // XML GENERATE STATEMENT //
		//////////////////////////// 

		internal XmlGenerateStatement CreateXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context)
        {
			var statement = new XmlGenerateStatement();
			statement.ReceivingField = CobolExpressionsBuilder.CreateStorageArea(context.receivingField);
			statement.DataItemToConvertToXml = CobolExpressionsBuilder.CreateVariable(context.dataItemToConvertToXml);
            if (context.generatedXmlCharsCount != null)
            {
                statement.GeneratedXmlCharsCount = CobolExpressionsBuilder.CreateStorageArea(context.generatedXmlCharsCount);
            }
            if (context.codepage() != null)
            {
                statement.CodePage = CobolExpressionsBuilder.CreateIntegerVariable(context.codepage().integerVariable1());
            }
            if (context.XML_DECLARATION() != null)
            {
                statement.StartWithXMLDeclaration = CreateSyntaxProperty(true, context.XML_DECLARATION());
            }
            if (context.ATTRIBUTES() != null)
            {
                statement.GenerateElementaryItemsAsAttributes = CreateSyntaxProperty(true, context.ATTRIBUTES());
            }
            if (context.@namespace != null)
            {
                statement.Namespace = CobolExpressionsBuilder.CreateAlphanumericVariable(context.@namespace);
            }
            if (context.namespacePrefix != null)
            {
                statement.NamespacePrefix = CobolExpressionsBuilder.CreateAlphanumericVariable(context.namespacePrefix);
            }
            if(context.xmlNameMapping() != null)
            {
                statement.XmlNameMappings = BuildObjectArrayFromParserRules(context.xmlNameMapping(), ctx => CreateXmlNameMapping(ctx));
            }
            if (context.xmlTypeMapping() != null)
            {
                statement.XmlTypeMappings = BuildObjectArrayFromParserRules(context.xmlTypeMapping(), ctx => CreateXmlTypeMapping(ctx));
            }
            if (context.xmlSuppressDirective() != null)
            {
                statement.XmlSuppressDirectives = BuildObjectArrayFromParserRules(context.xmlSuppressDirective(),
                    ctx => CreateXmlSuppressDirective(ctx));
            }
            return statement;
		}

		private XmlNameMapping CreateXmlNameMapping(CodeElementsParser.XmlNameMappingContext context)
        {
			var nameMapping = new XmlNameMapping();
			nameMapping.DataItemName = CobolExpressionsBuilder.CreateVariable(context.subordinateDataItem);
			nameMapping.XmlNameToGenerate = CobolWordsBuilder.CreateAlphanumericValue(context.xmlNameToGenerate);
			return nameMapping;
		}

		private XmlTypeMapping CreateXmlTypeMapping(CodeElementsParser.XmlTypeMappingContext context) {
			var typeMapping = new XmlTypeMapping();
			typeMapping.DataItemName = CobolExpressionsBuilder.CreateVariable(context.subordinateDataItem);
            if (context.ATTRIBUTE() != null)
            {
                typeMapping.XmlSyntaxTypeToGenerate = CreateSyntaxProperty(XmlTypeMapping.XmlSyntaxType.ATTRIBUTE,
                    context.ATTRIBUTE());
            }
            if (context.ELEMENT() != null)
            {
                typeMapping.XmlSyntaxTypeToGenerate = CreateSyntaxProperty(XmlTypeMapping.XmlSyntaxType.ELEMENT,
                    context.ELEMENT());
            }
            if (context.CONTENT() != null)
            {
                typeMapping.XmlSyntaxTypeToGenerate = CreateSyntaxProperty(XmlTypeMapping.XmlSyntaxType.CONTENT,
                    context.CONTENT());
            }
            return typeMapping;
		}

		private XmlSuppressDirective CreateXmlSuppressDirective(CodeElementsParser.XmlSuppressDirectiveContext context) {
			var suppressDirective = new XmlSuppressDirective();
            if (context.subordinateDataItem != null)
            {
                suppressDirective.DataItemName = CobolExpressionsBuilder.CreateVariable(context.subordinateDataItem);
            }
            else
            {
                if (context.ATTRIBUTE() != null)
                {
                    if (context.NUMERIC() != null)
                        suppressDirective.XmlSyntaxTypeToSuppress = CreateSyntaxProperty(XmlSuppressDirective.XmlSyntaxType.NUMERIC_ATTRIBUTE,
                            context.NUMERIC());
                    else if (context.NONNUMERIC() != null)
                        suppressDirective.XmlSyntaxTypeToSuppress = CreateSyntaxProperty(XmlSuppressDirective.XmlSyntaxType.NONNUMERIC_ATTRIBUTE,
                            context.NONNUMERIC());
                    else
                        suppressDirective.XmlSyntaxTypeToSuppress = CreateSyntaxProperty(XmlSuppressDirective.XmlSyntaxType.ATTRIBUTE,
                            context.ATTRIBUTE());
                }
                else if (context.ELEMENT() != null)
                {
                    if (context.NUMERIC() != null)
                        suppressDirective.XmlSyntaxTypeToSuppress = CreateSyntaxProperty(XmlSuppressDirective.XmlSyntaxType.NUMERIC_ELEMENT,
                            context.NUMERIC());
                    else if (context.NONNUMERIC() != null)
                        suppressDirective.XmlSyntaxTypeToSuppress = CreateSyntaxProperty(XmlSuppressDirective.XmlSyntaxType.NONNUMERIC_ELEMENT,
                            context.NONNUMERIC());
                    else
                        suppressDirective.XmlSyntaxTypeToSuppress = CreateSyntaxProperty(XmlSuppressDirective.XmlSyntaxType.ELEMENT,
                            context.ELEMENT());
                }
            }
			suppressDirective.ItemValuesToSuppress = BuildObjectArrayFromParserRules(context.repeatedCharacterValue3(), ctx => CobolWordsBuilder.CreateRepeatedCharacterValue(ctx));
			return suppressDirective;
		}

		  /////////////////////////
		 // XML PARSE STATEMENT //
		/////////////////////////

		internal XmlParseStatement CreateXmlParseStatement(CodeElementsParser.XmlParseStatementContext context) {
			var statement = new XmlParseStatement();
			statement.XmlTextToParse = CobolExpressionsBuilder.CreateVariable(context.xmlTextToParse);
            if (context.codepage() != null)
            {
                statement.CodePage = CobolExpressionsBuilder.CreateIntegerVariable(context.codepage().integerVariable1());
            }
            if (context.NATIONAL() != null)
            {
                statement.ReturningNational = CreateSyntaxProperty(true, context.NATIONAL());
            }
            if (context.optimizedXmlSchemaData != null)
            {
                statement.OptimizedXmlSchema = CobolExpressionsBuilder.CreateVariable(context.optimizedXmlSchemaData);
            }
            if (context.optimizedXmlSchemaFile != null)
            {
                statement.OptimizedXmlSchemaFile = CobolWordsBuilder.CreateXmlSchemaNameReference(context.optimizedXmlSchemaFile);
            }
            if (context.procedureName() != null)
            {
                statement.ProcessingProcedure = CobolWordsBuilder.CreateProcedureName(context.procedureName());
            }
            else if (context.proceduresRange() != null)
            {
                statement.ProcessingProcedure = CobolWordsBuilder.CreateProcedureName(context.proceduresRange().startProcedure);
                statement.ThroughProcessingProcedure = CobolWordsBuilder.CreateProcedureName(context.proceduresRange().endProcedure);
            }
            return statement;
		}
        


        /////////////////////
        // UTILITY METHODS //
        /////////////////////

        private O[] BuildObjectArrrayWithNoNullFromParserRules<R, O>(R[] parserRules, Func<R, O> createObject)
        {
            var objectList = new List<O>();
            
            if (parserRules != null && parserRules.Length > 0)
            {
                for (int i = 0; i < parserRules.Length; i++)
                {
                    var obj = createObject(parserRules[i]);
                    if (obj != null)
                        objectList.Add(obj);
                }
            }
            return objectList.ToArray();
        }

        private O[] BuildObjectArrayFromParserRules<R, O>(R[] parserRules, Func<R, O> createObject) {
			O[] objectArray = new O[parserRules.Length];
			if (parserRules != null && parserRules.Length > 0) {
				for (int i = 0; i < parserRules.Length; i++) {
					objectArray[i] = createObject(parserRules[i]);
				}
			}
			return objectArray;
		}

		internal static SyntaxProperty<T> CreateSyntaxProperty<T>(T value, ITerminalNode terminalNode) {
			if (terminalNode == null) return null;
			return new SyntaxProperty<T>(value, ParseTreeUtils.GetFirstToken(terminalNode));
		}
	}
}
