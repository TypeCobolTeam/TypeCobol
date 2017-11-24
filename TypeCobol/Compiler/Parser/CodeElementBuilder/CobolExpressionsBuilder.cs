using System;
using JetBrains.Annotations;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Parser
{
	internal class CobolExpressionsBuilder
	{
        // Storage area definitions (explicit data definitions AND compiler generated storage area allocations)
       internal IDictionary<SymbolDefinition, DataDescriptionEntry> storageAreaDefinitions { get; set; }
        
        // List of storage areas read from by this CodeElement
        internal IList<StorageArea> storageAreaReads { get; set; }

        // List of storage areas written to by this CodeElement
        internal IList<ReceivingStorageArea> storageAreaWrites { get; set; }

        // Impacts which we will need to resolve at the next stage between two group items
        // because of MOVE CORRESPONDING, ADD CORRESPONDING, and SUBTRACT CORRESPONDING statements
        internal GroupCorrespondingImpact storageAreaGroupsCorrespondingImpact { get; set; }

        // List of program, method, or function entry points which can be target by call instructions (with shared storage areas)
        internal CallTarget callTarget { get; set; }

        // List of program, method, or function call instructions (with shared sotrage areas)
        internal IList<CallSite> callSites { get; set; }

        public CobolExpressionsBuilder(CobolWordsBuilder cobolWordsBuilder)
		{
			CobolWordsBuilder = cobolWordsBuilder;
            storageAreaDefinitions = new Dictionary<SymbolDefinition, DataDescriptionEntry>();
            storageAreaReads = new List<StorageArea>();
            storageAreaWrites = new List<ReceivingStorageArea>();
            callTarget = null;
            callSites = new List<CallSite>();
        }

		private CobolWordsBuilder CobolWordsBuilder { get; set; }

		// --- (Data storage area) Identifiers ---

		// - 1. Table elements reference : subscripting data names or condition names -

		[CanBeNull]
		internal DataOrConditionStorageArea CreateDataItemReference([CanBeNull]CodeElementsParser.DataItemReferenceContext context) {
			if (context == null) return null;
			SymbolReference qualifiedDataName = CobolWordsBuilder.CreateQualifiedDataName(context.qualifiedDataName());
			if (context.subscript() == null || context.subscript().Length == 0) {
				return new DataOrConditionStorageArea(qualifiedDataName);
			} else {
				return new DataOrConditionStorageArea(qualifiedDataName, CreateSubscriptExpressions(context.subscript()));
			}
		}

		internal DataOrConditionStorageArea CreateConditionReference(CodeElementsParser.ConditionReferenceContext context)
		{
			SymbolReference qualifiedConditionName = CobolWordsBuilder.CreateQualifiedConditionName(context.qualifiedConditionName());
			if (context.subscript() == null || context.subscript().Length == 0)
			{
				return new DataOrConditionStorageArea(qualifiedConditionName);
			}
			else
			{
				return new DataOrConditionStorageArea(qualifiedConditionName,
					CreateSubscriptExpressions(context.subscript()));
			}
		}

		internal DataOrConditionStorageArea CreateDataItemReferenceOrConditionReference(CodeElementsParser.DataItemReferenceOrConditionReferenceContext context)
		{
			SymbolReference qualifiedDataNameOrQualifiedConditionName = CobolWordsBuilder.CreateQualifiedDataNameOrQualifiedConditionName(context.qualifiedDataNameOrQualifiedConditionName());
            if (qualifiedDataNameOrQualifiedConditionName == null)
                return null;
            if (context.subscript() == null || context.subscript().Length == 0)
			{
				return new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionName);
			}
			else
			{
				return new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionName,
					CreateSubscriptExpressions(context.subscript()));
			}
		}

        internal DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrTCFunctionProcedure(CodeElementsParser.DataItemReferenceOrConditionReferenceContext context)
		{
			SymbolReference qualifiedDataNameOrQualifiedConditionName = CobolWordsBuilder.CreateQualifiedDataNameOrQualifiedConditionNameOrTCFunctionProcedure(context.qualifiedDataNameOrQualifiedConditionName());
			if (context.subscript() == null || context.subscript().Length == 0)
			{
				return new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionName);
			}
			else
			{
				return new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionName,
					CreateSubscriptExpressions(context.subscript()));
			}
		}


        

        internal DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrIndexName(CodeElementsParser.DataItemReferenceOrConditionReferenceOrIndexNameContext context)
		{
			SymbolReference qualifiedDataNameOrQualifiedConditionNameOrIndexName = CobolWordsBuilder.CreateQualifiedDataNameOrQualifiedConditionNameOrIndexName(context.qualifiedDataNameOrQualifiedConditionNameOrIndexName());
			DataOrConditionStorageArea storageArea = null;
			if (context.subscript() == null || context.subscript().Length == 0)
			{
				storageArea = new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionNameOrIndexName);
			}
			else
			{
				storageArea = new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionNameOrIndexName,
					CreateSubscriptExpressions(context.subscript()));
			}
			storageArea.AlternativeSymbolType = SymbolType.IndexName;
			return storageArea;
		}

		internal DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrFileName(CodeElementsParser.DataItemReferenceOrConditionReferenceOrFileNameContext context)
		{
			SymbolReference qualifiedDataNameOrQualifiedConditionNameOrFileName = CobolWordsBuilder.CreateQualifiedDataNameOrQualifiedConditionNameOrFileName(context.qualifiedDataNameOrQualifiedConditionNameOrFileName());
			DataOrConditionStorageArea storageArea = null;
			if (context.subscript() == null || context.subscript().Length == 0)
			{
				storageArea = new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionNameOrFileName);
			}
			else
			{
				storageArea = new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionNameOrFileName,
					CreateSubscriptExpressions(context.subscript()));
			}
			storageArea.AlternativeSymbolType = SymbolType.IndexName;
			return storageArea;
		}

		internal DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrClassName(CodeElementsParser.DataItemReferenceOrConditionReferenceOrClassNameContext context)
		{
			SymbolReference qualifiedDataNameOrQualifiedConditionNameOrClassName = CobolWordsBuilder.CreateQualifiedDataNameOrQualifiedConditionNameOrClassName(context.qualifiedDataNameOrQualifiedConditionNameOrClassName());
			DataOrConditionStorageArea storageArea = null;
			if (context.subscript() == null || context.subscript().Length == 0)
			{
				storageArea = new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionNameOrClassName);
			}
			else
			{
				storageArea = new DataOrConditionStorageArea(qualifiedDataNameOrQualifiedConditionNameOrClassName,
					CreateSubscriptExpressions(context.subscript()));
			}
			storageArea.AlternativeSymbolType = SymbolType.IndexName;
			return storageArea;
		}

		internal SubscriptExpression[] CreateSubscriptExpressions(CodeElementsParser.SubscriptContext[] contextArray)
		{
			SubscriptExpression[] subscriptExpressions = new SubscriptExpression[contextArray.Length];
			for(int i = 0; i < contextArray.Length; i++)
			{
				CodeElementsParser.SubscriptContext context = contextArray[i];
				if(context.ALL() != null)
				{
					subscriptExpressions[i] = new SubscriptExpression(
						ParseTreeUtils.GetFirstToken(context.ALL()));
				}
				else
				{
					IntegerVariable integerVariable = CreateIntegerVariableOrIndex(context.integerVariableOrIndex2());
					ArithmeticExpression arithmeticExpression = new NumericVariableOperand(integerVariable);
					if(context.withRelativeSubscripting() != null)
					{
						SyntaxProperty<ArithmeticOperator> arithmeticOperator = null;
						if(context.withRelativeSubscripting().PlusOperator() != null)
						{
							arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
								ArithmeticOperator.Plus,
								ParseTreeUtils.GetFirstToken(context.withRelativeSubscripting().PlusOperator()));
						}
						else
						{
							arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
								ArithmeticOperator.Minus,
								ParseTreeUtils.GetFirstToken(context.withRelativeSubscripting().MinusOperator()));
						}

						IntegerVariable integerVariable2 = new IntegerVariable(
							CobolWordsBuilder.CreateIntegerValue(context.withRelativeSubscripting().integerValue()));
						ArithmeticExpression numericOperand2 = new NumericVariableOperand(integerVariable2);

						arithmeticExpression = new ArithmeticOperation(
							arithmeticExpression,
							arithmeticOperator,
							numericOperand2);
					}
					subscriptExpressions[i] = new SubscriptExpression(arithmeticExpression);
				}
			}
			return subscriptExpressions;
		}

		// - 2. Special registers (allocate a storage area on reference) -

		internal StorageArea CreateLinageCounterSpecialRegister(CodeElementsParser.LinageCounterSpecialRegisterContext context)
		{
			var specialRegister = new FilePropertySpecialRegister(
				ParseTreeUtils.GetFirstToken(context.LINAGE_COUNTER()),
				CobolWordsBuilder.CreateFileNameReference(context.fileNameReference()));
            if(specialRegister.DataDescriptionEntry != null) {
                var dataDescription = specialRegister.DataDescriptionEntry;
                CobolWordsBuilder.symbolInformationForTokens[specialRegister.DataDescriptionEntry.DataName.NameLiteral.Token] = specialRegister.DataDescriptionEntry.DataName;
            }
            if (specialRegister.SymbolReference != null) {
                CobolWordsBuilder.symbolInformationForTokens[specialRegister.SymbolReference.NameLiteral.Token] = specialRegister.SymbolReference;
            }
            return specialRegister;
		}

		internal StorageArea CreateAddressOfSpecialRegister(CodeElementsParser.AddressOfSpecialRegisterContext context)
		{
			var specialRegister = new StorageAreaPropertySpecialRegister(
				ParseTreeUtils.GetFirstToken(context.ADDRESS()),
				CreateStorageAreaReference(context.storageAreaReference()));
            if (specialRegister.DataDescriptionEntry != null)
            {
                var dataDescription = specialRegister.DataDescriptionEntry;
                CobolWordsBuilder.symbolInformationForTokens[specialRegister.DataDescriptionEntry.DataName.NameLiteral.Token] = specialRegister.DataDescriptionEntry.DataName;
            }
            if (specialRegister.SymbolReference != null)
            {
                CobolWordsBuilder.symbolInformationForTokens[specialRegister.SymbolReference.NameLiteral.Token] = specialRegister.SymbolReference;
            }
            return specialRegister;
        }

		internal StorageArea CreateLengthOfSpecialRegister(CodeElementsParser.LengthOfSpecialRegisterContext context)
		{
			var specialRegister = new StorageAreaPropertySpecialRegister(
				ParseTreeUtils.GetFirstToken(context.LENGTH()),
				CreateStorageAreaReference(context.storageAreaReference()));
            if (specialRegister.DataDescriptionEntry != null)
            {
                var dataDescription = specialRegister.DataDescriptionEntry;
                CobolWordsBuilder.symbolInformationForTokens[specialRegister.DataDescriptionEntry.DataName.NameLiteral.Token] = specialRegister.DataDescriptionEntry.DataName;
            }
            if (specialRegister.SymbolReference != null)
            {
                CobolWordsBuilder.symbolInformationForTokens[specialRegister.SymbolReference.NameLiteral.Token] = specialRegister.SymbolReference;
            }
            return specialRegister;
        }

		// - 3. Function calls (allocate a storage area for the result) -

		internal StorageArea CreateFunctionIdentifier(CodeElementsParser.FunctionIdentifierContext context) {
			FunctionCallResult result = null;
			if (context.intrinsicFunctionCall() != null) {
				result = new FunctionCallResult(CreateIntrinsicFunctionCall(context.intrinsicFunctionCall()));
			} else { // [TYPECOBOL] user defined function calls
				result = new FunctionCallResult(CreateUserDefinedFunctionCall(context.userDefinedFunctionCall()));
			}
			if (result.DataDescriptionEntry != null) {
				var dataDescription = result.DataDescriptionEntry;
				CobolWordsBuilder.symbolInformationForTokens[result.DataDescriptionEntry.DataName.NameLiteral.Token] = result.DataDescriptionEntry.DataName;
			}
			if (result.SymbolReference != null) {
				CobolWordsBuilder.symbolInformationForTokens[result.SymbolReference.NameLiteral.Token] = result.SymbolReference;
			}
			// Register call parameters (shared storage areas) information at the CodeElement level
			var functionCall = result.FunctionCall;
			var callSite = new CallSite() {
				CallTarget = functionCall is UserDefinedFunctionCall ? ((UserDefinedFunctionCall)functionCall).UserDefinedFunctionName : null, // TO DO : IntrinsicFunctionName
				Parameters = functionCall.Arguments
			};
			this.callSites.Add(callSite);
			return result;
		}

		internal FunctionCall CreateIntrinsicFunctionCall(CodeElementsParser.IntrinsicFunctionCallContext context) {
			var name = CobolWordsBuilder.CreateIntrinsicFunctionName(context.intrinsicFunctionName());
			return new IntrinsicFunctionCall(name, CreateArguments(context.argument()));
		}

		internal FunctionCall CreateUserDefinedFunctionCall(CodeElementsParser.UserDefinedFunctionCallContext context) {
			var name = CobolWordsBuilder.CreateFunctionNameReference(context.functionNameReference());
			return new UserDefinedFunctionCall(name, CreateArguments(context.argument()));
		}

		private CallSiteParameter[] CreateArguments(CodeElementsParser.ArgumentContext[] argumentContext) {
			CallSiteParameter[] arguments = new CallSiteParameter[argumentContext.Length];
			for(int i = 0; i < argumentContext.Length; i++) {
				var variableOrExpression = CreateSharedVariableOrExpression(argumentContext[i].sharedVariableOrExpression1());
				if (variableOrExpression != null) {
					arguments[i] = new CallSiteParameter() { StorageAreaOrValue = variableOrExpression };
				}
			}
			return arguments;
		}



		internal StorageArea CreateStorageAreaReference(CodeElementsParser.StorageAreaReferenceContext context)
		{
			if(context.dataItemReference() != null)
			{
				return CreateDataItemReference(context.dataItemReference());
			}
			else
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
		}

		[CanBeNull]
		internal StorageArea CreateOtherStorageAreaReference([CanBeNull]CodeElementsParser.OtherStorageAreaReferenceContext context) {
			if (context == null) return null;
			if (context.intrinsicDataNameReference() != null) {
                SymbolReference specialRegisterName = CobolWordsBuilder.CreateInstrinsicDataNameReference(context.intrinsicDataNameReference());
				StorageArea specialRegister = new IntrinsicStorageArea(specialRegisterName);
				return specialRegister;
			}
			else if (context.autoAllocatedDataItemReference() != null)
			{
				if (context.autoAllocatedDataItemReference().linageCounterSpecialRegister() != null)
				{
					return CreateLinageCounterSpecialRegister(context.autoAllocatedDataItemReference().linageCounterSpecialRegister());
				}
				else if (context.autoAllocatedDataItemReference().addressOfSpecialRegister() != null)
				{
					return CreateAddressOfSpecialRegister(context.autoAllocatedDataItemReference().addressOfSpecialRegister());
				}
				else
				{
					return CreateLengthOfSpecialRegister(context.autoAllocatedDataItemReference().lengthOfSpecialRegister());
				}
			}
			else
			{
				return CreateFunctionIdentifier(context.functionIdentifier());
			}
		}

        [CanBeNull]
		internal StorageArea CreateStorageAreaReferenceOrConditionReference([NotNull] CodeElementsParser.StorageAreaReferenceOrConditionReferenceContext context)
		{
			if (context.dataItemReferenceOrConditionReference() != null)
			{
				return CreateDataItemReferenceOrConditionReference(context.dataItemReferenceOrConditionReference());
			}
			else if(context.otherStorageAreaReference() != null)
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
            return null;
		}

        [CanBeNull]
		internal StorageArea CreateStorageAreaReferenceOrConditionReferenceOrTCFunctionProcedure([NotNull] CodeElementsParser.StorageAreaReferenceOrConditionReferenceContext context)
		{
			if (context.dataItemReferenceOrConditionReference() != null)
			{
				return CreateDataItemReferenceOrConditionReferenceOrTCFunctionProcedure(context.dataItemReferenceOrConditionReference());
			}
			else if(context.otherStorageAreaReference() != null)
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
            return null;
		}

		internal StorageArea CreateStorageAreaReferenceOrConditionReferenceOrIndexName(CodeElementsParser.StorageAreaReferenceOrConditionReferenceOrIndexNameContext context)
		{
			if (context.dataItemReferenceOrConditionReferenceOrIndexName() != null)
			{
				return CreateDataItemReferenceOrConditionReferenceOrIndexName(context.dataItemReferenceOrConditionReferenceOrIndexName());
			}
			else
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
		}

		internal StorageArea CreateStorageAreaReferenceOrConditionReferenceOrFileName(CodeElementsParser.StorageAreaReferenceOrConditionReferenceOrFileNameContext context)
		{
			if (context.dataItemReferenceOrConditionReferenceOrFileName() != null)
			{
				return CreateDataItemReferenceOrConditionReferenceOrFileName(context.dataItemReferenceOrConditionReferenceOrFileName());
			}
			else
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
		}

		internal StorageArea CreateStorageAreaReferenceOrConditionReferenceOrClassName(CodeElementsParser.StorageAreaReferenceOrConditionReferenceOrClassNameContext context)
		{
			if (context.dataItemReferenceOrConditionReferenceOrClassName() != null)
			{
				return CreateDataItemReferenceOrConditionReferenceOrClassName(context.dataItemReferenceOrConditionReferenceOrClassName());
			}
			else
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
		}

        [CanBeNull]
		internal StorageArea CreateIdentifier(CodeElementsParser.IdentifierContext context) {
			if (context == null) return null;
			StorageArea storageArea = CreateStorageAreaReferenceOrConditionReference(context.storageAreaReferenceOrConditionReference());
			if(storageArea != null && context.referenceModifier() != null)
			{
				storageArea.ApplyReferenceModifier(CreateReferenceModifier(context.referenceModifier()));
			}
			return storageArea;
		}

        [CanBeNull]
		internal StorageArea CreateIdentifierOrTCFunctionProcedure(CodeElementsParser.IdentifierContext context) {
			if (context == null) return null;
			StorageArea storageArea = CreateStorageAreaReferenceOrConditionReferenceOrTCFunctionProcedure(context.storageAreaReferenceOrConditionReference());
            var refModifier = context.referenceModifier();

            if (storageArea != null && refModifier != null)
			{
				storageArea.ApplyReferenceModifier(CreateReferenceModifier(refModifier));
			}
			return storageArea;
		}

		private ReferenceModifier CreateReferenceModifier(CodeElementsParser.ReferenceModifierContext context)
		{
			ArithmeticExpression leftmostCharacterPosition = CreateArithmeticExpression(context.leftMostCharacterPosition);
			ArithmeticExpression length = null;
			if(context.length != null)
			{
				length = CreateArithmeticExpression(context.length);
			}
			return new ReferenceModifier(leftmostCharacterPosition, length);
		}

		internal StorageArea CreateIdentifierOrIndexName(CodeElementsParser.IdentifierOrIndexNameContext context)
		{
			StorageArea storageArea = CreateStorageAreaReferenceOrConditionReferenceOrIndexName(context.storageAreaReferenceOrConditionReferenceOrIndexName());
			if (context.referenceModifier() != null)
			{
				storageArea.ApplyReferenceModifier(
					CreateReferenceModifier(context.referenceModifier()));
			}
			return storageArea;
		}

		internal StorageArea CreateIdentifierOrFileName(CodeElementsParser.IdentifierOrFileNameContext context)
		{
			StorageArea storageArea = CreateStorageAreaReferenceOrConditionReferenceOrFileName(context.storageAreaReferenceOrConditionReferenceOrFileName());
			if (context.referenceModifier() != null)
			{
				storageArea.ApplyReferenceModifier(
					CreateReferenceModifier(context.referenceModifier()));
			}
			return storageArea;
		}

		internal StorageArea CreateIdentifierOrClassName(CodeElementsParser.IdentifierOrClassNameContext context)
		{
			StorageArea storageArea = CreateStorageAreaReferenceOrConditionReferenceOrClassName(context.storageAreaReferenceOrConditionReferenceOrClassName());
			if (context.referenceModifier() != null)
			{
				storageArea.ApplyReferenceModifier(
					CreateReferenceModifier(context.referenceModifier()));
			}
			return storageArea;
		}


		// --- Arithmetic Expressions ---

		internal ArithmeticExpression CreateArithmeticExpression(CodeElementsParser.ArithmeticExpressionContext context)
		{
			if(context.numericVariable3() != null)
			{
				return new NumericVariableOperand(
					CreateNumericVariable(context.numericVariable3()));
			}

			SyntaxProperty<ArithmeticOperator> arithmeticOperator = null;
			if (context.PlusOperator() != null)
			{
				if (context.arithmeticExpression() != null && context.arithmeticExpression().Length == 1)
				{
					arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
						ArithmeticOperator.UnaryPlus,
						ParseTreeUtils.GetFirstToken(context.PlusOperator()));
				}
				else
				{
					arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
						ArithmeticOperator.Plus,
						ParseTreeUtils.GetFirstToken(context.PlusOperator()));
				}
			}
			else if (context.MinusOperator() != null)
			{
				if (context.arithmeticExpression() != null && context.arithmeticExpression().Length == 1)
				{
					arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
						ArithmeticOperator.UnaryMinus,
						ParseTreeUtils.GetFirstToken(context.MinusOperator()));
				}
				else
				{
					arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
						ArithmeticOperator.Minus,
						ParseTreeUtils.GetFirstToken(context.MinusOperator()));
				}
			}
			else if (context.PowerOperator() != null)
			{
				arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
					ArithmeticOperator.Power,
					ParseTreeUtils.GetFirstToken(context.PowerOperator()));
			}
			else if (context.MultiplyOperator() != null)
			{
				arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
					ArithmeticOperator.Multiply,
					ParseTreeUtils.GetFirstToken(context.MultiplyOperator()));
			}
			else if (context.DivideOperator() != null)
			{
				arithmeticOperator = new SyntaxProperty<ArithmeticOperator>(
					ArithmeticOperator.Divide,
					ParseTreeUtils.GetFirstToken(context.DivideOperator()));
			}

			if (arithmeticOperator == null)
			{
				return CreateArithmeticExpression(context.arithmeticExpression()[0]);
			}
			else
			{
				if (context.arithmeticExpression().Length == 1)
				{
					ArithmeticExpression rightOperand = CreateArithmeticExpression(context.arithmeticExpression()[0]);
					return new ArithmeticOperation(null, arithmeticOperator, rightOperand);
				}
				else
				{
					ArithmeticExpression leftOperand = CreateArithmeticExpression(context.arithmeticExpression()[0]);
					ArithmeticExpression rightOperand = CreateArithmeticExpression(context.arithmeticExpression()[1]);
					return new ArithmeticOperation(leftOperand, arithmeticOperator, rightOperand);
				}
			}
		}


		// --- Conditional Expressions ---

		internal ConditionalExpression CreateConditionalExpression(CodeElementsParser.ConditionalExpressionContext context)
		{

			if(context.classCondition() != null)
			{
				return CreateClassCondition(context.classCondition());
			}
			else if(context.conditionNameConditionOrSwitchStatusCondition() != null)
			{
				return CreateConditionNameConditionOrSwitchStatusCondition(context.conditionNameConditionOrSwitchStatusCondition());
			}
			else if(context.relationCondition() != null)
			{
				return CreateRelationCondition(context.relationCondition());
			}
			else if(context.signCondition() != null)
			{
				return CreateSignCondition(context.signCondition());
			}
			else
			{
				SyntaxProperty<LogicalOperator> logicalOperator = null;
				if(context.NOT() != null)
				{
					logicalOperator = new SyntaxProperty<LogicalOperator>(
					   LogicalOperator.NOT,
					   ParseTreeUtils.GetFirstToken(context.NOT()));
				}
				else if (context.AND() != null)
				{
					logicalOperator = new SyntaxProperty<LogicalOperator>(
					   LogicalOperator.AND,
					   ParseTreeUtils.GetFirstToken(context.AND()));
				}
				else if (context.OR() != null)
				{
					logicalOperator = new SyntaxProperty<LogicalOperator>(
					   LogicalOperator.OR,
					   ParseTreeUtils.GetFirstToken(context.OR()));
				}

				if (logicalOperator == null)
				{
                    if (context.conditionalExpression().Length == 1)
                        return CreateConditionalExpression(context.conditionalExpression()[0]);
                    else {
                        return null;
                    }
				}
				else
				{
					if (context.conditionalExpression().Length == 1)
					{
						ConditionalExpression rightOperand = CreateConditionalExpression(context.conditionalExpression()[0]);
						return new LogicalOperation(null, logicalOperator, rightOperand);
					}
					else
					{
						ConditionalExpression leftOperand = CreateConditionalExpression(context.conditionalExpression()[0]);
						ConditionalExpression rightOperand = CreateConditionalExpression(context.conditionalExpression()[1]);
						return new LogicalOperation(leftOperand, logicalOperator, rightOperand);
					}
				}
			}
		}

		internal ConditionalExpression CreateClassCondition(CodeElementsParser.ClassConditionContext context)
		{
			SyntaxProperty<bool> invertResult = null;
			if (context.NOT() != null)
			{
				invertResult = new SyntaxProperty<bool>(
					true,
					ParseTreeUtils.GetFirstToken(context.NOT()));
			}

			ClassCondition classCondition = null;
			if(context.characterClassNameReference() != null)
			{
				classCondition = new ClassCondition(
					CreateIdentifier(context.identifier()),
					CobolWordsBuilder.CreateCharacterClassNameReference(context.characterClassNameReference()), 
					invertResult);
			}
			else
			{
				DataItemContentType contentTypeEnum = DataItemContentType.Numeric;
				if(context.dataItemContentType().ALPHABETIC() != null)
				{
					contentTypeEnum = DataItemContentType.Alphabetic;
				}
				else if (context.dataItemContentType().ALPHABETIC_LOWER() != null)
				{
					contentTypeEnum = DataItemContentType.AlphabeticLower;
				}
				else if (context.dataItemContentType().ALPHABETIC_UPPER() != null)
				{
					contentTypeEnum = DataItemContentType.AlphabeticUpper;
				}
				else if (context.dataItemContentType().DBCS() != null)
				{
					contentTypeEnum = DataItemContentType.DBCS;
				}
				else if (context.dataItemContentType().KANJI() != null)
				{
					contentTypeEnum = DataItemContentType.Kanji;
				}
				SyntaxProperty<DataItemContentType> dataItemContentType = new SyntaxProperty<DataItemContentType>(
					contentTypeEnum,
					ParseTreeUtils.GetFirstToken(context.dataItemContentType()));

				classCondition = new ClassCondition(
					CreateIdentifier(context.identifier()),
					dataItemContentType,
					invertResult);
			}

            // Collect storage area read/writes at the code element level
            if (classCondition.DataItem != null)
            {
                this.storageAreaReads.Add(classCondition.DataItem);
            }

            return classCondition;
		}

		internal ConditionalExpression CreateConditionNameConditionOrSwitchStatusCondition(CodeElementsParser.ConditionNameConditionOrSwitchStatusConditionContext context)
		{
            var conditionStorageArea = CreateConditionReference(context.conditionReference());
            var condition = new ConditionNameConditionOrSwitchStatusCondition(conditionStorageArea);

            // Collect storage area read/writes at the code element level
            this.storageAreaReads.Add(conditionStorageArea);

            return condition;
		}

		internal ConditionalExpression CreateRelationCondition(CodeElementsParser.RelationConditionContext context)
		{
			ConditionOperand subjectOperand = CreateConditionOperand(context.conditionOperand());
			SyntaxProperty<RelationalOperator> relationalOperator = CreateRelationalOperator(context.relationalOperator());
			return CreateAbbreviatedExpression(subjectOperand, relationalOperator, context.abbreviatedExpression());
		}

		private ConditionalExpression CreateAbbreviatedExpression(ConditionOperand subjectOperand, SyntaxProperty<RelationalOperator> distributedRelationalOperator, CodeElementsParser.AbbreviatedExpressionContext context)
		{
			if (context.conditionOperand() != null)
			{
				ConditionOperand objectOperand = CreateConditionOperand(context.conditionOperand());
				SyntaxProperty<RelationalOperator> relationalOperator = distributedRelationalOperator;
				if (context.relationalOperator() != null)
				{
					relationalOperator = CreateRelationalOperator(context.relationalOperator());
				}
				return new RelationCondition(subjectOperand, relationalOperator, objectOperand);
			}
			else
			{
				SyntaxProperty<LogicalOperator> logicalOperator = null;
				if (context.NOT() != null)
				{
					logicalOperator = new SyntaxProperty<LogicalOperator>(
					   LogicalOperator.NOT,
					   ParseTreeUtils.GetFirstToken(context.NOT()));
				}
				else if (context.AND() != null)
				{
					logicalOperator = new SyntaxProperty<LogicalOperator>(
					   LogicalOperator.AND,
					   ParseTreeUtils.GetFirstToken(context.AND()));
				}
				else if (context.OR() != null)
				{
					logicalOperator = new SyntaxProperty<LogicalOperator>(
					   LogicalOperator.OR,
					   ParseTreeUtils.GetFirstToken(context.OR()));
				}

				if (logicalOperator == null)
				{
					return CreateAbbreviatedExpression(subjectOperand, distributedRelationalOperator, context.abbreviatedExpression()[0]);
				}
				else
				{
					if (context.abbreviatedExpression().Length == 1)
					{
						ConditionalExpression rightOperand = CreateAbbreviatedExpression(subjectOperand, distributedRelationalOperator, context.abbreviatedExpression()[0]);
						return new LogicalOperation(null, logicalOperator, rightOperand);
					}
					else
					{
						ConditionalExpression leftOperand = CreateAbbreviatedExpression(subjectOperand, distributedRelationalOperator, context.abbreviatedExpression()[0]);
						ConditionalExpression rightOperand = CreateAbbreviatedExpression(subjectOperand, distributedRelationalOperator, context.abbreviatedExpression()[1]);
						return new LogicalOperation(leftOperand, logicalOperator, rightOperand);
					}
				}
			}
		}

		private ConditionOperand CreateConditionOperand(CodeElementsParser.ConditionOperandContext context)
		{
			ConditionOperand conditionOperand = null;
			if (context.arithmeticExpression() != null)
			{
				conditionOperand = new ConditionOperand(
					CreateArithmeticExpression(context.arithmeticExpression()));
			}
			else if (context.variableOrIndex() != null)
			{
				conditionOperand = new ConditionOperand(
					CreateVariableOrIndex(context.variableOrIndex()));
			}
			else if (context.nullPointerValue() != null)
			{
				conditionOperand = new ConditionOperand(
					CobolWordsBuilder.CreateNullPointerValue(context.nullPointerValue()));
			}
			else if (context.selfObjectIdentifier() != null)
			{
				conditionOperand = new ConditionOperand(
					ParseTreeUtils.GetFirstToken(context.selfObjectIdentifier()));
			}
			return conditionOperand;
		}

		internal SyntaxProperty<RelationalOperator> CreateRelationalOperator(CodeElementsParser.RelationalOperatorContext context)
		{
			if(context.strictRelation() != null)
			{
				bool invertStrictRelation = false;
				if (context.NOT() != null)
				{
					invertStrictRelation = true;
				}

				CodeElementsParser.StrictRelationContext strictContext = context.strictRelation();
				if(strictContext.GREATER() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						!invertStrictRelation ? RelationalOperator.GreaterThan : RelationalOperator.LessThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(strictContext.GREATER()));
				}
				else if (strictContext.GreaterThanOperator() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						!invertStrictRelation ? RelationalOperator.GreaterThan : RelationalOperator.LessThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(strictContext.GreaterThanOperator()));
				}
				else if (strictContext.LESS() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						!invertStrictRelation ? RelationalOperator.LessThan : RelationalOperator.GreaterThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(strictContext.LESS()));
				}
				else if (strictContext.LessThanOperator() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						!invertStrictRelation ? RelationalOperator.LessThan : RelationalOperator.GreaterThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(strictContext.LessThanOperator()));
				}
				else if (strictContext.EQUAL() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						!invertStrictRelation ? RelationalOperator.EqualTo : RelationalOperator.NotEqualTo,
						ParseTreeUtils.GetFirstToken(strictContext.EQUAL()));
				}
				else // if (strictContext.EqualOperator() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						!invertStrictRelation ? RelationalOperator.EqualTo : RelationalOperator.NotEqualTo,
						ParseTreeUtils.GetFirstToken(strictContext.EqualOperator()));
				}
			}
			else
			{
				CodeElementsParser.SimpleRelationContext simpleContext = context.simpleRelation();
				if (simpleContext.GREATER() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						RelationalOperator.GreaterThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(simpleContext.GREATER()));
				}
				else if (simpleContext.GreaterThanOrEqualOperator() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						RelationalOperator.GreaterThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(simpleContext.GreaterThanOrEqualOperator()));
				}
				if (simpleContext.LESS() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						RelationalOperator.LessThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(simpleContext.LESS()));
				}
				else // if (simpleContext.LessThanOrEqualOperator() != null)
				{
					return new SyntaxProperty<RelationalOperator>(
						RelationalOperator.LessThanOrEqualTo,
						ParseTreeUtils.GetFirstToken(simpleContext.LessThanOrEqualOperator()));
				}
			}
		}

		internal ConditionalExpression CreateSignCondition(CodeElementsParser.SignConditionContext context)
		{
			SyntaxProperty<bool> invertResult = null;
			if (context.NOT() != null)
			{
				invertResult = new SyntaxProperty<bool>(
					true,
					ParseTreeUtils.GetFirstToken(context.NOT()));
			}
			SyntaxProperty<SignComparison> signComparison = null;
			if(context.POSITIVE() != null)
			{
				signComparison = new SyntaxProperty<SignComparison>(
					SignComparison.Positive,
					ParseTreeUtils.GetFirstToken(context.POSITIVE()));
			}
			else if (context.NEGATIVE() != null)
			{
				signComparison = new SyntaxProperty<SignComparison>(
					SignComparison.Negative,
					ParseTreeUtils.GetFirstToken(context.NEGATIVE()));
			}
			else if (context.ZERO() != null)
			{
				signComparison = new SyntaxProperty<SignComparison>(
					SignComparison.Zero,
					ParseTreeUtils.GetFirstToken(context.ZERO()));
			}

			return new SignCondition(
				CreateConditionOperand(context.conditionOperand()),
				signComparison,
				invertResult);
		}


		// --- Cobol variables : runtime value or literal ---

		internal BooleanValueOrExpression CreateBooleanValueOrExpression(CodeElementsParser.BooleanValueOrExpressionContext context)
		{
			if(context.booleanValue() != null)
			{
				return new BooleanValueOrExpression(
					CobolWordsBuilder.CreateBooleanValue(context.booleanValue()));
			}
			else
			{
				return new BooleanValueOrExpression(
					CreateConditionalExpression(context.conditionalExpression()));
			}
		}

        [CanBeNull]
        internal IntegerVariable CreateIntegerVariable([CanBeNull] CodeElementsParser.IntegerVariable1Context context)
		{
            if(context == null) return null;
            IntegerVariable variable = null;
			if(context.identifier() != null)
			{
				variable = new IntegerVariable(
					CreateIdentifier(context.identifier()));
			}
			else
			{
				variable = new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal IntegerVariable CreateIntegerVariable(CodeElementsParser.IntegerVariable2Context context)
		{
            IntegerVariable variable = null;
			if (context.dataNameReference() != null)
			{
				variable = new IntegerVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
			}
			else
			{
				variable = new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal IntegerVariable CreateIntegerVariableOrIndex(CodeElementsParser.IntegerVariableOrIndex1Context context)
		{
            IntegerVariable variable = null;
			if (context.identifierOrIndexName() != null)
			{
				variable = new IntegerVariable(
					CreateIdentifierOrIndexName(context.identifierOrIndexName()));
			}
			else
			{
				variable = new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal IntegerVariable CreateIntegerVariableOrIndex(CodeElementsParser.IntegerVariableOrIndex2Context context)
		{
            IntegerVariable variable = null;
			if (context.qualifiedDataNameOrIndexName() != null)
			{
				variable = new IntegerVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateQualifiedDataNameOrIndexName(context.qualifiedDataNameOrIndexName())));
			}
			else
			{
				variable = new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable1Context context)
		{
			var variable = new NumericVariable(
				CreateIdentifier(context.identifier()));

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable2Context context)
		{
			var variable = new NumericVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable3Context context) {
            NumericVariable variable = null;
            if (context.identifier() != null)
				variable = new NumericVariable(CreateIdentifier(context.identifier()));
			if (context.numericValue() != null)
				variable = new NumericVariable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));

            // Collect storage area read/writes at the code element level
            if (variable != null && variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal NumericVariable CreateNumericVariableOrIndex(CodeElementsParser.NumericVariableOrIndexContext context)
		{
            NumericVariable variable = null;
			if (context.identifierOrIndexName() != null)
			{
				variable = new NumericVariable(
					CreateIdentifierOrIndexName(context.identifierOrIndexName()));
			}
			else
			{
				variable = new NumericVariable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal CharacterVariable CreateCharacterVariable(CodeElementsParser.CharacterVariableContext context)
		{
            CharacterVariable variable = null;
			if (context.dataNameReference() != null)
			{
				variable = new CharacterVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
			}
			else
			{
				variable = new CharacterVariable(
					CobolWordsBuilder.CreateCharacterValue(context.characterValue4()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal AlphanumericVariable CreateAlphanumericVariable(CodeElementsParser.AlphanumericVariable1Context context)
		{
            AlphanumericVariable variable = null;
			if (context.identifier() != null)
			{
				variable = new AlphanumericVariable(
					CreateIdentifier(context.identifier()));
			}
			else
			{
				variable = new AlphanumericVariable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue3()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		[CanBeNull]
		internal AlphanumericVariable CreateAlphanumericVariable([CanBeNull] CodeElementsParser.AlphanumericVariable2Context context) {
			if (context == null) return null;
            AlphanumericVariable variable = null;
			if (context.identifier() != null) {
				variable = new AlphanumericVariable(
					CreateIdentifier(context.identifier()));
			} else {
				if (context.alphanumericValue2() != null) {
					variable = new AlphanumericVariable(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
				} else {
					variable = new AlphanumericVariable(CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
				}
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal SymbolReferenceVariable CreateProgramNameVariable(CodeElementsParser.ProgramNameVariableContext context) {
            SymbolReferenceVariable variable = null;
            if (context.programNameReference1() != null) {
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReference(context.programNameReference1());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramName, symbolReference);
			}
			if (context.identifier() != null) {
				StorageArea storageArea = CreateIdentifier(context.identifier());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramName, storageArea);
			}

            // Collect storage area read/writes at the code element level
            if (variable!= null && variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal SymbolReferenceVariable CreateProgramNameOrProgramEntryVariable(CodeElementsParser.ProgramNameOrProgramEntryVariableContext context)
		{
            SymbolReferenceVariable variable = null;
			if (context.programNameReferenceOrProgramEntryReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReferenceOrProgramEntryReference(context.programNameReferenceOrProgramEntryReference());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntry, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntry, storageArea);
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal SymbolReferenceVariable CreateProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariable(CodeElementsParser.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariableContext context)
		{
            SymbolReferenceVariable variable = null;
			if (context.programNameReferenceOrProgramEntryReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReferenceOrProgramEntryReference(context.programNameReferenceOrProgramEntryReference());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer, storageArea);
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

        internal SymbolReferenceVariable CreateProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariableOrTCFunctionProcedure(CodeElementsParser.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariableContext context)
		{
            SymbolReferenceVariable variable = null;
			if (context.programNameReferenceOrProgramEntryReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReferenceOrProgramEntryReference(context.programNameReferenceOrProgramEntryReference());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifierOrTCFunctionProcedure(context.identifier());
				variable = new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer, storageArea);
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

        internal SymbolReferenceVariable CreateProcedurePointerOrFunctionPointerVariableOrTCFunctionProcedure(CodeElementsParser.IdentifierContext context)
		{
            SymbolReferenceVariable variable = null;
			
			StorageArea storageArea = CreateIdentifierOrTCFunctionProcedure(context);
			variable = new SymbolReferenceVariable(StorageDataType.ProcedurePointerOrFunctionPointerOrTCFunctionName, storageArea);
			

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal SymbolReferenceVariable CreateClassNameOrObjectReferenceVariable(CodeElementsParser.ClassNameOrObjectReferenceVariableContext context)
		{
			StorageArea storageArea = CreateIdentifierOrClassName(context.identifierOrClassName());
            var variable = new SymbolReferenceVariable(StorageDataType.ClassNameOrObjectReference, storageArea);

            // Collect storage area read/writes at the code element level
            this.storageAreaReads.Add(storageArea);

            return variable;
		}

		internal SymbolReferenceVariable CreateMethodNameVariable(CodeElementsParser.MethodNameVariableContext context)
		{
            SymbolReferenceVariable variable = null;
			if (context.methodNameReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateMethodNameReference(context.methodNameReference());
				variable = new SymbolReferenceVariable(StorageDataType.MethodName, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				variable = new SymbolReferenceVariable(StorageDataType.MethodName, storageArea);
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal Variable CreateVariable(CodeElementsParser.Variable1Context context) {
			if (context == null) return null;
			StorageArea storageArea = CreateIdentifier(context.identifier());
			var variable = new Variable(storageArea);

            // Collect storage area read/writes at the code element level
            this.storageAreaReads.Add(storageArea);

            return variable;
        }

		internal Variable CreateVariable(CodeElementsParser.Variable2Context context)
		{
			SymbolReference qualifiedDataName = CobolWordsBuilder.CreateQualifiedDataName(context.qualifiedDataName());
			StorageArea storageArea = new DataOrConditionStorageArea(qualifiedDataName);

            // Collect storage area read/writes at the code element level
            this.storageAreaReads.Add(storageArea);

            return new Variable(storageArea);
		}

        [CanBeNull]
        internal Variable CreateVariable([NotNull] CodeElementsParser.Variable4Context context)
        {
            Variable variable = null;
            if (context.identifier() != null)
            {
                StorageArea storageArea = CreateIdentifier(context.identifier());
                variable = new Variable(storageArea);
            }
            else if (context.numericValue() != null)
            {
                variable = new Variable(
                    CobolWordsBuilder.CreateNumericValue(context.numericValue()));
            }
            else if (context.alphanumericValue3() != null)
            {
                variable = new Variable(
                    CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue3()));
            }
            else
            {
                variable = null;
            }

            // Collect storage area read/writes at the code element level
            if (variable != null && variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal Variable CreateVariable(CodeElementsParser.Variable5Context context)
		{
            Variable variable = null;
			if (context.dataNameReference() != null)
			{
				SymbolReference dataNameReference = CobolWordsBuilder.CreateDataNameReference(context.dataNameReference());
				StorageArea storageArea = new DataOrConditionStorageArea(dataNameReference);
				variable = new Variable(storageArea);
			}
			else if (context.numericValue() != null)
			{
				variable = new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else
			{
				variable = new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue3()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		[CanBeNull]
		internal Variable CreateVariable([CanBeNull]CodeElementsParser.Variable6Context context) {
			if (context == null) return null;
            Variable variable = null;
			if (context.identifier() != null) {
				StorageArea storageArea = CreateIdentifier(context.identifier());
				variable = new Variable(storageArea);
			} else
			if (context.numericValue() != null) {
				variable = new Variable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			} else
			if(context.alphanumericValue2() != null) {
				variable = new Variable(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			} else {
				variable = new Variable(CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal Variable CreateVariable(CodeElementsParser.Variable7Context context) {
			if (context == null) return null;
            Variable variable = null;
			if (context.identifier() != null) {
				variable = new Variable(CreateIdentifier(context.identifier()));
			} else
			if (context.numericValue() != null) {
				variable = new Variable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			} else
			if (context.alphanumericValue2() != null) {
				variable = new Variable(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			} else {
				variable = new Variable(CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue2()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
        }

		internal Variable CreateVariableOrIndex(CodeElementsParser.VariableOrIndexContext context)
		{
            Variable variable = null;
			if(context.identifierOrIndexName() != null)
			{
				variable = new Variable(
					CreateIdentifierOrIndexName(context.identifierOrIndexName()));
			}
			else if (context.numericValue() != null)
			{
				variable = new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if (context.alphanumericValue2() != null)
			{
				variable = new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else
			{
				variable = new Variable(
					CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue2()));
			}

            // Collect storage area read/writes at the code element level
            if (variable.StorageArea != null)
            {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}        

		internal VariableOrExpression CreateVariableOrExpression(CodeElementsParser.VariableOrExpression2Context context)
		{
            VariableOrExpression variableOrExpression = null;
			if (context.identifier() != null)
			{
                variableOrExpression = new VariableOrExpression(
					CreateIdentifier(context.identifier()));
			}
			else if (context.numericValue() != null)
			{
                variableOrExpression = new VariableOrExpression(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if (context.alphanumericValue2() != null)
			{
                variableOrExpression = new VariableOrExpression(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else if (context.repeatedCharacterValue1() != null)
			{
                variableOrExpression = new VariableOrExpression(
					CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
			}
			else
			{
                variableOrExpression = new VariableOrExpression(
					CreateArithmeticExpression(context.arithmeticExpression()));
			}

            // Collect storage area read/writes at the code element level
            if (variableOrExpression.StorageArea != null)
            {
                this.storageAreaReads.Add(variableOrExpression.StorageArea);
            }

            return variableOrExpression;
        }


        // --- Storage areas where statements results are saved ---

        internal ReceivingStorageArea CreateConditionStorageArea(CodeElementsParser.ConditionStorageAreaContext context)
        {
            var storageArea = new ReceivingStorageArea(StorageDataType.Condition,
                CreateConditionReference(context.conditionReference()));

            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(storageArea);

            return storageArea;
        }

        internal ReceivingStorageArea CreateNumericStorageArea(CodeElementsParser.NumericStorageAreaContext context)
		{
			var storageArea = new ReceivingStorageArea(StorageDataType.Numeric,
				CreateIdentifier(context.identifier()));

            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(storageArea);

            return storageArea;
		}

		internal ReceivingStorageArea CreateAlphanumericStorageArea(CodeElementsParser.AlphanumericStorageAreaContext context)
		{
			var storageArea = new ReceivingStorageArea(StorageDataType.Alphanumeric,
				CreateIdentifier(context.identifier()));

            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(storageArea);

            return storageArea;
		}

		internal ReceivingStorageArea CreateIndexStorageArea(CodeElementsParser.IndexStorageAreaContext context)
		{
			StorageArea indexStorageArea = new IndexStorageArea(CobolWordsBuilder.CreateIndexNameReference(context.qualifiedIndexName()));
            var receivingStorageArea = new ReceivingStorageArea(StorageDataType.Numeric, indexStorageArea);

            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(receivingStorageArea);

            return receivingStorageArea;
        }

		internal ReceivingStorageArea CreateDataOrIndexStorageArea(CodeElementsParser.DataOrIndexStorageAreaContext context)
		{
			var storageArea = new ReceivingStorageArea(StorageDataType.Numeric,
				CreateIdentifierOrIndexName(context.identifierOrIndexName()));

            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(storageArea);

            return storageArea;
        }

		[CanBeNull]
		internal ReceivingStorageArea CreateStorageArea([CanBeNull] CodeElementsParser.StorageArea1Context context) {
			if (context == null || context.identifier() == null) return null;
			var identifier = CreateIdentifier(context.identifier());
			if (identifier == null) return null;
		    var receivingStorageArea = new ReceivingStorageArea(StorageDataType.Any, identifier);

            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(receivingStorageArea);

            return receivingStorageArea;
		}

		internal ReceivingStorageArea CreateStorageArea(CodeElementsParser.StorageArea2Context context) {
		    var receivingStorageArea = new ReceivingStorageArea(StorageDataType.Any,
		        new DataOrConditionStorageArea(
		            CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
            
            // Collect storage area read/writes at the code element level
            this.storageAreaWrites.Add(receivingStorageArea);

            return receivingStorageArea;
		}


	    // --- Storage areas shared with calling or called program ---

		internal Variable CreateSharedVariable(CodeElementsParser.SharedVariable3Context context) {
			Variable variable = null;
			if (context.identifier() != null) {
				StorageArea storageArea = CreateIdentifier(context.identifier());
				variable = new Variable(storageArea);
			} else
			if (context.numericValue() != null) {
				variable = new Variable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			} else {
				variable = new Variable(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			// Collect storage area read/writes at the code element level
			if (variable.StorageArea != null) {
				this.storageAreaReads.Add(variable.StorageArea);
			}
			return variable;
		}

		internal Variable CreateSharedVariableOrFileName(CodeElementsParser.SharedVariableOrFileNameContext context) {
            Variable variable = null;
            if (context.identifierOrFileName() != null) {
                variable = new Variable(CreateIdentifierOrFileName(context.identifierOrFileName()));
			} else
			if (context.numericValue() != null) {
                variable = new Variable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			} else
			if (context.alphanumericValue2() != null) {
                variable = new Variable(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			} else {
                variable = new Variable(CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
			}

            if (variable.StorageArea != null) {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal VariableOrExpression CreateSharedVariableOrExpression(CodeElementsParser.SharedVariableOrExpression1Context context) {
            VariableOrExpression variable = null;
            if (context.identifier() != null) {
                variable = new VariableOrExpression(CreateIdentifier(context.identifier()));
			} else
			if (context.numericValue() != null) {
                variable = new VariableOrExpression(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			} else
			if (context.alphanumericValue2() != null) {
                variable = new VariableOrExpression(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			} else {
                variable = new VariableOrExpression(CreateArithmeticExpression(context.arithmeticExpression()));
			}

            if (variable.StorageArea != null) {
                this.storageAreaReads.Add(variable.StorageArea);
            }

            return variable;
		}

		internal StorageArea CreateSharedStorageArea(CodeElementsParser.SharedStorageArea1Context context) {
			if (context == null || context.identifier() == null) return null;
			var identifier = CreateIdentifier(context.identifier());

		    if (identifier != null)
		    {
                var receivingStorageArea = new ReceivingStorageArea(StorageDataType.Any, identifier);
                this.storageAreaWrites.Add(receivingStorageArea);
            }

			return identifier;
		}

		internal StorageArea CreateSharedStorageArea(CodeElementsParser.SharedStorageArea2Context context) {
			return new DataOrConditionStorageArea(CobolWordsBuilder.CreateDataNameReference(context.dataNameReference()));
		}
    }
}
