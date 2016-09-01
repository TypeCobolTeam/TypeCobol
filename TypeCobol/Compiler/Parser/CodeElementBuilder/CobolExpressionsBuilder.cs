using System;
using JetBrains.Annotations;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
	internal class CobolExpressionsBuilder
	{        
		public CobolExpressionsBuilder(CobolWordsBuilder cobolWordsBuilder)
		{
			CobolWordsBuilder = cobolWordsBuilder;
		}

		private CobolWordsBuilder CobolWordsBuilder { get; set; }

		// --- (Data storage area) Identifiers ---

		// - 1. Table elements reference : subscripting data names or condition names -

		internal DataOrConditionStorageArea CreateDataItemReference(CodeElementsParser.DataItemReferenceContext context)
		{
			SymbolReference qualifiedDataName = CobolWordsBuilder.CreateQualifiedDataName(context.qualifiedDataName());
			if (context.subscript() == null || context.subscript().Length == 0)
			{
				return new DataOrConditionStorageArea(qualifiedDataName);
			}
			else
			{
				return new DataOrConditionStorageArea(qualifiedDataName,
					CreateSubscriptExpressions(context.subscript()));
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
			return new FilePropertySpecialRegister(
				ParseTreeUtils.GetFirstToken(context.LINAGE_COUNTER()),
				CobolWordsBuilder.CreateFileNameReference(context.fileNameReference()));
		}

		internal StorageArea CreateAddressOfSpecialRegister(CodeElementsParser.AddressOfSpecialRegisterContext context)
		{
			return new StorageAreaPropertySpecialRegister(
				ParseTreeUtils.GetFirstToken(context.ADDRESS()),
				CreateStorageAreaReference(context.storageAreaReference()));
		}

		internal StorageArea CreateLengthOfSpecialRegister(CodeElementsParser.LengthOfSpecialRegisterContext context)
		{
			return new StorageAreaPropertySpecialRegister(
				ParseTreeUtils.GetFirstToken(context.LENGTH()),
				CreateStorageAreaReference(context.storageAreaReference()));
		}

		// - 3. Intrinsic function calls (allocate a storage area for the result) -

		internal StorageArea CreateFunctionIdentifier(CodeElementsParser.FunctionIdentifierContext context)
		{
			return new IntrinsicFunctionCallResult(
				CobolWordsBuilder.CreateIntrinsicFunctionName(context.intrinsicFunctionName()),
				CreateArguments(context.argument()));
		}

		private VariableOrExpression[] CreateArguments(CodeElementsParser.ArgumentContext[] argumentContext)
		{
			VariableOrExpression[] arguments = new VariableOrExpression[argumentContext.Length];
			for(int i = 0; i < argumentContext.Length; i++)
			{
				arguments[i] = CreateVariableOrExpression(argumentContext[i].variableOrExpression1());
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

		internal StorageArea CreateOtherStorageAreaReference(CodeElementsParser.OtherStorageAreaReferenceContext context)
		{
			if (context.intrinsicDataNameReference() != null)
			{
				SymbolReference specialRegisterName = CobolWordsBuilder.CreateInstrinsicDataNameReference(context.intrinsicDataNameReference());
				StorageArea specialRegister = new DataOrConditionStorageArea(specialRegisterName);
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

		internal StorageArea CreateStorageAreaReferenceOrConditionReference(CodeElementsParser.StorageAreaReferenceOrConditionReferenceContext context)
		{
			if (context.dataItemReferenceOrConditionReference() != null)
			{
				return CreateDataItemReferenceOrConditionReference(context.dataItemReferenceOrConditionReference());
			}
			else
			{
				return CreateOtherStorageAreaReference(context.otherStorageAreaReference());
			}
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

		internal StorageArea CreateIdentifier(CodeElementsParser.IdentifierContext context)
		{
			StorageArea storageArea = CreateStorageAreaReferenceOrConditionReference(context.storageAreaReferenceOrConditionReference());
			if(context.referenceModifier() != null)
			{
				storageArea.ApplyReferenceModifier(
					CreateReferenceModifier(context.referenceModifier()));
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
					return CreateConditionalExpression(context.conditionalExpression()[0]);
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
			
			return classCondition;
		}

		internal ConditionalExpression CreateConditionNameConditionOrSwitchStatusCondition(CodeElementsParser.ConditionNameConditionOrSwitchStatusConditionContext context)
		{
			return new ConditionNameConditionOrSwitchStatusCondition(
				CreateConditionReference(context.conditionReference()));
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

		private SyntaxProperty<RelationalOperator> CreateRelationalOperator(CodeElementsParser.RelationalOperatorContext context)
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
			if(context.identifier() != null)
			{
				return new IntegerVariable(
					CreateIdentifier(context.identifier()));
			}
			else
			{
				return new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}
		}

		internal IntegerVariable CreateIntegerVariable(CodeElementsParser.IntegerVariable2Context context)
		{
			if (context.dataNameReference() != null)
			{
				return new IntegerVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
			}
			else
			{
				return new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}
		}

		internal IntegerVariable CreateIntegerVariableOrIndex(CodeElementsParser.IntegerVariableOrIndex1Context context)
		{
			if (context.identifierOrIndexName() != null)
			{
				return new IntegerVariable(
					CreateIdentifierOrIndexName(context.identifierOrIndexName()));
			}
			else
			{
				return new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}
		}

		internal IntegerVariable CreateIntegerVariableOrIndex(CodeElementsParser.IntegerVariableOrIndex2Context context)
		{
			if (context.qualifiedDataNameOrIndexName() != null)
			{
				return new IntegerVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateQualifiedDataNameOrIndexName(context.qualifiedDataNameOrIndexName())));
			}
			else
			{
				return new IntegerVariable(
					CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
			}
		}

		internal NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable1Context context)
		{
			return new NumericVariable(
				CreateIdentifier(context.identifier()));
		}

		internal NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable2Context context)
		{
			return new NumericVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
		}

		internal NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable3Context context) {
			if(context.identifier() != null)
				return new NumericVariable(CreateIdentifier(context.identifier()));
			if (context.numericValue() != null)
				return new NumericVariable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			return null;
		}

		internal NumericVariable CreateNumericVariableOrIndex(CodeElementsParser.NumericVariableOrIndexContext context)
		{
			if (context.identifierOrIndexName() != null)
			{
				return new NumericVariable(
					CreateIdentifierOrIndexName(context.identifierOrIndexName()));
			}
			else
			{
				return new NumericVariable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
		}

		internal CharacterVariable CreateCharacterVariable(CodeElementsParser.CharacterVariableContext context)
		{
			if (context.dataNameReference() != null)
			{
				return new CharacterVariable(
					new DataOrConditionStorageArea(
						CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
			}
			else
			{
				return new CharacterVariable(
					CobolWordsBuilder.CreateCharacterValue(context.characterValue4()));
			}
		}

		internal AlphanumericVariable CreateAlphanumericVariable(CodeElementsParser.AlphanumericVariable1Context context)
		{
			if (context.identifier() != null)
			{
				return new AlphanumericVariable(
					CreateIdentifier(context.identifier()));
			}
			else
			{
				return new AlphanumericVariable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue3()));
			}
		}

		internal AlphanumericVariable CreateAlphanumericVariable(CodeElementsParser.AlphanumericVariable2Context context)
		{
			if (context.identifier() != null)
			{
				return new AlphanumericVariable(
					CreateIdentifier(context.identifier()));
			}
			else
			{
				if (context.alphanumericValue2() != null)
				{
					return new AlphanumericVariable(
						CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
				}
				else
				{
					return new AlphanumericVariable(
						CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
				}
			}
		}

		internal SymbolReferenceVariable CreateProgramNameVariable(CodeElementsParser.ProgramNameVariableContext context) {
			if (context.programNameReference1() != null) {
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReference(context.programNameReference1());
				return new SymbolReferenceVariable(StorageDataType.ProgramName, symbolReference);
			}
			if (context.identifier() != null) {
				StorageArea storageArea = CreateIdentifier(context.identifier());
				return new SymbolReferenceVariable(StorageDataType.ProgramName, storageArea);
			}
			return null;
		}

		internal SymbolReferenceVariable CreateProgramNameOrProgramEntryVariable(CodeElementsParser.ProgramNameOrProgramEntryVariableContext context)
		{
			if (context.programNameReferenceOrProgramEntryReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReferenceOrProgramEntryReference(context.programNameReferenceOrProgramEntryReference());
				return new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntry, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				return new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntry, storageArea);
			}
		}

		internal SymbolReferenceVariable CreateProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariable(CodeElementsParser.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariableContext context)
		{
			if (context.programNameReferenceOrProgramEntryReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateProgramNameReferenceOrProgramEntryReference(context.programNameReferenceOrProgramEntryReference());
				return new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				return new SymbolReferenceVariable(StorageDataType.ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer, storageArea);
			}
		}

		internal SymbolReferenceVariable CreateClassNameOrObjectReferenceVariable(CodeElementsParser.ClassNameOrObjectReferenceVariableContext context)
		{
			StorageArea storageArea = CreateIdentifierOrClassName(context.identifierOrClassName());
			return new SymbolReferenceVariable(StorageDataType.ClassNameOrObjectReference, storageArea);
		}

		internal SymbolReferenceVariable CreateMethodNameVariable(CodeElementsParser.MethodNameVariableContext context)
		{
			if (context.methodNameReference() != null)
			{
				SymbolReference symbolReference = CobolWordsBuilder.CreateMethodNameReference(context.methodNameReference());
				return new SymbolReferenceVariable(StorageDataType.MethodName, symbolReference);
			}
			else
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				return new SymbolReferenceVariable(StorageDataType.MethodName, storageArea);
			}
		}

		internal Variable CreateVariable(CodeElementsParser.Variable1Context context) {
			if (context == null) return null;
			StorageArea storageArea = CreateIdentifier(context.identifier());
			return new Variable(storageArea);
		}

		internal Variable CreateVariable(CodeElementsParser.Variable2Context context)
		{
			SymbolReference qualifiedDataName = CobolWordsBuilder.CreateQualifiedDataName(context.qualifiedDataName());
			StorageArea storageArea = new DataOrConditionStorageArea(qualifiedDataName);
			return new Variable(storageArea);
		}

		internal Variable CreateVariable(CodeElementsParser.Variable3Context context)
		{
			if (context.identifier() != null)
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				return new Variable(storageArea);
			}
			else if(context.numericValue() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else
			{
				return new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
		}

        [CanBeNull]
        internal Variable CreateVariable([NotNull] CodeElementsParser.Variable4Context context)
        {
            if (context.identifier() != null)
            {
                StorageArea storageArea = CreateIdentifier(context.identifier());
                return new Variable(storageArea);
            }
            else if (context.numericValue() != null)
            {
                return new Variable(
                    CobolWordsBuilder.CreateNumericValue(context.numericValue()));
            }
            else if (context.alphanumericValue3() != null)
            {
                return new Variable(
                    CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue3()));
            }
            else
            {
                return null;
            }
        }

		internal Variable CreateVariable(CodeElementsParser.Variable5Context context)
		{
			if (context.dataNameReference() != null)
			{
				SymbolReference dataNameReference = CobolWordsBuilder.CreateDataNameReference(context.dataNameReference());
				StorageArea storageArea = new DataOrConditionStorageArea(dataNameReference);
				return new Variable(storageArea);
			}
			else if (context.numericValue() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else
			{
				return new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue3()));
			}
		}

		internal Variable CreateVariable(CodeElementsParser.Variable6Context context)
		{
			if (context.identifier() != null)
			{
				StorageArea storageArea = CreateIdentifier(context.identifier());
				return new Variable(storageArea);
			}
			else if (context.numericValue() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if(context.alphanumericValue2() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else
			{
				return new Variable(
					CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
			}
		}

		internal Variable CreateVariable(CodeElementsParser.Variable7Context context) {
			if (context == null) return null;
			if (context.identifier() != null) {
				return new Variable(CreateIdentifier(context.identifier()));
			} else
			if (context.numericValue() != null) {
				return new Variable(CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			} else
			if (context.alphanumericValue2() != null) {
				return new Variable(CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			} else {
				return new Variable(CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue2()));
			}
		}

		internal Variable CreateVariableOrIndex(CodeElementsParser.VariableOrIndexContext context)
		{
			if(context.identifierOrIndexName() != null)
			{
				return new Variable(
					CreateIdentifierOrIndexName(context.identifierOrIndexName()));
			}
			else if (context.numericValue() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if (context.alphanumericValue2() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else
			{
				return new Variable(
					CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue2()));
			}
		}

		internal Variable CreateVariableOrFileName(CodeElementsParser.VariableOrFileNameContext context)
		{
			if (context.identifierOrFileName() != null)
			{
				return new Variable(
					CreateIdentifierOrFileName(context.identifierOrFileName()));
			}
			else if (context.numericValue() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if (context.alphanumericValue2() != null)
			{
				return new Variable(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else
			{
				return new Variable(
					CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
			}
		}

		internal VariableOrExpression CreateVariableOrExpression(CodeElementsParser.VariableOrExpression1Context context)
		{
			if (context.identifier() != null)
			{
				return new VariableOrExpression(
					CreateIdentifier(context.identifier()));
			}
			else if (context.numericValue() != null)
			{
				return new VariableOrExpression(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if (context.alphanumericValue2() != null)
			{
				return new VariableOrExpression(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else
			{
				return new VariableOrExpression(
					CreateArithmeticExpression(context.arithmeticExpression()));
			}
		}

		internal VariableOrExpression CreateVariableOrExpression(CodeElementsParser.VariableOrExpression2Context context)
		{
			if (context.identifier() != null)
			{
				return new VariableOrExpression(
					CreateIdentifier(context.identifier()));
			}
			else if (context.numericValue() != null)
			{
				return new VariableOrExpression(
					CobolWordsBuilder.CreateNumericValue(context.numericValue()));
			}
			else if (context.alphanumericValue2() != null)
			{
				return new VariableOrExpression(
					CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue2()));
			}
			else if (context.repeatedCharacterValue1() != null)
			{
				return new VariableOrExpression(
					CobolWordsBuilder.CreateRepeatedCharacterValue(context.repeatedCharacterValue1()));
			}
			else
			{
				return new VariableOrExpression(
					CreateArithmeticExpression(context.arithmeticExpression()));
			}
		}


		// --- Storage areas where statements results are saved ---

		internal ReceivingStorageArea CreateNumericStorageArea(CodeElementsParser.NumericStorageAreaContext context)
		{
			return new ReceivingStorageArea(StorageDataType.Numeric,
				CreateIdentifier(context.identifier()));
		}

		internal ReceivingStorageArea CreateAlphanumericStorageArea(CodeElementsParser.AlphanumericStorageAreaContext context)
		{
			return new ReceivingStorageArea(StorageDataType.Alphanumeric,
				CreateIdentifier(context.identifier()));
		}

		internal ReceivingStorageArea CreateIndexStorageArea(CodeElementsParser.IndexStorageAreaContext context)
		{
			StorageArea indexStorageArea = new IndexStorageArea(
				CobolWordsBuilder.CreateIndexNameReference(context.indexNameReference()));

			return new ReceivingStorageArea(StorageDataType.Numeric, indexStorageArea);
		}

		internal ReceivingStorageArea CreateDataOrIndexStorageArea(CodeElementsParser.DataOrIndexStorageAreaContext context)
		{
			return new ReceivingStorageArea(StorageDataType.Numeric,
				CreateIdentifierOrIndexName(context.identifierOrIndexName()));
		}

		internal ReceivingStorageArea CreateStorageArea(CodeElementsParser.StorageArea1Context context) {
			if (context == null || context.identifier() == null) return null;
			return new ReceivingStorageArea(StorageDataType.Any, CreateIdentifier(context.identifier()));
		}

		internal ReceivingStorageArea CreateStorageArea(CodeElementsParser.StorageArea2Context context)
		{
			return new ReceivingStorageArea(StorageDataType.Any,
				new DataOrConditionStorageArea(
					CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
		}
	}
}
