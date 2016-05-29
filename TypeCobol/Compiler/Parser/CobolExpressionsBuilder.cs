using System;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Parser
{
    internal static class CobolExpressionsBuilder
    {
        // --- Qualified names : give explicit context to resolve ambiguous name references ---

        internal static SymbolReference CreateProcedureName(CodeElementsParser.ProcedureNameContext context)
        {
            if(context.paragraphNameReferenceOrSectionNameReference() != null)
            {
                return CobolWordsBuilder.CreateParagraphNameReferenceOrSectionNameReference(
                    context.paragraphNameReferenceOrSectionNameReference());
            }
            else
            {
                return CreateQualifiedParagraphNameReference(context.qualifiedParagraphNameReference());
            }
        }

        internal static SymbolReference CreateQualifiedParagraphNameReference(CodeElementsParser.QualifiedParagraphNameReferenceContext context)
        {
            return new QualifiedSymbolReference(
                    CobolWordsBuilder.CreateParagraphNameReference(context.paragraphNameReference()),
                    CobolWordsBuilder.CreateSectionNameReference(context.sectionNameReference()));
        }

        internal static SymbolReference CreateQualifiedDataName(CodeElementsParser.QualifiedDataNameContext context)
        {
            if(context.dataNameReference() != null)
            {
                return CobolWordsBuilder.CreateDataNameReference(context.dataNameReference());
            }
            else
            {
                return CreateQualifiedDataName1(context.qualifiedDataName1());
            }
        }

        internal static SymbolReference CreateQualifiedDataName1(CodeElementsParser.QualifiedDataName1Context context)
        {
            SymbolReference qualifiedDataName = new QualifiedSymbolReference(
                    CobolWordsBuilder.CreateDataNameReference(context.dataNameReference()),
                    CobolWordsBuilder.CreateDataNameReferenceOrFileNameReference(context.dataNameReferenceOrFileNameReference()[0]));

            for (int i = 1; i < context.dataNameReferenceOrFileNameReference().Length; i++)
            {
                qualifiedDataName = new QualifiedSymbolReference(
                    qualifiedDataName,
                    CobolWordsBuilder.CreateDataNameReferenceOrFileNameReference(context.dataNameReferenceOrFileNameReference()[i]));
            }
            return qualifiedDataName;
        }

        internal static SymbolReference CreateQualifiedDataNameOrIndexName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context)
        {
            if (context.dataNameReferenceOrIndexNameReference() != null)
            {
                return CobolWordsBuilder.CreateDataNameReferenceOrIndexNameReference(context.dataNameReferenceOrIndexNameReference());
            }
            else
            {
                return CreateQualifiedDataName1(context.qualifiedDataName1());
            }
        }

        internal static SymbolReference CreateRecordName(CodeElementsParser.RecordNameContext context)
        {
            // Could add here a specific property to mark the data name as a record name
            return CreateQualifiedDataName(context.qualifiedDataName());
        }

        internal static SymbolReference CreateQualifiedConditionName(CodeElementsParser.QualifiedConditionNameContext context)
        {
            if(context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference() == null ||
                context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference().Length == 0)
            {
                return CobolWordsBuilder.CreateConditionNameReferenceOrConditionForUPSISwitchNameReference(
                    context.conditionNameReferenceOrConditionForUPSISwitchNameReference());
            }
            else
            {
                SymbolReference qualifiedDataName = new QualifiedSymbolReference(
                    CobolWordsBuilder.CreateConditionNameReferenceOrConditionForUPSISwitchNameReference(
                        context.conditionNameReferenceOrConditionForUPSISwitchNameReference()),
                    CobolWordsBuilder.CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(
                        context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference()[0]));

                for (int i = 1; i < context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference().Length; i++)
                {
                    qualifiedDataName = new QualifiedSymbolReference(
                        qualifiedDataName,
                        CobolWordsBuilder.CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(
                            context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference()[i]));
                }
                return qualifiedDataName;
            }
        }

        internal static SymbolReference CreateQualifiedDataNameOrQualifiedConditionName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference() != null)
            {
                return CobolWordsBuilder.CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }

        internal static SymbolReference CreateQualifiedDataNameOrQualifiedConditionName1(CodeElementsParser.QualifiedDataNameOrQualifiedConditionName1Context context)
        {
            SymbolReference qualifiedDataNameOrConditionNameOrConditionForUPSISwitchName = new QualifiedSymbolReference(
                    CobolWordsBuilder.CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference()),
                    CobolWordsBuilder.CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference()[0]));

            for (int i = 1; i < context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference().Length; i++)
            {
                qualifiedDataNameOrConditionNameOrConditionForUPSISwitchName = new QualifiedSymbolReference(
                    qualifiedDataNameOrConditionNameOrConditionForUPSISwitchName,
                    CobolWordsBuilder.CreateDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(context.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference()[i]));
            }
            return qualifiedDataNameOrConditionNameOrConditionForUPSISwitchName;
        }

        internal static SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrIndexName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrIndexNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference() != null)
            {
                return CobolWordsBuilder.CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }

        internal static SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrFileName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrFileNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference() != null)
            {
                return CobolWordsBuilder.CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }

        internal static SymbolReference CreateQualifiedDataNameOrQualifiedConditionNameOrClassName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrClassNameContext context)
        {
            if (context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference() != null)
            {
                return CobolWordsBuilder.CreateDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
            }
            else
            {
                return CreateQualifiedDataNameOrQualifiedConditionName1(context.qualifiedDataNameOrQualifiedConditionName1());
            }
        }


        // --- (Data storage area) Identifiers ---

        // - 1. Table elements reference : subscripting data names or condition names -

        internal static DataOrConditionStorageArea CreateDataItemReference(CodeElementsParser.DataItemReferenceContext context)
        {
            SymbolReference qualifiedDataName = CreateQualifiedDataName(context.qualifiedDataName());
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

        internal static DataOrConditionStorageArea CreateConditionReference(CodeElementsParser.ConditionReferenceContext context)
        {
            SymbolReference qualifiedConditionName = CreateQualifiedConditionName(context.qualifiedConditionName());
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

        internal static DataOrConditionStorageArea CreateDataItemReferenceOrConditionReference(CodeElementsParser.DataItemReferenceOrConditionReferenceContext context)
        {
            SymbolReference qualifiedDataNameOrQualifiedConditionName = CreateQualifiedDataNameOrQualifiedConditionName(context.qualifiedDataNameOrQualifiedConditionName());
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

        internal static DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrIndexName(CodeElementsParser.DataItemReferenceOrConditionReferenceOrIndexNameContext context)
        {
            SymbolReference qualifiedDataNameOrQualifiedConditionNameOrIndexName = CreateQualifiedDataNameOrQualifiedConditionNameOrIndexName(context.qualifiedDataNameOrQualifiedConditionNameOrIndexName());
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

        internal static DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrFileName(CodeElementsParser.DataItemReferenceOrConditionReferenceOrFileNameContext context)
        {
            SymbolReference qualifiedDataNameOrQualifiedConditionNameOrFileName = CreateQualifiedDataNameOrQualifiedConditionNameOrFileName(context.qualifiedDataNameOrQualifiedConditionNameOrFileName());
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

        internal static DataOrConditionStorageArea CreateDataItemReferenceOrConditionReferenceOrClassName(CodeElementsParser.DataItemReferenceOrConditionReferenceOrClassNameContext context)
        {
            SymbolReference qualifiedDataNameOrQualifiedConditionNameOrClassName = CreateQualifiedDataNameOrQualifiedConditionNameOrClassName(context.qualifiedDataNameOrQualifiedConditionNameOrClassName());
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

        internal static SubscriptExpression[] CreateSubscriptExpressions(CodeElementsParser.SubscriptContext[] contextArray)
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

        internal static StorageArea CreateLinageCounterSpecialRegister(CodeElementsParser.LinageCounterSpecialRegisterContext context)
        {
            return new FilePropertySpecialRegister(
                ParseTreeUtils.GetFirstToken(context.LINAGE_COUNTER()),
                CobolWordsBuilder.CreateFileNameReference(context.fileNameReference()));
        }

        internal static StorageArea CreateAddressOfSpecialRegister(CodeElementsParser.AddressOfSpecialRegisterContext context)
        {
            return new StorageAreaPropertySpecialRegister(
                ParseTreeUtils.GetFirstToken(context.ADDRESS()),
                CreateStorageAreaReference(context.storageAreaReference()));
        }

        internal static StorageArea CreateLengthOfSpecialRegister(CodeElementsParser.LengthOfSpecialRegisterContext context)
        {
            return new StorageAreaPropertySpecialRegister(
                ParseTreeUtils.GetFirstToken(context.LENGTH()),
                CreateStorageAreaReference(context.storageAreaReference()));
        }

        // - 3. Intrinsic function calls (allocate a storage area for the result) -

        internal static StorageArea CreateFunctionIdentifier(CodeElementsParser.FunctionIdentifierContext context)
        {
            return new IntrinsicFunctionCallResultStorageArea(
                CobolWordsBuilder.CreateIntrinsicFunctionName(context.intrinsicFunctionName()),
                CreateArguments(context.argument()));
        }

        private static Expression[] CreateArguments(CodeElementsParser.ArgumentContext[] argumentContext)
        {
            Expression[] arguments = new Expression[argumentContext.Length];
            for(int i = 0; i < argumentContext.Length; i++)
            {
                arguments[i] = CreateVariableOrExpression(argumentContext[i].variableOrExpression1());
            }
            return arguments;
        }

        internal static StorageArea CreateStorageAreaReference(CodeElementsParser.StorageAreaReferenceContext context)
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

        internal static StorageArea CreateOtherStorageAreaReference(CodeElementsParser.OtherStorageAreaReferenceContext context)
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

        internal static StorageArea CreateStorageAreaReferenceOrConditionReference(CodeElementsParser.StorageAreaReferenceOrConditionReferenceContext context)
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

        internal static StorageArea CreateStorageAreaReferenceOrConditionReferenceOrIndexName(CodeElementsParser.StorageAreaReferenceOrConditionReferenceOrIndexNameContext context)
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

        internal static StorageArea CreateStorageAreaReferenceOrConditionReferenceOrFileName(CodeElementsParser.StorageAreaReferenceOrConditionReferenceOrFileNameContext context)
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

        internal static StorageArea CreateStorageAreaReferenceOrConditionReferenceOrClassName(CodeElementsParser.StorageAreaReferenceOrConditionReferenceOrClassNameContext context)
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

        internal static StorageArea CreateIdentifier(CodeElementsParser.IdentifierContext context)
        {
            StorageArea storageArea = CreateStorageAreaReferenceOrConditionReference(context.storageAreaReferenceOrConditionReference());
            if(context.referenceModifier() != null)
            {
                storageArea.ApplyReferenceModifier(
                    CreateReferenceModifier(context.referenceModifier()));
            }
            return storageArea;
        }

        private static ReferenceModifier CreateReferenceModifier(CodeElementsParser.ReferenceModifierContext context)
        {
            ArithmeticExpression leftmostCharacterPosition = CreateArithmeticExpression(context.leftMostCharacterPosition);
            ArithmeticExpression length = null;
            if(context.length != null)
            {
                length = CreateArithmeticExpression(context.length);
            }
            return new ReferenceModifier(leftmostCharacterPosition, length);
        }

        internal static StorageArea CreateIdentifierOrIndexName(CodeElementsParser.IdentifierOrIndexNameContext context)
        {
            StorageArea storageArea = CreateStorageAreaReferenceOrConditionReferenceOrIndexName(context.storageAreaReferenceOrConditionReferenceOrIndexName());
            if (context.referenceModifier() != null)
            {
                storageArea.ApplyReferenceModifier(
                    CreateReferenceModifier(context.referenceModifier()));
            }
            return storageArea;
        }

        internal static StorageArea CreateIdentifierOrFileName(CodeElementsParser.IdentifierOrFileNameContext context)
        {
            StorageArea storageArea = CreateStorageAreaReferenceOrConditionReferenceOrFileName(context.storageAreaReferenceOrConditionReferenceOrFileName());
            if (context.referenceModifier() != null)
            {
                storageArea.ApplyReferenceModifier(
                    CreateReferenceModifier(context.referenceModifier()));
            }
            return storageArea;
        }

        internal static StorageArea CreateIdentifierOrClassName(CodeElementsParser.IdentifierOrClassNameContext context)
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

        internal static ArithmeticExpression CreateArithmeticExpression(CodeElementsParser.ArithmeticExpressionContext context)
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

        internal static ConditionalExpression CreateConditionalExpression(CodeElementsParser.ConditionalExpressionContext context)
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

        internal static ConditionalExpression CreateClassCondition(CodeElementsParser.ClassConditionContext context)
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

        internal static ConditionalExpression CreateConditionNameConditionOrSwitchStatusCondition(CodeElementsParser.ConditionNameConditionOrSwitchStatusConditionContext context)
        {
            return new ConditionNameConditionOrSwitchStatusCondition(
                CreateConditionReference(context.conditionReference()));
        }

        internal static ConditionalExpression CreateRelationCondition(CodeElementsParser.RelationConditionContext context)
        {
        }

        internal static ConditionalExpression CreateSignCondition(CodeElementsParser.SignConditionContext context)
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

        internal static ConditionOperand CreateConditionOperand(CodeElementsParser.ConditionOperandContext context)
        {
            ConditionOperand conditionOperand = null;
            if(context.arithmeticExpression() != null)
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


        // --- Cobol variables : runtime value or literal ---

        internal static BooleanValueOrExpression CreateBooleanValueOrExpression(CodeElementsParser.BooleanValueOrExpressionContext context)
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

        internal static IntegerVariable CreateIntegerVariable(CodeElementsParser.IntegerVariable1Context context)
        {
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

        internal static IntegerVariable CreateIntegerVariable(CodeElementsParser.IntegerVariable2Context context)
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

        internal static IntegerVariable CreateIntegerVariableOrIndex(CodeElementsParser.IntegerVariableOrIndex1Context context)
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

        internal static IntegerVariable CreateIntegerVariableOrIndex(CodeElementsParser.IntegerVariableOrIndex2Context context)
        {
            if (context.qualifiedDataNameOrIndexName() != null)
            {
                return new IntegerVariable(
                    new DataOrConditionStorageArea(
                        CreateQualifiedDataNameOrIndexName(context.qualifiedDataNameOrIndexName())));
            }
            else
            {
                return new IntegerVariable(
                    CobolWordsBuilder.CreateIntegerValue(context.integerValue()));
            }
        }

        internal static NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable1Context context)
        {
            return new NumericVariable(
                CreateIdentifier(context.identifier()));
        }

        internal static NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable2Context context)
        {
            return new NumericVariable(
                    new DataOrConditionStorageArea(
                        CobolWordsBuilder.CreateDataNameReference(context.dataNameReference())));
        }

        internal static NumericVariable CreateNumericVariable(CodeElementsParser.NumericVariable3Context context)
        {
            if(context.identifier() != null)
            {
                return new NumericVariable(
                    CreateIdentifier(context.identifier()));
            }
            else
            {
                return new NumericVariable(
                    CobolWordsBuilder.CreateNumericValue(context.numericValue()));
            }
        }

        internal static NumericVariable CreateNumericVariableOrIndex(CodeElementsParser.NumericVariableOrIndexContext context)
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

        internal static CharacterVariable CreateCharacterVariable(CodeElementsParser.CharacterVariableContext context)
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

        internal static AlphanumericVariable CreateAlphanumericVariable(CodeElementsParser.AlphanumericVariable1Context context)
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

        internal static AlphanumericVariable CreateAlphanumericVariable(CodeElementsParser.AlphanumericVariable2Context context)
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

        internal static ProgramNameVariable CreateProgramNameVariable(CodeElementsParser.ProgramNameVariableContext context)
        {
        }

        internal static Expression CreateVariableOrExpression(CodeElementsParser.VariableOrExpression1Context context)
        {
        }

        internal static Variable CreateVariableOrIndex(CodeElementsParser.VariableOrIndexContext context)
        {
        }
    }
}
