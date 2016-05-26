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


        // --- (Data storage area) Identifiers ---

        // - 1. Table elements reference : subscripting data names or condition names -

        internal static StorageArea CreateDataItemReference(CodeElementsParser.DataItemReferenceContext context)
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

        internal static StorageArea CreateConditionReference(CodeElementsParser.ConditionReferenceContext context)
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
                    IntegerVariable integerVariable = CreateIntegerVariable(context.integerVariableOrIndex2());
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
            throw new NotImplementedException();
        }

        internal static StorageArea CreateStorageAreaReference(CodeElementsParser.StorageAreaReferenceContext storageAreaReferenceContext)
        {
            throw new NotImplementedException();
        }


        internal static IntegerVariable CreateIntegerVariable(CodeElementsParser.IntegerVariableOrIndex2Context integerVariableOrIndex2Context)
        {
            throw new NotImplementedException();
        }

        // -- OLD CODE --
        /*
        internal static IList<Identifier> CreateIdentifiers(IReadOnlyList<CodeElementsParser.IdentifierContext> context)
        {
            IList<Identifier> identifiers = new List<Identifier>();
            if (context != null)
            {
                foreach (var identifier in context)
                {
                    var i = CreateIdentifier(identifier);
                    if (i != null) identifiers.Add(i);
                }
            }
            return identifiers;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReference(context.dataReferenceOrConditionReference());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
            var substring = identifier as ReferenceModifiable;
            if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrIndexNameContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReferenceOrIndexName(context.dataReferenceOrConditionReferenceOrIndexName());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
            var substring = identifier as ReferenceModifiable;
            if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrFileNameContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReferenceOrFileName(context.dataReferenceOrConditionReferenceOrFileName());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
            var substring = identifier as ReferenceModifiable;
            if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Identifier CreateIdentifier(CodeElementsParser.IdentifierOrClassNameContext context)
        {
            if (context == null) return null;
            Identifier identifier = CreateDataReferenceOrConditionReferenceOrClassName(context.dataReferenceOrConditionReferenceOrClassName());
            if (identifier != null) return identifier;
            identifier = CreateSpecialRegister(context.specialRegister());
            if (identifier != null) return identifier;
            identifier = CreateFunctionReference(context.functionIdentifier());
            if (identifier != null) return identifier;
            identifier = CreateLinageCounter(context.linageCounterSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateLengthOf(context.lengthOfSpecialRegisterDecl());
            if (identifier != null) return identifier;
            identifier = CreateAddressOf(context.addressOfSpecialRegisterDecl());
            var substring = identifier as ReferenceModifiable;
            if (substring != null) substring.ReferenceModifier = CreateReferenceModifier(context.referenceModifier());
            return identifier;
        }

        public static Token GetSymbolTokenIfIdentifierIsOneUserDefinedWord(CodeElementsParser.IdentifierOrClassNameContext identifier)
        {
            if (identifier.referenceModifier() == null)
            {
                var dataReferenceOrConditionReferenceOrClassName = identifier.dataReferenceOrConditionReferenceOrClassName();
                if (dataReferenceOrConditionReferenceOrClassName != null)
                {
                    if (dataReferenceOrConditionReferenceOrClassName.subscript() == null)
                    {
                        var qualifiedName = dataReferenceOrConditionReferenceOrClassName.qualifiedDataNameOrQualifiedConditionNameOrClassName();
                        var legacy = qualifiedName.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
                        if (legacy != null)
                        {
                            if (legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference() == null)
                                return ParseTreeUtils.GetFirstToken(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
                        }
                        else
                        {
                            if (qualifiedName.qDataOrFileOrUPSI() == null)
                                return ParseTreeUtils.GetFirstToken(qualifiedName.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
                        }
                    }
                }
            }
            return null;
        }

        private static Identifier CreateFunctionReference(CodeElementsParser.FunctionIdentifierContext context)
        {
            if (context == null || context.FUNCTION() == null) return null;
            var symbol = new FunctionName(ParseTreeUtils.GetFirstToken(context.intrinsicFunctionName()));
            var parameters = CreateFunctionParameters(context.argument());
            return new FunctionReference(symbol, parameters);
        }

        private static IList<FunctionParameter> CreateFunctionParameters(IReadOnlyList<CodeElementsParser.ArgumentContext> context)
        {
            if (context == null) return null;
            IList<FunctionParameter> parameters = new List<FunctionParameter>();
            foreach (var argument in context) parameters.Add(CreateFunctionParameter(argument));
            return parameters;
        }

        private static FunctionParameter CreateFunctionParameter(CodeElementsParser.ArgumentContext context)
        {
            if (context.literal() != null) return new FunctionParameter(CreateLiteral(context.literal()));
            //if (context.arithmeticExpression() != null) return new FunctionParameter(new ArithmeticExpressionBuilder().CreateArithmeticExpression(context.arithmeticExpression()));
            if (context.identifier() != null) return new FunctionParameter(CreateIdentifier(context.identifier()));
            return null;
        }

        private static Identifier CreateSpecialRegister(CodeElementsParser.SpecialRegisterContext context)
        {
            if (context == null) return null;
            return new SpecialRegister(new SpecialRegisterName(ParseTreeUtils.GetFirstToken(context)));
        }

        private static DataReference CreateDataReference(CodeElementsParser.DataReferenceContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = null;
            if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
            return new DataReference(name, subscripts);
        }

        private static DataReference CreateDataReferenceOrConditionReference(CodeElementsParser.DataReferenceOrConditionReferenceContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = null;
            if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
            return new DataReference(name, subscripts);
        }

        private static DataReference CreateDataReferenceOrConditionReferenceOrIndexName(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = null;
            if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
            return new DataReference(name, subscripts);
        }

        private static DataReference CreateDataReferenceOrConditionReferenceOrFileName(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = null;
            if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
            return new DataReference(name, subscripts);
        }

        private static DataReference CreateDataReferenceOrConditionReferenceOrClassName(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext context)
        {
            if (context == null) return null;
            QualifiedName name = CreateQualifiedName(context);
            IList<Subscript> subscripts = null;
            if (!(name is Subscripted)) subscripts = CreateSubscripts(context);
            return new DataReference(name, subscripts);
        }

        private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceContext context)
        {
            if (context == null) return null;
            return CreateQualifiedName(context.qualifiedDataName());
        }
        private static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context)
        {
            if (context == null) return null;
            var legacy = context.legacyQualifiedDataNameOrIndexName();
            if (legacy != null)
            {
                var dataname = CreateDataName(legacy.dataNameReferenceOrIndexNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFile())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateDataName(context.dataNameReferenceOrIndexNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }
        private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceContext ctxt)
        {
            if (ctxt == null) return null;
            var context = ctxt.qualifiedDataNameOrQualifiedConditionName();
            if (context == null) return null;
            var legacy = context.legacyQualifiedDataNameOrConditionName();
            if (legacy != null)
            {
                var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFileOrUPSI())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }
        private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext ctxt)
        {
            if (ctxt == null) return null;
            var context = ctxt.qualifiedDataNameOrQualifiedConditionNameOrIndexName();
            if (context == null) return null;
            var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName();
            if (legacy != null)
            {
                var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFileOrUPSI())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }
        private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext ctxt)
        {
            if (ctxt == null) return null;
            var context = ctxt.qualifiedDataNameOrQualifiedConditionNameOrFileName();
            if (context == null) return null;
            var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrFileName();
            if (legacy != null)
            {
                var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFileOrUPSI())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }
        private static QualifiedName CreateQualifiedName(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext ctxt)
        {
            if (ctxt == null) return null;
            var context = ctxt.qualifiedDataNameOrQualifiedConditionNameOrClassName();
            if (context == null) return null;
            var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
            if (legacy != null)
            {
                var dataname = CreateDataName(legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFileOrUPSI())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateDataName(context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }

        public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedDataNameContext context)
        {
            if (context == null) return null;
            var legacy = context.legacyQualifiedDataName();
            if (legacy != null)
            {
                var dataname = CreateDataName(legacy.dataNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFile())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateDataName(context.dataNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }
        public static QualifiedName CreateQualifiedName(CodeElementsParser.QualifiedConditionNameContext context)
        {
            if (context == null) return null;
            var legacy = context.legacyQualifiedConditionName();
            if (legacy != null)
            {
                var dataname = CreateConditionName(legacy.conditionNameReferenceOrConditionForUPSISwitchNameReference());
                var qualifiers = CreateDataNames(legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                return CreateQualifiedName(dataname, qualifiers, true, false);
            }
            else
            {
                var qname = new SubscriptedQualifiedName();
                Symbol symbol;
                foreach (var c in context.qDataOrFileOrUPSI())
                {
                    if (c == null) continue; //TODO? else qname.IsExplicit = false;
                    symbol = CreateDataName(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
                    AddToSubscriptedQualifiedName(qname, symbol, c.subscript());
                }
                symbol = CreateConditionName(context.conditionNameReferenceOrConditionForUPSISwitchNameReference());
                AddToSubscriptedQualifiedName(qname, symbol, context.subscript());
                return qname;
            }
        }
        private static void AddToSubscriptedQualifiedName(SubscriptedQualifiedName qname, Symbol symbol, CodeElementsParser.SubscriptContext context)
        {
            string name = symbol != null ? symbol.Name : null;
            var subscript = CreateSubscript(context);
            var pair = new Tuple<string, Subscript>(name, subscript);//TODO? test name|subscript == null
            if (pair != null) qname.Add(pair.Item1, pair.Item2);
            //TODO? else qname.IsExplicit = false;
        }
        private static QualifiedName CreateQualifiedName(Symbol name, List<DataName> qualifiers, bool reverse, bool isExplicit)
        {
            if (reverse) qualifiers.Reverse();
            // TODO: need to lookup symbol table to distinguish data name and file name
            FileName filename = null; // may be first element of qualifiers
            return new SyntacticQualifiedName(name, qualifiers, filename, isExplicit);
        }

        internal static IList<QualifiedName> CreateQualifiedNames(IReadOnlyList<CodeElementsParser.QualifiedDataNameContext> context)
        {
            var names = new List<QualifiedName>();
            foreach (var name in context)
            {
                var x = CreateQualifiedName(name);
                if (x != null) names.Add(x);
            }
            return names;
        }


        private static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.QDataOrFileContext> context)
        {
            var list = new List<CodeElementsParser.DataNameReferenceOrFileNameReferenceContext>();
            foreach (var c in context)
                if (c.dataNameReferenceOrFileNameReference() != null)
                    list.Add(c.dataNameReferenceOrFileNameReference());
            //TODO: subscripting
            return CreateDataNames(list);
        }

        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameReferenceContext> context)
        {
            List<DataName> datanames = new List<DataName>();
            if (context != null)
                foreach (var dataname in context)
                {
                    var name = CreateDataName(dataname);
                    if (name != null) datanames.Add(name);
                }
            return datanames;
        }

        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameReferenceOrFileNameReferenceContext> context)
        {
            List<DataName> datanames = new List<DataName>();
            if (context != null)
                foreach (var dataname in context)
                {
                    var name = CreateDataName(dataname);
                    if (name != null) datanames.Add(name);
                }
            return datanames;
        }

        private static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.QDataOrFileOrUPSIContext> context)
        {
            var list = new List<CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext>();
            foreach (var c in context)
                if (c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference() != null)
                    list.Add(c.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
            //TODO: subscripting
            return CreateDataNames(list);
        }

        public static List<DataName> CreateDataNames(IReadOnlyList<CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext> context)
        {
            List<DataName> datanames = new List<DataName>();
            if (context != null)
                foreach (var dataname in context)
                {
                    var name = CreateDataName(dataname);
                    if (name != null) datanames.Add(name);
                }
            return datanames;
        }

        internal static IList<FileName> CreateFileNames(IReadOnlyList<CodeElementsParser.FileNameReferenceContext> context)
        {
            List<FileName> filenames = new List<FileName>();
            if (context != null)
                foreach (var filename in context)
                {
                    var name = CreateFileName(filename);
                    if (name != null) filenames.Add(name);
                }
            filenames.Reverse();
            return filenames;
        }

        internal static IList<QualifiedProcedureName> CreateProcedureNames(IReadOnlyList<CodeElementsParser.ProcedureNameContext> context)
        {
            List<QualifiedProcedureName> procedurenames = new List<QualifiedProcedureName>();
            if (context != null)
                foreach (var procedurename in context)
                {
                    var name = CreateProcedureName(procedurename);
                    if (name != null) procedurenames.Add(name);
                }
            procedurenames.Reverse();
            return procedurenames;
        }

        internal static QualifiedProcedureName CreateProcedureName(CodeElementsParser.ProcedureNameContext context)
        {
            if (context.paragraphNameOrSectionNameReference() != null)
            {
                Token symbolToken = ParseTreeUtils.GetFirstToken(context.paragraphNameOrSectionNameReference());
                // TO DO : use here the symbol table to check if the name is a paragraph or a section
                // In the meantime, because sections are obsolete, we just assume it is always a paragraph name
                ParagraphName paragraphName = new ParagraphName(symbolToken);
                return new QualifiedProcedureName(paragraphName, null);
            }
            else //if(context.qualifiedParagraphNameReference() != null)
            {
                Token paragraphToken = ParseTreeUtils.GetFirstToken(context.qualifiedParagraphNameReference().paragraphNameReference());
                Token sectionToken = ParseTreeUtils.GetFirstToken(context.qualifiedParagraphNameReference().sectionNameReference());
                ParagraphName paragraphName = new ParagraphName(paragraphToken);
                SectionName sectionName = new SectionName(sectionToken);
                return new QualifiedProcedureName(paragraphName, sectionName);
            }
        }

        private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceContext context)
        {
            if (context == null) return null;
            //TODO: subscripting
            return CreateSubscripts(context.subscript());
        }

        private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceContext context)
        {
            if (context == null) return null;
            var subscripts = CreateSubscripts(context.subscript());
            //TODO: subscripting (probably not more correct than other CreateSubscripts() methods, but allows SEARCH test to pass)
            if (context.qualifiedDataNameOrQualifiedConditionName() != null)
            {
                var s = CreateSubscript(context.qualifiedDataNameOrQualifiedConditionName().subscript());
                if (s != null) subscripts.Add(s);
            }
            return subscripts;
        }

        private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceOrIndexNameContext context)
        {
            if (context == null) return null;
            //TODO: subscripting
            return CreateSubscripts(context.subscript());
        }

        private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceOrFileNameContext context)
        {
            if (context == null) return null;
            //TODO: subscripting
            return CreateSubscripts(context.subscript());
        }

        private static IList<Subscript> CreateSubscripts(CodeElementsParser.DataReferenceOrConditionReferenceOrClassNameContext context)
        {
            if (context == null) return null;
            //TODO: subscripting
            return CreateSubscripts(context.subscript());
        }

        public static IList<Subscript> CreateSubscripts(IReadOnlyList<CodeElementsParser.SubscriptContext> context)
        {
            if (context == null) return null;
            var subscripts = new List<Subscript>();
            foreach (var subscript in context) subscripts.Add(CreateSubscript(subscript));
            return subscripts;
        }


        public static Subscript CreateSubscript(CodeElementsParser.SubscriptContext context)
        {
            if (context == null) return null;

            Subscript subscript = new Subscript();
            if (context.IntegerLiteral() != null)
            {
                subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
            }
            if (context.ALL() != null)
            {
                var token = ParseTreeUtils.GetTokenFromTerminalNode(context.ALL());
                subscript.indexname = new IndexName(token);
            }
            if (context.qualifiedDataNameOrIndexName() != null)
            {
                subscript.dataname = CreateQualifiedName(context.qualifiedDataNameOrIndexName());
                InitializeSubscriptOperatorAndLiteral(subscript, context.withRelativeSubscripting());
            }
            //if (context.indexName() != null)
            //{
            //    subscript.indexname = CreateIndexName(context.indexName());
            //    InitializeSubscriptOperatorAndLiteral(subscript, context.withRelativeSubscripting());
            //}
            return subscript;
        }

        private static void InitializeSubscriptOperatorAndLiteral(Subscript subscript, CodeElementsParser.WithRelativeSubscriptingContext context)
        {
            if (context == null) return;
            if (context.PlusOperator() != null) subscript.op = '+';
            if (context.MinusOperator() != null) subscript.op = '-';
            if (context.IntegerLiteral() != null) subscript.offset = new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral()));
        }

        private static Substring CreateReferenceModifier(CodeElementsParser.ReferenceModifierContext context)
        {
            if (context == null) return null;
            var builder = new ArithmeticExpressionBuilder();
            ArithmeticExpression left = null, right = null;
            //if (context.leftMostCharacterPosition() != null)
            //    left = builder.CreateArithmeticExpression(context.leftMostCharacterPosition().arithmeticExpression());
            //if (context.length() != null)
            //    right = builder.CreateArithmeticExpression(context.length().arithmeticExpression());
            if (left == null && right == null) return null;
            return new Substring(left, right);
        }



        private static Address CreateAddressOf(CodeElementsParser.AddressOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataReference() == null) return null;
            return new Address(CreateDataReference(context.dataReference()));
        }

        private static Length CreateLengthOf(CodeElementsParser.LengthOfSpecialRegisterDeclContext context)
        {
            if (context == null || context.dataReference() == null) return null;
            return new Length(CreateDataReference(context.dataReference()));
        }



        public static Expression CreateAddressOfIdentifier(ITerminalNode address, CodeElementsParser.IdentifierContext identifier)
        {
            throw new NotImplementedException();
        }



        public static Expression CreateIdentifierOrLiteral(CodeElementsParser.IdentifierOrLiteralContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null) return CobolWordsBuilder.CreateIdentifier(context.identifier());
            if (context.literal() != null) return CobolWordsBuilder.CreateLiteral(context.literal());
            return null;
        }


        internal static Expression CreateEncoding(CodeElementsParser.CodepageContext context)
        {
            if (context == null) return null;
            if (context.identifier() != null)
                return CreateIdentifier(context.identifier());
            if (context.IntegerLiteral() != null)
                return new Literal(new SyntaxNumber(ParseTreeUtils.GetTokenFromTerminalNode(context.IntegerLiteral())));
            return null;
        }*/
    }
}
