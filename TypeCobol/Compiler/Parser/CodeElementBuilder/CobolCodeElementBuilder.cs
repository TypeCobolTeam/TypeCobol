using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Diagnostics;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.Sql.CodeElements;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Parser
{
	/// <summary>Builds a CodeElement object while visiting its parse tree.</summary>
	internal partial class CodeElementBuilder: CodeElementsBaseListener {

        private bool IsDebuggingModeEnabled { get; set; }
        private ParserRuleContext Context;
        /// <summary>CodeElement object resulting of the visit the parse tree</summary>
		public CodeElement CodeElement { get; set; }
		private readonly CobolWordsBuilder _cobolWordsBuilder;
		private readonly CobolExpressionsBuilder _cobolExpressionsBuilder;
		private readonly CobolStatementsBuilder _cobolStatementsBuilder;
		private readonly SqlCodeElementBuilder _sqlCodeElementBuilder;
		private readonly UnsupportedLanguageLevelFeaturesChecker _languageLevelChecker;

		public CodeElementBuilder(TypeCobolOptions compilerOptions)
		{
			var targetLevel = compilerOptions.IsCobolLanguage ? CobolLanguageLevel.Cobol85 : CobolLanguageLevel.TypeCobol;
			_languageLevelChecker = new UnsupportedLanguageLevelFeaturesChecker(targetLevel);
            _cobolWordsBuilder = new CobolWordsBuilder();
            _cobolExpressionsBuilder = new CobolExpressionsBuilder(_cobolWordsBuilder, _languageLevelChecker);
			_cobolStatementsBuilder = new CobolStatementsBuilder(_cobolWordsBuilder, _cobolExpressionsBuilder, _languageLevelChecker);
			_sqlCodeElementBuilder = new SqlCodeElementBuilder();
		}

        /// <summary>Initialization code run before parsing each new COBOL CodeElement</summary>
        public override void EnterCodeElement(CodeElementsParser.CodeElementContext context) {
			CodeElement = null;
			Context = null;
			_cobolWordsBuilder.Reset();
			_cobolExpressionsBuilder.Reset();
		}

		/// <summary>Code run after parsing each new CodeElement</summary>
		public override void ExitCodeElement(CodeElementsParser.CodeElementContext context) {
			if(CodeElement != null)
            {
                if (_cobolWordsBuilder.symbolInformationForTokens.Keys.Count > 0) {
                    CodeElement.SymbolInformationForTokens = _cobolWordsBuilder.symbolInformationForTokens;
                }
                if (_cobolExpressionsBuilder.storageAreaDefinitions.Count > 0) {
                    CodeElement.StorageAreaDefinitions = _cobolExpressionsBuilder.storageAreaDefinitions;
                }
                if (_cobolExpressionsBuilder.storageAreaReads.Count > 0) {
                    CodeElement.StorageAreaReads = _cobolExpressionsBuilder.storageAreaReads;
                }
                if (_cobolExpressionsBuilder.storageAreaWrites.Count > 0) {
                    CodeElement.StorageAreaWrites = _cobolExpressionsBuilder.storageAreaWrites;
                }
                if (_cobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact != null) {
                    CodeElement.StorageAreaGroupsCorrespondingImpact = _cobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact;
                }
                if (_cobolExpressionsBuilder.callTarget != null) {
                    CodeElement.CallTarget = _cobolExpressionsBuilder.callTarget;
                }
                if (_cobolExpressionsBuilder.callSites.Count > 0) {
                    CodeElement.CallSites = _cobolExpressionsBuilder.callSites;
                }
                // Attach all tokens consumed by the parser for this code element
                // Collect all error messages encoutered while parsing this code element
                List<Diagnostic> diagnostics = CodeElement.Diagnostics ?? new List<Diagnostic>();
                AddTokensConsumedAndDiagnosticsAttachedInContext(CodeElement.ConsumedTokens, diagnostics, Context);
                if (diagnostics.Count > 0)
                {
                    CodeElement.Diagnostics = diagnostics;
                }
                CodeElementChecker.OnCodeElement(CodeElement, IsDebuggingModeEnabled);
            }
            // If the errors can't be attached to a CodeElement object, attach it to the parent codeElements rule context
            else if (CodeElement == null && context.Diagnostics != null)
            {
                var parentRuleContext = (ParserRuleContextWithDiagnostics)context.Parent;
                foreach (var ruleDiagnostic in context.Diagnostics)
                {
                    parentRuleContext.AttachDiagnostic(ruleDiagnostic);
                }
            }
		}

        private void AddTokensConsumedAndDiagnosticsAttachedInContext(IList<Token> consumedTokens, List<Diagnostic> diagnostics, ParserRuleContext context)
        {
            var ruleNodeWithDiagnostics = (ParserRuleContextWithDiagnostics)context;
            if (ruleNodeWithDiagnostics != null && ruleNodeWithDiagnostics.Diagnostics != null)
            {
				diagnostics.AddRange(ruleNodeWithDiagnostics.Diagnostics);
            }
            if (context.children != null)
            {
                foreach(var childNode in context.children)
                {
                    if (childNode is IRuleNode)
                    {                        
                        AddTokensConsumedAndDiagnosticsAttachedInContext(consumedTokens, diagnostics, (ParserRuleContext)((IRuleNode)childNode).RuleContext);
                    }
                    else if(childNode is ITerminalNode)
                    {
                        Token token = (Token)((ITerminalNode)childNode).Symbol;
                        consumedTokens.Add(token);
                    }
                }
            }
        }

        // Code structure

        ////////////////////
        // IDENTIFICATION //
        ////////////////////



        // PROGRAM IDENTIFICATION
        ////////////////////////////

        public override void EnterProgramIdentification(CodeElementsParser.ProgramIdentificationContext context) {
            var program = new ProgramIdentification();
            program.ProgramName = _cobolWordsBuilder.CreateProgramNameDefinition(context.programNameDefinition());

            if (context.COMMON() != null) {
                program.Common = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.COMMON()));
            }
            if (context.INITIAL() != null) {
                program.Initial = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.INITIAL()));
            }
            if (context.RECURSIVE() != null) {
                program.Recursive = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.RECURSIVE()));
            }
            program.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

            Context = context;
            CodeElement = program;
            IsDebuggingModeEnabled = false;

            if (context.pgmIdPeriodSeparator == null)
            {
                //a dot doesn't follow PROGRAM-ID
                if (CodeElement.Diagnostics == null) CodeElement.Diagnostics = new List<Diagnostic>();
                CodeElement.Diagnostics.Add(new ParserDiagnostic("Dot expected after PROGRAM-ID", context.PROGRAM_ID().Symbol, null, MessageCode.Warning));
            }
            if (context.pgmIdDeclarPeriodSeparator == null)
            {
                //a dot doesn't follow PROGRAM-ID declaration
                if (CodeElement.Diagnostics == null) CodeElement.Diagnostics = new List<Diagnostic>();
                IToken previousToken = context.Stop;
                if (context.authoringProperties().ChildCount > 0)
                {
                    //authoring properties are in last position: search previous token of authoring properties
                    previousToken = ParseTreeUtils.GetFirstToken(context.children.Reverse().Skip(1).First());
                }

                CodeElement.Diagnostics.Add(new ParserDiagnostic("Dot expected at the end of PROGRAM-ID declaration", previousToken, null, MessageCode.Warning));
            }
        }

        public override void EnterProgramEnd(CodeElementsParser.ProgramEndContext context) {
			var programEnd = new ProgramEnd();
			programEnd.ProgramName = _cobolWordsBuilder.CreateProgramNameReference(context.programNameReference2());

			Context = context;
			CodeElement = programEnd;
		}



		 // CLASS IDENTIFICATION
		//////////////////////////

		public override void EnterClassIdentification(CodeElementsParser.ClassIdentificationContext context) {
			var classIdentification = new ClassIdentification();
			classIdentification.ClassName = _cobolWordsBuilder.CreateClassNameDefinition(context.classNameDefinition());
			classIdentification.InheritsFrom = _cobolWordsBuilder.CreateClassNameReference(context.inheritsFromClassName);
			classIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = classIdentification;
		}

		public override void EnterClassEnd(CodeElementsParser.ClassEndContext context) {
			var classEnd = new ClassEnd();
			classEnd.ClassName = _cobolWordsBuilder.CreateClassNameReference(context.classNameReference());

			Context = context;
			CodeElement = classEnd;
		}



		 // FACTORY IDENTIFICATION
		////////////////////////////

		public override void EnterFactoryIdentification(CodeElementsParser.FactoryIdentificationContext context) {
			Context = context;
			CodeElement = new FactoryIdentification();
		}

		public override void EnterFactoryEnd(CodeElementsParser.FactoryEndContext context) {
			Context = context;
			CodeElement = new FactoryEnd();
		}

		 // OBJECT IDENTIFICATION
		///////////////////////////

		public override void EnterObjectIdentification(CodeElementsParser.ObjectIdentificationContext context) {
			Context = context;
			CodeElement = new ObjectIdentification();
		}

		public override void EnterObjectEnd(CodeElementsParser.ObjectEndContext context) {
			Context = context;
			CodeElement = new ObjectEnd();
		}

		 // METHOD IDENTIFICATION
		///////////////////////////

		public override void EnterMethodIdentification(CodeElementsParser.MethodIdentificationContext context) {
			var methodIdentification = new MethodIdentification();
			methodIdentification.MethodName = _cobolWordsBuilder.CreateMethodNameDefinition(context.methodNameDefinition());
			methodIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = methodIdentification;
		}

		public override void EnterMethodEnd(CodeElementsParser.MethodEndContext context) {
			var methodEnd = new MethodEnd();
			methodEnd.MethodName = _cobolWordsBuilder.CreateMethodNameReference(context.methodNameReference());

			Context = context;
			CodeElement = methodEnd;
		}

		// --- Authoring properties common to all identification divisions ---

		internal AuthoringProperties CreateAuthoringProperties(CodeElementsParser.AuthoringPropertiesContext context) {
			var authoringProperties = new AuthoringProperties();
            if (context == null) return authoringProperties;
			if (context.authorParagraph().Length > 0) {
				var alphanumericValueContexts = context.authorParagraph().SelectMany(p => p.CommentEntry()).ToArray();
				authoringProperties.Author = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.dateCompiledParagraph().Length > 0) {
				var alphanumericValueContexts = context.dateCompiledParagraph().SelectMany(p => p.CommentEntry()).ToArray();
				authoringProperties.DateCompiled = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.dateWrittenParagraph().Length > 0) {
				var alphanumericValueContexts = context.dateWrittenParagraph().SelectMany(p => p.CommentEntry()).ToArray();
				authoringProperties.DateWritten = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.installationParagraph().Length > 0) {
				var alphanumericValueContexts = context.installationParagraph().SelectMany(p => p.CommentEntry()).ToArray();
				authoringProperties.Installation = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.securityParagraph().Length > 0) {
				var alphanumericValueContexts = context.securityParagraph().SelectMany(p => p.CommentEntry()).ToArray();
				authoringProperties.Security = CreateAlphanumericValues(alphanumericValueContexts);
			}
			return authoringProperties;
		}

		private AlphanumericValue[] CreateAlphanumericValues(ITerminalNode[] contexts) {
			AlphanumericValue[] alphanumericValues = new AlphanumericValue[contexts.Length];
			for (int i = 0; i < contexts.Length; i++) {
				alphanumericValues[i] = _cobolWordsBuilder.CreateAlphanumericValue(contexts[i]);
			}
			return alphanumericValues;
		}



		  //////////////////////////
		 // ENVIRONMENT DIVISION //
		//////////////////////////

		public override void EnterEnvironmentDivisionHeader(CodeElementsParser.EnvironmentDivisionHeaderContext context) {
			Context = context;
			CodeElement = new EnvironmentDivisionHeader();
		}



		 // CONFIGURATION SECTION
		///////////////////////////

		public override void EnterConfigurationSectionHeader(CodeElementsParser.ConfigurationSectionHeaderContext context) {
			Context = context;
			CodeElement = new ConfigurationSectionHeader();
		}

		// --- SOURCE-COMPUTER PARAGRAPH ---

		public override void EnterSourceComputerParagraph(CodeElementsParser.SourceComputerParagraphContext context)
		{
			var paragraph = new SourceComputerParagraph();
			if (context.computerName != null)
			{
				System.Diagnostics.Debug.Assert(context.computerName is Token);
				//TokenType is UserDefinedWord, so it's ok to create an AlphanumericValue
				paragraph.ComputerName = new AlphanumericValue((Token) context.computerName);
			}

			if (context.DEBUGGING() != null)
			{
				var debuggingToken = ParseTreeUtils.GetFirstToken(context.DEBUGGING());
				paragraph.DebuggingMode = new SyntaxProperty<bool>(true, debuggingToken);
				IsDebuggingModeEnabled = true;
				DiagnosticUtils.AddError(paragraph, "Debugging mode is active", debuggingToken, null, MessageCode.Warning);
			}

			Context = context;
			CodeElement = paragraph;
		}

		// --- OBJECT-COMPUTER PARAGRAPH ---

		public override void EnterObjectComputerParagraph(CodeElementsParser.ObjectComputerParagraphContext context) {
			var paragraph = new ObjectComputerParagraph();
			if(context.computerName != null) {
				System.Diagnostics.Debug.Assert(context.computerName is Token);
				//TokenType is UserDefinedWord, so it's ok to create an AlphanumericValue
				paragraph.ComputerName = new AlphanumericValue((Token) context.computerName);
			}
			if(context.memorySizeClause() != null) {
				var memorySizeClauseContext = context.memorySizeClause();
				paragraph.MemorySize = CobolWordsBuilder.CreateIntegerValue(memorySizeClauseContext.IntegerLiteral());
				if(memorySizeClauseContext.WORDS() != null) {
					paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Words, ParseTreeUtils.GetFirstToken(memorySizeClauseContext.WORDS()));
				} else
				if (memorySizeClauseContext.CHARACTERS() != null) {
					paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Characters, ParseTreeUtils.GetFirstToken(memorySizeClauseContext.CHARACTERS()));
				} else
				if (memorySizeClauseContext.MODULES() != null) {
					paragraph.MemorySizeUnit = new SyntaxProperty<MemorySizeUnit>(MemorySizeUnit.Modules, ParseTreeUtils.GetFirstToken(memorySizeClauseContext.MODULES()));
				}
			}
			if(context.programCollatingSequenceClause() != null) {
				var collatingSeqClauseContext = context.programCollatingSequenceClause();
				paragraph.CollatingSequence = _cobolWordsBuilder.CreateAlphabetName(collatingSeqClauseContext.alphabetName());
			}
			if(context.segmentLimitClause() != null) {
				var segmentLimitClauseContext = context.segmentLimitClause();
				paragraph.SegmentLimit = CobolWordsBuilder.CreateIntegerValue(segmentLimitClauseContext.priorityNumber().IntegerLiteral());
			}

			Context = context;
			CodeElement = paragraph;
		}

		// --- SPECIAL-NAMES PARAGRAPH ---

		public override void EnterSpecialNamesParagraph(CodeElementsParser.SpecialNamesParagraphContext context)
		{
			var paragraph = new SpecialNamesParagraph();
			var environments = new HashSet<ExternalName>();
			var duplicateEnvironments = new List<RuleContext>();
			var duplicateMnemonicsForEnvironment = new List<RuleContext>();
			
			if(context.upsiSwitchNameClause() != null && context.upsiSwitchNameClause().Length > 0)
			{
				foreach (var upsiSwitchNameContext in context.upsiSwitchNameClause())
				{
					var upsiSwitchName = _cobolWordsBuilder.CreateUPSISwitchName(upsiSwitchNameContext.upsiSwitchName());
					if (upsiSwitchNameContext.mnemonicForUPSISwitchNameDefinition() != null)
					{
						var mnemonicForUPSISwitchName = _cobolWordsBuilder.CreateMnemonicForUPSISwitchNameDefinition(
							upsiSwitchNameContext.mnemonicForUPSISwitchNameDefinition());
						if(paragraph.MnemonicsForUPSISwitchNames == null)
						{
							paragraph.MnemonicsForUPSISwitchNames = new Dictionary<SymbolDefinition, ExternalName>();
						}
						paragraph.MnemonicsForUPSISwitchNames.Add(mnemonicForUPSISwitchName, upsiSwitchName);
					}
					if(upsiSwitchNameContext.conditionNamesForUPSISwitch() != null)
					{
						if (paragraph.ConditionNamesForUPSISwitchStatus == null)
						{
							paragraph.ConditionNamesForUPSISwitchStatus = new Dictionary<SymbolDefinition, Tuple<ExternalName, UPSISwitchStatus>>();
						}
						if (upsiSwitchNameContext.conditionNamesForUPSISwitch().offConditionNameForUPSISwitch() != null)
						{
							var conditionForUPSISwitchName = _cobolWordsBuilder.CreateConditionForUPSISwitchNameDefinition(
								upsiSwitchNameContext.conditionNamesForUPSISwitch().offConditionNameForUPSISwitch().conditionForUPSISwitchNameDefinition());
							paragraph.ConditionNamesForUPSISwitchStatus.Add(conditionForUPSISwitchName,
								new Tuple<ExternalName, UPSISwitchStatus>(upsiSwitchName, UPSISwitchStatus.Off));
						}
						if (upsiSwitchNameContext.conditionNamesForUPSISwitch().onConditionNameForUPSISwitch() != null)
						{
							var conditionForUPSISwitchName = _cobolWordsBuilder.CreateConditionForUPSISwitchNameDefinition(
								upsiSwitchNameContext.conditionNamesForUPSISwitch().onConditionNameForUPSISwitch().conditionForUPSISwitchNameDefinition());
							paragraph.ConditionNamesForUPSISwitchStatus.Add(conditionForUPSISwitchName,
								new Tuple<ExternalName, UPSISwitchStatus>(upsiSwitchName, UPSISwitchStatus.On));
						}
					}
				}
				
			}
			if (context.environmentNameClause() != null && context.environmentNameClause().Length > 0)
			{
				if(paragraph.MnemonicsForEnvironmentNames == null)
				{
					paragraph.MnemonicsForEnvironmentNames = new Dictionary<SymbolDefinition, ExternalName>();
				}
				foreach(var environmentNameContext in context.environmentNameClause())
				{
					var environmentName = _cobolWordsBuilder.CreateEnvironmentName(environmentNameContext.environmentName());
					if (!environments.Add(environmentName))
					{
						// Duplicate environment, add to duplicate list and skip definition
						duplicateEnvironments.Add(environmentNameContext.environmentName());
						continue;
					}
					
					var mnemonicForEnvironmentName = _cobolWordsBuilder.CreateMnemonicForEnvironmentNameDefinition(environmentNameContext.mnemonicForEnvironmentNameDefinition());
					if (paragraph.MnemonicsForEnvironmentNames.ContainsKey(mnemonicForEnvironmentName))
					{
						// Duplicate mnemonic, add to duplicate list and skip definition
						duplicateMnemonicsForEnvironment.Add(environmentNameContext.mnemonicForEnvironmentNameDefinition());
						continue;
					}
					
					paragraph.MnemonicsForEnvironmentNames.Add(mnemonicForEnvironmentName, environmentName);
				}
			}
			if (context.alphabetClause() != null && context.alphabetClause().Length > 0)
			{
				if(paragraph.AlphabetNames == null)
				{
					paragraph.AlphabetNames = new Dictionary<SymbolDefinition, CollatingSequence>();
				}
				foreach(var alphabetContext in context.alphabetClause())
				{
					var alphabetName = _cobolWordsBuilder.CreateAlphabetNameDefinition(alphabetContext.alphabetNameDefinition());
					if(alphabetContext.intrinsicAlphabetNameReference() != null)
					{
						var intrinsicCollatingSequence = new InstrinsicCollatingSequence();
						intrinsicCollatingSequence.IntrinsicAlphabetName = _cobolWordsBuilder.CreateIntrinsicAlphabetNameReference(
							alphabetContext.intrinsicAlphabetNameReference());
						paragraph.AlphabetNames.Add(alphabetName, intrinsicCollatingSequence);
					}
					else if(alphabetContext.userDefinedCollatingSequence() != null && alphabetContext.userDefinedCollatingSequence().Length > 0)
					{
						var userDefinedCollatingSequence = new UserDefinedCollatingSequence();
						userDefinedCollatingSequence.CharacterSets = new CharacterSetInCollatingSequence[alphabetContext.userDefinedCollatingSequence().Length];
						for (int i = 0; i < alphabetContext.userDefinedCollatingSequence().Length; i++)
						{
							var userDefinedCSContext = alphabetContext.userDefinedCollatingSequence()[i];
							if (userDefinedCSContext.characterInCollatingSequence() != null)
							{
								var charInCSContext = userDefinedCSContext.characterInCollatingSequence();
								var characters = CreateCharacterInCollatingSequence(charInCSContext);
								userDefinedCollatingSequence.CharacterSets[i] = characters;
							}
							else if (userDefinedCSContext.charactersRange() != null)
							{
								var charactersRangeContext = userDefinedCSContext.charactersRange();
								CharactersRangeInCollatingSequence charactersRange = CreateCharactersRange(charactersRangeContext);
								userDefinedCollatingSequence.CharacterSets[i] = charactersRange;
							}
							else if (userDefinedCSContext.charactersEqualSet() != null)
							{
								var charactersEqualSetContext = userDefinedCSContext.charactersEqualSet();
								var charactersEqualSet = new CharactersEqualSetInCollatingSequence();
								charactersEqualSet.EqualCharacters = new CharacterInCollatingSequence[charactersEqualSetContext.characterInCollatingSequence().Length];
								for (int j = 0; j < charactersEqualSetContext.characterInCollatingSequence().Length; j++)
								{
									var characterInCSContext = charactersEqualSetContext.characterInCollatingSequence()[j];
									charactersEqualSet.EqualCharacters[j] = CreateCharacterInCollatingSequence(characterInCSContext);
								}
								userDefinedCollatingSequence.CharacterSets[i] = charactersEqualSet;
							}
						}
						paragraph.AlphabetNames.Add(alphabetName, userDefinedCollatingSequence);
					}
				}
			}
			if (context.symbolicCharactersClause() != null && context.symbolicCharactersClause().Length > 0)
			{
				if(paragraph.SymbolicCharacters == null)
				{
					paragraph.SymbolicCharacters = new Dictionary<SymbolDefinition, Tuple<IntegerValue, SymbolReference>>();
				}
				foreach(var symbolicCharactersContext in context.symbolicCharactersClause())
				{
					SymbolReference alphabetName = null;
					if (symbolicCharactersContext.alphabetNameReference() != null)
					{
						alphabetName = _cobolWordsBuilder.CreateAlphabetNameReference(symbolicCharactersContext.alphabetNameReference());
					}
                    foreach(var symbolicCharOPContext in symbolicCharactersContext.symbolicCharactersOrdinalPositions())
                    {
                        for (int i = 0; i < Math.Min(symbolicCharOPContext.symbolicCharacterDefinition().Length, symbolicCharOPContext.ordinalPositionInCollatingSequence().Length); i++)
						{
							var symbolicCharacter = _cobolWordsBuilder.CreateSymbolicCharacterDefinition(symbolicCharOPContext.symbolicCharacterDefinition()[i]);
							var ordinalPosition = CobolWordsBuilder.CreateIntegerValue(symbolicCharOPContext.ordinalPositionInCollatingSequence()[i].IntegerLiteral());
							paragraph.SymbolicCharacters.Add(symbolicCharacter,
								new Tuple<IntegerValue, SymbolReference>(ordinalPosition, alphabetName));
						}
					}
				}
			}
			if (context.classClause() != null && context.classClause().Length > 0)
			{
				if(paragraph.CharsetClassNames == null)
				{
					paragraph.CharsetClassNames = new Dictionary<SymbolDefinition, UserDefinedCollatingSequence>();
				}
				foreach(var classContext in context.classClause())
				{
					var characterClassName = _cobolWordsBuilder.CreateCharacterClassNameDefinition(classContext.characterClassNameDefinition());
					var userDefinedCharacterClass = new UserDefinedCollatingSequence();
					userDefinedCharacterClass.CharacterSets = new CharacterSetInCollatingSequence[classContext.userDefinedCharacterClass().Length];
					for (int i = 0; i < classContext.userDefinedCharacterClass().Length; i++)
					{
						var userDefinedCCContext = classContext.userDefinedCharacterClass()[i];
						if (userDefinedCCContext.characterInCollatingSequence() != null)
						{
							userDefinedCharacterClass.CharacterSets[i] = CreateCharacterInCollatingSequence(
								userDefinedCCContext.characterInCollatingSequence());
						}
						else if (userDefinedCCContext.charactersRange() != null)
						{
							userDefinedCharacterClass.CharacterSets[i] = CreateCharactersRange(
								userDefinedCCContext.charactersRange());
						}
					}
					paragraph.CharsetClassNames.Add(characterClassName, userDefinedCharacterClass);
				}
			}
			if (context.currencySignClause() != null && context.currencySignClause().Length > 0)
			{
				if(paragraph.CurrencySymbols == null)
				{
					paragraph.CurrencySymbols = new Dictionary<AlphanumericValue, CharacterValue>();
				}
				foreach (var currencySignContext in context.currencySignClause())
				{

                    var currencySign = _cobolWordsBuilder.CreateAlphanumericValue(currencySignContext.sign);
					CharacterValue characterValue = null;
					if (currencySignContext.pictureSymbol != null)
					{
						characterValue = CobolWordsBuilder.CreateCharacterValue(currencySignContext.pictureSymbol);
					}
					paragraph.CurrencySymbols.Add(currencySign, characterValue);
				}
			}
			if (context.decimalPointClause() != null && context.decimalPointClause().Length > 0)
			{
				var decimalPointContext = context.decimalPointClause()[0];
				if (decimalPointContext.COMMA() != null)
				{
					paragraph.DecimalPointIsComma = new SyntaxProperty<bool>(true,
						ParseTreeUtils.GetFirstToken(decimalPointContext.COMMA()));
				}
			}
			if (context.xmlSchemaClause() != null && context.xmlSchemaClause().Length > 0)
			{
				if(paragraph.XmlSchemaNames == null)
				{
					paragraph.XmlSchemaNames = new Dictionary<SymbolDefinition, ExternalName>();
				}
				foreach (var xmlSchemaContext in context.xmlSchemaClause())
				{
					var xmlSchemName = _cobolWordsBuilder.CreateXmlSchemaNameDefinition(xmlSchemaContext.xmlSchemaNameDefinition());
					var assignmentName = _cobolWordsBuilder.CreateAssignmentName(xmlSchemaContext.assignmentName());
					paragraph.XmlSchemaNames.Add(xmlSchemName, assignmentName);
				}
			}

			Context = context;
			CodeElement = paragraph;
			SpecialNamesParagraphChecker.OnCodeElement(paragraph, context, duplicateEnvironments, duplicateMnemonicsForEnvironment);
		}

		private CharactersRangeInCollatingSequence CreateCharactersRange(CodeElementsParser.CharactersRangeContext context) {
			var charactersRange = new CharactersRangeInCollatingSequence();
			charactersRange.StartCharacter = CreateCharacterInCollatingSequence(context.startCharacter);
			charactersRange.EndCharacter = CreateCharacterInCollatingSequence(context.endCharacter);
			return charactersRange;
		}

        private CharacterInCollatingSequence CreateCharacterInCollatingSequence(CodeElementsParser.CharacterInCollatingSequenceContext context) {
			var chars = new CharacterInCollatingSequence();
			if (context.alphanumericLiteralToken() != null) {
				chars.CharacterValue = CobolWordsBuilder.CreateCharacterValue(context.alphanumericLiteralToken());
			} else if (context.figurativeConstant() != null) {
				chars.CharacterValue = _cobolWordsBuilder.CreateFigurativeConstant(context.figurativeConstant());
			} else if (context.ordinalPositionInCollatingSequence() != null) {
				chars.OrdinalPositionInCollatingSequence = CobolWordsBuilder.CreateIntegerValue(context.ordinalPositionInCollatingSequence().IntegerLiteral());
			}
			return chars;
		}

		// --- REPOSITORY PARAGRAPH ---

		public override void EnterRepositoryParagraph(CodeElementsParser.RepositoryParagraphContext context) {
			var paragraph = new RepositoryParagraph();
			if(context.repositoryClassDeclaration() != null &  context.repositoryClassDeclaration().Length > 0) {
				if(paragraph.ClassNames == null) {
					paragraph.ClassNames = new Dictionary<SymbolDefinitionOrReference, SymbolDefinitionOrReference>();
				}
				foreach(var c in context.repositoryClassDeclaration()) {
					var className = _cobolWordsBuilder.CreateClassNameDefOrRef(c.classNameDefOrRef());
					SymbolDefinitionOrReference externalClassName = null;
					if(c.externalClassNameDefOrRef() != null) {
						externalClassName = _cobolWordsBuilder.CreateExternalClassNameDefOrRef(c.externalClassNameDefOrRef());
					}
					paragraph.ClassNames.Add(className, externalClassName);
				}
			}

			Context = context;
			CodeElement = paragraph;
		}



		 // INPUT-OUTPUT SECTION
		//////////////////////////

		public override void EnterInputOutputSectionHeader(CodeElementsParser.InputOutputSectionHeaderContext context) {
			Context = context;
			CodeElement = new InputOutputSectionHeader();
		}

		public override void EnterFileControlParagraphHeader(CodeElementsParser.FileControlParagraphHeaderContext context) {
			Context = context;
			CodeElement = new FileControlParagraphHeader();
		}

		public override void EnterFileControlEntry(CodeElementsParser.FileControlEntryContext context)
		{
			var entry = new FileControlEntry();

			if (context.selectClause() != null)
			{
				entry.FileName = _cobolWordsBuilder.CreateFileNameDefinition(context.selectClause().fileNameDefinition());
				if (context.selectClause().OPTIONAL() != null)
				{
					entry.IsOptional = new SyntaxProperty<bool>(true,
						ParseTreeUtils.GetFirstToken(context.selectClause().OPTIONAL()));
				}
			}
			if (context.assignClause() != null && context.assignClause().Length > 0)
			{
				var assignClauseContext = context.assignClause()[0];
                entry.ExternalDataSet = _cobolWordsBuilder.CreateAssignmentName(assignClauseContext.assignmentName().FirstOrDefault());
			}
			if (context.reserveClause() != null && context.reserveClause().Length > 0)
			{
				var reserveClauseContext = context.reserveClause()[0];
				entry.ReserveIOBuffersCount = CobolWordsBuilder.CreateIntegerValue(reserveClauseContext.IntegerLiteral());
			}
			if (context.accessModeClause() != null && context.accessModeClause().Length > 0)
			{
				var accessModeClauseContext = context.accessModeClause()[0];
				if (accessModeClauseContext.SEQUENTIAL() != null)
				{
					entry.AccessMode = new SyntaxProperty<FileAccessMode>(FileAccessMode.Sequential,
						ParseTreeUtils.GetFirstToken(accessModeClauseContext.SEQUENTIAL()));
				}
				else if (accessModeClauseContext.RANDOM() != null)
				{
					entry.AccessMode = new SyntaxProperty<FileAccessMode>(FileAccessMode.Random,
						ParseTreeUtils.GetFirstToken(accessModeClauseContext.RANDOM()));
				}
				else if (accessModeClauseContext.DYNAMIC() != null)
				{
					entry.AccessMode = new SyntaxProperty<FileAccessMode>(FileAccessMode.Dynamic,
						ParseTreeUtils.GetFirstToken(accessModeClauseContext.DYNAMIC()));
				}
			}
			if (context.fileStatusClause() != null && context.fileStatusClause().Length > 0)
			{
				var fileStatusClauseContext = context.fileStatusClause()[0];
				entry.FileStatus = _cobolExpressionsBuilder.CreateStorageArea(fileStatusClauseContext.fileStatus);
				if (fileStatusClauseContext.vsamReturnCode != null)
				{
					entry.VSAMReturnCode = _cobolExpressionsBuilder.CreateStorageArea(fileStatusClauseContext.vsamReturnCode);
				}
			}

			SyntaxProperty<FileRecordsOrganization> recordsOrganization = null;
			CharacterVariable paddingCharacter = null;
			Token recordDelimiter = null;
			SymbolReference recordKey = null;
			AlternateRecordKey[] alternateRecordKeys = null;
			SymbolReference relativeKey = null;
			SymbolReference password = null;
			if (context.organizationClause() != null && context.organizationClause().Length > 0)
			{
				var organizationClauseContext = context.organizationClause()[0];
				if (organizationClauseContext.SEQUENTIAL() != null)
				{
					recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.Sequential,
						ParseTreeUtils.GetFirstToken(organizationClauseContext.SEQUENTIAL()));
				}
				else if (organizationClauseContext.INDEXED() != null)
				{
					recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.Indexed,
						ParseTreeUtils.GetFirstToken(organizationClauseContext.INDEXED()));
				}
				else if (organizationClauseContext.RELATIVE() != null)
				{
					recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.Relative,
						ParseTreeUtils.GetFirstToken(organizationClauseContext.RELATIVE()));
				}
				if (organizationClauseContext.LINE() != null)
				{
					recordsOrganization = new SyntaxProperty<FileRecordsOrganization>(FileRecordsOrganization.LineSequential,
						ParseTreeUtils.GetFirstToken(organizationClauseContext.LINE()));
				}
			}
			if (context.paddingCharacterClause() != null && context.paddingCharacterClause().Length > 0)
			{
				var paddingCharacterClauseContext = context.paddingCharacterClause()[0];
				paddingCharacter = _cobolExpressionsBuilder.CreateCharacterVariable(paddingCharacterClauseContext.characterVariable());
			}
			if (context.recordDelimiterClause() != null && context.recordDelimiterClause().Length > 0)
			{
				var recordDelimiterClauseContext = context.recordDelimiterClause()[0];
				if (recordDelimiterClauseContext.STANDARD_1() != null)
				{
					recordDelimiter = ParseTreeUtils.GetFirstToken(recordDelimiterClauseContext.STANDARD_1());
				}
				else if (recordDelimiterClauseContext.literalOrUserDefinedWordOReservedWordExceptCopy() != null)
				{
					recordDelimiter = ParseTreeUtils.GetFirstToken(recordDelimiterClauseContext.literalOrUserDefinedWordOReservedWordExceptCopy());
				}
			}
			if (context.recordKeyClause() != null && context.recordKeyClause().Length > 0)
			{
				var recordKeyClauseContext = context.recordKeyClause()[0];
				recordKey = _cobolWordsBuilder.CreateDataNameReference(recordKeyClauseContext.dataNameReference());
			}
			if (context.alternateRecordKeyClause() != null && context.alternateRecordKeyClause().Length > 0)
			{
				alternateRecordKeys = new AlternateRecordKey[context.alternateRecordKeyClause().Length];
				for (int i = 0; i < context.alternateRecordKeyClause().Length; i++)
				{
					var alternateRecordKeyClauseContext = context.alternateRecordKeyClause()[i];
					alternateRecordKeys[i] = new AlternateRecordKey();
					alternateRecordKeys[i].RecordKey = _cobolWordsBuilder.CreateDataNameReference(alternateRecordKeyClauseContext.recordKey);
					if (alternateRecordKeyClauseContext.DUPLICATES() != null)
					{
						alternateRecordKeys[i].AllowDuplicates = new SyntaxProperty<bool>(true,
							ParseTreeUtils.GetFirstToken(alternateRecordKeyClauseContext.DUPLICATES()));
					}
					if (alternateRecordKeyClauseContext.password != null)
					{
						alternateRecordKeys[i].Password = _cobolWordsBuilder.CreateDataNameReference(alternateRecordKeyClauseContext.password);
					}
				}
			}
			if (context.relativeKeyClause() != null && context.relativeKeyClause().Length > 0)
			{
				var relativeKeyClauseContext = context.relativeKeyClause()[0];
				relativeKey = _cobolWordsBuilder.CreateDataNameReference(relativeKeyClauseContext.dataNameReference());
			}
			if (context.passwordClause() != null && context.passwordClause().Length > 0)
			{
				var passwordClauseContext = context.passwordClause()[0];
				password = _cobolWordsBuilder.CreateDataNameReference(passwordClauseContext.dataNameReference());
			}
			if (recordsOrganization != null)
			{
				switch (recordsOrganization.Value)
				{
					case FileRecordsOrganization.Sequential:
					case FileRecordsOrganization.LineSequential:
						var sequentialFileStructure = new SequentialFileStructure();
						sequentialFileStructure.RecordsOrganization = recordsOrganization;
						sequentialFileStructure.PaddingCharacter = paddingCharacter;
						sequentialFileStructure.RecordDelimiter = recordDelimiter;
						sequentialFileStructure.Password = password;
						entry.Structure = sequentialFileStructure;
						break;
					case FileRecordsOrganization.Indexed:
						var indexedFileStructure = new IndexedFileStructure();
						indexedFileStructure.RecordsOrganization = recordsOrganization;
						indexedFileStructure.RecordKey = recordKey;
						indexedFileStructure.Password = password;
						indexedFileStructure.AlternateRecordKeys = alternateRecordKeys;
						entry.Structure = indexedFileStructure;
						break;
					case FileRecordsOrganization.Relative:
						var relativeFileStructure = new RelativeFileStructure();
						relativeFileStructure.RecordsOrganization = recordsOrganization;
						relativeFileStructure.RelativeKey = relativeKey;
						relativeFileStructure.Password = password;
						entry.Structure = relativeFileStructure;
						break;
				}
			}

			Context = context;
			CodeElement = entry;
		}
		
		public override void EnterIoControlParagraphHeader(CodeElementsParser.IoControlParagraphHeaderContext context) {
			Context = context;
			CodeElement = new IOControlParagraphHeader();
		}

		public override void EnterIoControlEntry(CodeElementsParser.IoControlEntryContext context)
		{
			IOControlEntry entry = null;
			if (context.rerunClause() != null)
			{
				var rerunClauseContext = context.rerunClause();
				var rerunEntry = new RerunIOControlEntry();
				rerunEntry.OnExternalDataSetOrFileName = _cobolWordsBuilder.CreateAssignmentNameOrFileNameReference(
					rerunClauseContext.assignmentNameOrFileNameReference());
				if (rerunClauseContext.RECORDS() != null)
				{
					rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryRecordCount,
						ParseTreeUtils.GetFirstToken(rerunClauseContext.RECORDS()));
					rerunEntry.EveryRecordCount = CobolWordsBuilder.CreateIntegerValue(rerunClauseContext.IntegerLiteral());
				}
				else if (rerunClauseContext.REEL() != null)
				{
					rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryEndOfReelUnit,
						ParseTreeUtils.GetFirstToken(rerunClauseContext.REEL()));
				}
				else if (rerunClauseContext.UNIT() != null)
				{
					rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryEndOfReelUnit,
						ParseTreeUtils.GetFirstToken(rerunClauseContext.UNIT()));
				}
				if (rerunClauseContext.fileNameReference() != null)
				{
					rerunEntry.OfFileName = _cobolWordsBuilder.CreateFileNameReference(rerunClauseContext.fileNameReference());
				}
				entry = rerunEntry;
			}
			else if (context.sameAreaClause() != null)
			{
				var sameAreaClauseContext = context.sameAreaClause();
				var sameAreaEntry = new SameAreaIOControlEntry();
				if (sameAreaClauseContext.RECORD() != null)
				{
					sameAreaEntry.SameAreaType = new SyntaxProperty<SameAreaType>(SameAreaType.SameRecordArea,
						ParseTreeUtils.GetFirstToken(sameAreaClauseContext.RECORD()));
				}
				else if (sameAreaClauseContext.SORT() != null)
				{
					sameAreaEntry.SameAreaType = new SyntaxProperty<SameAreaType>(SameAreaType.SameSortArea,
						ParseTreeUtils.GetFirstToken(sameAreaClauseContext.SORT()));
				}
				else if (sameAreaClauseContext.SORT_MERGE() != null)
				{
					sameAreaEntry.SameAreaType = new SyntaxProperty<SameAreaType>(SameAreaType.SameSortMergeArea,
						ParseTreeUtils.GetFirstToken(sameAreaClauseContext.SORT_MERGE()));
				}
				sameAreaEntry.FileNames = new SymbolReference[sameAreaClauseContext.fileNameReference().Length];
				for (int i = 0; i < sameAreaClauseContext.fileNameReference().Length; i++)
				{
					var fileNameReferenceContext = sameAreaClauseContext.fileNameReference()[i];
					sameAreaEntry.FileNames[i] = _cobolWordsBuilder.CreateFileNameReference(fileNameReferenceContext);
				}
				entry = sameAreaEntry;
			}
			else if (context.multipleFileTapeClause() != null)
			{
				var multipleFTClauseContext = context.multipleFileTapeClause();
				var multipleFileEntry = new MultipleFileTapeIOControlEntry();
				multipleFileEntry.PhysicalReelOfTape = new PhysicalReelOfTape[multipleFTClauseContext.physicalReelOfTape().Length];
				for (int i = 0; i < multipleFTClauseContext.physicalReelOfTape().Length; i++)
				{
					var physicalReelOfTapeContext = multipleFTClauseContext.physicalReelOfTape()[i];
					var physicalReelOfTape = new PhysicalReelOfTape();
					physicalReelOfTape.FileName = _cobolWordsBuilder.CreateFileNameReference(physicalReelOfTapeContext.fileNameReference());
					if (physicalReelOfTapeContext.IntegerLiteral() != null)
					{
						physicalReelOfTape.FilePosition = CobolWordsBuilder.CreateIntegerValue(physicalReelOfTapeContext.IntegerLiteral());
					}
					multipleFileEntry.PhysicalReelOfTape[i] = physicalReelOfTape;
				}
				entry = multipleFileEntry;
			}
			else if (context.applyWriteOnlyClause() != null)
			{
				var applyWOClauseContext = context.applyWriteOnlyClause();
				var applyWOEntry = new ApplyWriteOnlyIOControlEntry();
				applyWOEntry.FileNames = new SymbolReference[applyWOClauseContext.fileNameReference().Length];
				for (int i = 0; i < applyWOClauseContext.fileNameReference().Length; i++)
				{
					var fileNameReferenceContext = applyWOClauseContext.fileNameReference()[i];
					applyWOEntry.FileNames[i] = _cobolWordsBuilder.CreateFileNameReference(fileNameReferenceContext);
				}
				entry = applyWOEntry;
			}

			Context = context;
			CodeElement = entry;
		}





		  ///////////////////
		 // DATA DIVISION //
		///////////////////

		public override void EnterDataDivisionHeader(CodeElementsParser.DataDivisionHeaderContext context) {
			Context = context;
			CodeElement = new DataDivisionHeader();
		}

		public override void EnterFileSectionHeader(CodeElementsParser.FileSectionHeaderContext context) {
			Context = context;
			CodeElement = new FileSectionHeader();
		}

		public override void EnterWorkingStorageSectionHeader(CodeElementsParser.WorkingStorageSectionHeaderContext context) {
			Context = context;
			CodeElement = new WorkingStorageSectionHeader();
		}

		public override void EnterLocalStorageSectionHeader(CodeElementsParser.LocalStorageSectionHeaderContext context) {
			Context = context;
			CodeElement = new LocalStorageSectionHeader();
		}

		public override void EnterLinkageSectionHeader(CodeElementsParser.LinkageSectionHeaderContext context) {
			Context = context;
			CodeElement = new LinkageSectionHeader();
		}

		 // FILE DESCRIPTION ENTRY
		////////////////////////////

		public override void EnterFileDescriptionEntry(CodeElementsParser.FileDescriptionEntryContext context) {
			var entry = new FileDescriptionEntry();

			if (context.FD() != null)
				entry.LevelIndicator = new SyntaxProperty<FileDescriptionType>(FileDescriptionType.File, ParseTreeUtils.GetFirstToken(context.FD()));
			else
			if (context.SD() != null)
				entry.LevelIndicator = new SyntaxProperty<FileDescriptionType>(FileDescriptionType.SortMergeFile, ParseTreeUtils.GetFirstToken(context.SD()));

			// FD or SD name acts as both a reference to a file defined in FILE-CONTROL paragraph and the definition of a new data item.
			entry.FileName = _cobolWordsBuilder.CreateFileNameReference(context.fileNameReferenceAndDataNameDefinition());
			entry.DataName = _cobolWordsBuilder.CreateDataNameDefinition(context.fileNameReferenceAndDataNameDefinition());

			if (context.externalClause() != null && context.externalClause().Length > 0) {
				var externalClauseContext = context.externalClause()[0];
				entry.External = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(externalClauseContext.EXTERNAL()));
			}
			if (context.globalClause() != null && context.globalClause().Length > 0) {
				var globalClauseContext = context.globalClause()[0];
				entry.Global = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(globalClauseContext.GLOBAL()));
			}
			if (context.blockContainsClause() != null && context.blockContainsClause().Length > 0) {
				var blockContainsClauseContext = context.blockContainsClause()[0];
				if (blockContainsClauseContext.maxNumberOfBytes != null) {
					entry.MaxBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.maxNumberOfBytes);
				}
				if (blockContainsClauseContext.minNumberOfBytes != null) {
					entry.MinBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.minNumberOfBytes);
				}
				if (blockContainsClauseContext.CHARACTERS() != null) {
					entry.BlockSizeUnit = new SyntaxProperty<BlockSizeUnit>(BlockSizeUnit.Characters,
						ParseTreeUtils.GetFirstToken(blockContainsClauseContext.CHARACTERS()));
				} else
				if (blockContainsClauseContext.RECORDS() != null) {
					entry.BlockSizeUnit = new SyntaxProperty<BlockSizeUnit>(BlockSizeUnit.Records, ParseTreeUtils.GetFirstToken(blockContainsClauseContext.RECORDS()));
				}
			}
			if (context.recordClause() != null && context.recordClause().Length > 0) {
				var recordClauseContext = context.recordClause()[0];
				if (recordClauseContext.numberOfBytes != null) {
					entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.numberOfBytes);
					entry.MaxRecordSize = entry.MinRecordSize;
				} else
				if (recordClauseContext.minNumberOfBytes != null) {
					entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.minNumberOfBytes);
					entry.MaxRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.maxNumberOfBytes);
				} else
				if (recordClauseContext.VARYING() != null) {
					if (recordClauseContext.fromNumberOfBytes != null)
						entry.MinRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.fromNumberOfBytes);
					if (recordClauseContext.toNumberOfBytes != null)
						entry.MaxRecordSize = CobolWordsBuilder.CreateIntegerValue(recordClauseContext.toNumberOfBytes);
					if (recordClauseContext.dataNameReference() != null)
						entry.RecordSizeDependingOn = _cobolWordsBuilder.CreateDataNameReference(recordClauseContext.dataNameReference());
				}
			}
			if (context.labelRecordsClause() != null && context.labelRecordsClause().Length > 0) {
				var labelRecordClauseContext = context.labelRecordsClause()[0];
				if (labelRecordClauseContext.STANDARD() != null) {
					entry.LabelRecordType = new SyntaxProperty<LabelRecordType>(LabelRecordType.StandardLabels, ParseTreeUtils.GetFirstToken(labelRecordClauseContext.STANDARD()));
				}
				if (labelRecordClauseContext.OMITTED() != null) {
					entry.LabelRecordType = new SyntaxProperty<LabelRecordType>(LabelRecordType.Omitted, ParseTreeUtils.GetFirstToken(labelRecordClauseContext.OMITTED()));
				} else
				if (labelRecordClauseContext.dataNameReference() != null && labelRecordClauseContext.dataNameReference().Length > 0) {
					entry.LabelRecords = new SymbolReference[labelRecordClauseContext.dataNameReference().Length];
					for (int i = 0; i < labelRecordClauseContext.dataNameReference().Length; i++)
						entry.LabelRecords[i] = _cobolWordsBuilder.CreateDataNameReference(labelRecordClauseContext.dataNameReference()[i]);
				}
			}
			if (context.valueOfClause() != null && context.valueOfClause().Length > 0) {
				var valueOfClauseContext = context.valueOfClause()[0];
				entry.ValueOfLabelRecords = new Dictionary<SymbolReference, Variable>();
				for (int i = 0; i < valueOfClauseContext.qualifiedDataName().Length; i++) {
					entry.ValueOfLabelRecords.Add(
						_cobolWordsBuilder.CreateQualifiedDataName(valueOfClauseContext.qualifiedDataName()[i]),
						_cobolExpressionsBuilder.CreateVariable(valueOfClauseContext.variable5()[i]));
				}
			}
			if (context.dataRecordsClause() != null && context.dataRecordsClause().Length > 0) {
				var dataRecordClauseContext = context.dataRecordsClause()[0];
				entry.DataRecords = new SymbolReference[dataRecordClauseContext.dataNameReference().Length];
				for (int i = 0; i < dataRecordClauseContext.dataNameReference().Length; i++)
					entry.DataRecords[i] = _cobolWordsBuilder.CreateDataNameReference(dataRecordClauseContext.dataNameReference()[i]);
			}
			if (context.linageClause() != null && context.linageClause().Length > 0) {
				var linageClauseContext = context.linageClause()[0];
				if (linageClauseContext.numberOfLinesInPage != null)
					entry.LogicalPageBodyLineCount = _cobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInPage);
				if (linageClauseContext.firstLineNumberOfFootingArea != null)
					entry.LogicalPageFootingLineNumber = _cobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.firstLineNumberOfFootingArea);
				if (linageClauseContext.numberOfLinesInTopMargin != null)
					entry.LogicalPageTopMarginLineCount = _cobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInTopMargin);
				if (linageClauseContext.numberOfLinesInBottomMargin != null)
					entry.LogicalPageBottomMarginLineCount = _cobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInBottomMargin);
			}
			if (context.recordingModeClause() != null && context.recordingModeClause().Length > 0) {
				var recordingModeClauseContext = context.recordingModeClause()[0];
				entry.RecordingMode = _cobolWordsBuilder.CreateRecordingMode(recordingModeClauseContext.recordingMode());
			}

			Context = context;
			CodeElement = entry;
		}

		public override void EnterDataDescriptionEntry(CodeElementsParser.DataDescriptionEntryContext context)
		{
            if (context.dataRenamesEntry() != null || context.dataConditionEntry() != null) {
				// For levels 66 and 88, the DataDefinitionEntry is created by the following methods
				// - EnterDataRenamesEntry
				// - EnterDataConditionEntry
				return;
			}
			if (context.redefinesClause() != null) {
				// Redefines clause is not a separate rule in the grammar for optimization purposes,
				// but we pretend here that it is a separate rule
				EnterDataRedefinesEntry(context);
                return;
			}

            DataDescriptionEntry entry;
// [COBOL 2002]
            //Variable declared with a TYPEDEF
            if (context.cobol2002TypedefClause() != null) {
				var typedef = new DataTypeDescriptionEntry();
                typedef.DataTypeName = _cobolWordsBuilder.CreateDataTypeNameDefinition(context.dataNameDefinition());
                var strong = context.cobol2002TypedefClause().STRONG();
                var strict = context.cobol2002TypedefClause().STRICT();
           
                typedef.Strong = new SyntaxProperty<bool>(strong != null, ParseTreeUtils.GetFirstToken(strong));
                typedef.Strict = new SyntaxProperty<bool>(strict != null, ParseTreeUtils.GetFirstToken(strict));
                
                //Set visibility if any qualifier is present otherwise the Local visibility is used.
                typedef.Visibility = AccessModifier.Local;
                if (context.cobol2002TypedefClause().PRIVATE() != null) typedef.Visibility = AccessModifier.Private;
                if (context.cobol2002TypedefClause().PUBLIC() != null) typedef.Visibility = AccessModifier.Public;

                var restrictionLevel = typedef.Strong.Value ? RestrictionLevel.STRONG 
                                        : typedef.Strict.Value ? RestrictionLevel.STRICT 
                                        : RestrictionLevel.WEAK;
        
        // [TypeCobol]
                if (context.formalizedComment() != null)
                {
                    typedef.FormalizedCommentDocumentation = new FormalizedCommentDocumentation(context.formalizedComment().formalizedCommentLine());
                    
                }
        // [/TypeCobol]
        
                entry = typedef;
                entry.DataName = typedef.DataTypeName;
                entry.DataType = new DataType(typedef.DataTypeName.Name, restrictionLevel, CobolLanguageLevel.Cobol2002); 
            }
// [/COBOL 2002]
            else {               
                entry = new DataDescriptionEntry();
                entry.DataName = _cobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
                entry.DataType = DataType.Unknown;
            }
            

			if (context.externalClause() != null && context.externalClause().Length > 0) {
				entry.External = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.externalClause()[0].EXTERNAL()));
			}

            EnterCommonDataDescriptionAndDataRedefines(entry, context);

            Context = context;
			CodeElement = entry;


		    DataDescriptionChecker.OnCodeElement(entry, context);
		    if (context.cobol2002TypedefClause() != null)
		        TypeDefinitionEntryChecker.CheckTypedef(entry as DataTypeDescriptionEntry, context);

		}
		private SyntaxProperty<DataUsage> CreateUsageClause(CodeElementsParser.UsageClauseContext c) {
			return  CreateDataUsageProperty(DataUsage.Binary, c.BINARY()) ??
					CreateDataUsageProperty(DataUsage.Binary, c.COMP()) ??
					CreateDataUsageProperty(DataUsage.Binary, c.COMPUTATIONAL()) ??
					CreateDataUsageProperty(DataUsage.Binary, c.COMP_4()) ??
					CreateDataUsageProperty(DataUsage.Binary, c.COMPUTATIONAL_4()) ??
					CreateDataUsageProperty(DataUsage.FloatingPoint, c.COMP_1()) ??
					CreateDataUsageProperty(DataUsage.FloatingPoint, c.COMPUTATIONAL_1()) ??
					CreateDataUsageProperty(DataUsage.LongFloatingPoint, c.COMP_2()) ??
					CreateDataUsageProperty(DataUsage.LongFloatingPoint, c.COMPUTATIONAL_2()) ??
					CreateDataUsageProperty(DataUsage.PackedDecimal, c.PACKED_DECIMAL()) ??
					CreateDataUsageProperty(DataUsage.PackedDecimal, c.COMP_3()) ??
					CreateDataUsageProperty(DataUsage.PackedDecimal, c.COMPUTATIONAL_3()) ??
					CreateDataUsageProperty(DataUsage.NativeBinary, c.COMP_5()) ??
					CreateDataUsageProperty(DataUsage.NativeBinary, c.COMPUTATIONAL_5()) ??
					CreateDataUsageProperty(DataUsage.Display, c.DISPLAY()) ??
					CreateDataUsageProperty(DataUsage.DBCS, c.DISPLAY_1()) ??
					CreateDataUsageProperty(DataUsage.Index, c.INDEX()) ??
					CreateDataUsageProperty(DataUsage.National, c.NATIONAL()) ??
					CreateDataUsageProperty(DataUsage.ObjectReference, c.OBJECT()) ??
					CreateDataUsageProperty(DataUsage.ObjectReference, c.REFERENCE()) ??
					CreateDataUsageProperty(DataUsage.Pointer, c.POINTER()) ??
					CreateDataUsageProperty(DataUsage.FunctionPointer, c.FUNCTION_POINTER()) ??
					CreateDataUsageProperty(DataUsage.ProcedurePointer, c.PROCEDURE_POINTER()) ??
					null;
		}
		private SyntaxProperty<DataUsage> CreateDataUsageProperty(DataUsage usage, ITerminalNode node) {
			if (node == null) return null;
			return new SyntaxProperty<DataUsage>(usage, ParseTreeUtils.GetFirstToken(node));
		}

		private void EnterDataRedefinesEntry(CodeElementsParser.DataDescriptionEntryContext context)
		{
            var entry = new DataRedefinesEntry();
			entry.DataName = _cobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
			
			if (context.redefinesClause() != null) {
				entry.RedefinesDataName = _cobolWordsBuilder.CreateDataNameReference(context.redefinesClause().dataNameReference());
			}

            EnterCommonDataDescriptionAndDataRedefines(entry, context);

            Context = context;
			CodeElement = entry;

		    DataDescriptionChecker.CheckRedefines(entry, context);
		}

	    private void EnterCommonDataDescriptionAndDataRedefines(CommonDataDescriptionAndDataRedefines entry, CodeElementsParser.DataDescriptionEntryContext context)
	    {
            if (context.levelNumber != null)
                entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber);
            if (context.FILLER() != null)
                entry.Filler = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.FILLER()));

            if (context.pictureClause() != null && context.pictureClause().Length > 0)
            {
                var pictureClauseContext = context.pictureClause()[0];
                if (pictureClauseContext.pictureCharacterString != null)
                {
                    System.Diagnostics.Debug.Assert(pictureClauseContext.pictureCharacterString is Token);
                    //TokenType is PictureCharacterString so it's ok to create an AlphanumericValue
                    entry.Picture = new AlphanumericValue((Token) pictureClauseContext.pictureCharacterString);
                }
            }

// [COBOL 2002]
            //Variable declared with a Type
            if (context.cobol2002TypeClause() != null && context.cobol2002TypeClause().Length > 0) {
                entry.UserDefinedDataType = _cobolWordsBuilder.CreateQualifiedDataTypeReference(context.cobol2002TypeClause()[0]);
            }
// [/COBOL 2002]

            if (context.blankWhenZeroClause() != null && context.blankWhenZeroClause().Length > 0)
            {
                var blankClauseContext = context.blankWhenZeroClause()[0];
                Token zeroToken = null;
                if (blankClauseContext.ZERO() != null)
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZERO());
                }
                else if (blankClauseContext.ZEROS() != null)
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZEROS());
                }
                else
                {
                    zeroToken = ParseTreeUtils.GetFirstToken(blankClauseContext.ZEROES());
                }
                entry.IsBlankWhenZero = new SyntaxProperty<bool>(true, zeroToken);
            }
            if (context.globalClause() != null && context.globalClause().Length > 0)
            {
                entry.Global = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.globalClause()[0].GLOBAL()));
            }
            if (context.justifiedClause() != null && context.justifiedClause().Length > 0)
            {
                var justifiedClauseContext = context.justifiedClause()[0];
                Token justifiedToken = null;
                if (justifiedClauseContext.JUSTIFIED() != null)
                {
                    justifiedToken = ParseTreeUtils.GetFirstToken(justifiedClauseContext.JUSTIFIED());
                }
                else
                {
                    justifiedToken = ParseTreeUtils.GetFirstToken(justifiedClauseContext.JUST());
                }
                entry.IsJustified = new SyntaxProperty<bool>(true, justifiedToken);
            }
            if (context.groupUsageClause() != null && context.groupUsageClause().Length > 0)
            {
                var groupUsageClauseContext = context.groupUsageClause()[0];
                entry.IsGroupUsageNational = new SyntaxProperty<bool>(true,
                    ParseTreeUtils.GetFirstToken(groupUsageClauseContext.NATIONAL()));
            }
            if (context.occursClause() != null && context.occursClause().Length > 0)
            {
                var occursClauseContext = context.occursClause()[0];
                if (occursClauseContext.minNumberOfOccurences != null)
                {
                    entry.MinOccurencesCount = CobolWordsBuilder.CreateIntegerValue(occursClauseContext.minNumberOfOccurences);
                }
                if (occursClauseContext.maxNumberOfOccurences != null)
                {
                    entry.MaxOccurencesCount = CobolWordsBuilder.CreateIntegerValue(occursClauseContext.maxNumberOfOccurences);
                }
                if (occursClauseContext.UNBOUNDED() != null)
                {
                    entry.HasUnboundedNumberOfOccurences = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(occursClauseContext.UNBOUNDED()));
                }
                if (occursClauseContext.varNumberOfOccurences != null)
                {
                    entry.OccursDependingOn = _cobolExpressionsBuilder.CreateNumericVariable(occursClauseContext.varNumberOfOccurences);
                }
                var duplicateSortingKeysReferences = new List<CodeElementsParser.DataNameReferenceContext>();
                if (occursClauseContext.tableSortingKeys() != null && occursClauseContext.tableSortingKeys().Length > 0)
                {
                    var keyNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                    var tableSortingKeys = new List<TableSortingKey>();
                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
                    {
                        SyntaxProperty<SortDirection> sortDirection;
                        if (tableSortingKeysContext.ASCENDING() != null)
                        {
                            sortDirection = new SyntaxProperty<SortDirection>(SortDirection.Ascending,
                                ParseTreeUtils.GetFirstToken(tableSortingKeysContext.ASCENDING()));
                        }
                        else
                        {
                            sortDirection = new SyntaxProperty<SortDirection>(SortDirection.Descending,
                                ParseTreeUtils.GetFirstToken(tableSortingKeysContext.DESCENDING()));
                        }
                        foreach (var dataNameReference in tableSortingKeysContext.dataNameReference())
                        {
                            SymbolReference sortKey = _cobolWordsBuilder.CreateDataNameReference(dataNameReference);
                            System.Diagnostics.Debug.Assert(sortKey != null);
                            if (keyNames.Add(sortKey.Name))
                            {
                                tableSortingKeys.Add(new TableSortingKey(sortKey, sortDirection));
                            }
                            else
                            {
                                duplicateSortingKeysReferences.Add(dataNameReference);
                            }
                        }
                    }
                    entry.TableSortingKeys = tableSortingKeys.ToArray();
                }
                if (occursClauseContext.indexNameDefinition() != null && occursClauseContext.indexNameDefinition().Length > 0)
                {
                    entry.Indexes = new SymbolDefinition[occursClauseContext.indexNameDefinition().Length];
                    for (int i = 0; i < occursClauseContext.indexNameDefinition().Length; i++)
                    {
                        var indexNameDefinition = occursClauseContext.indexNameDefinition()[i];
                        entry.Indexes[i] = _cobolWordsBuilder.CreateIndexNameDefinition(indexNameDefinition);
                    }
                }

                DataDescriptionChecker.CheckOccurs(entry, occursClauseContext, duplicateSortingKeysReferences);
            }
            if (context.signClause() != null && context.signClause().Length > 0)
            {
                var signClauseContext = context.signClause()[0];
                if (signClauseContext.LEADING() != null)
                {
                    entry.SignPosition = new SyntaxProperty<SignPosition>(SignPosition.Leading,
                        ParseTreeUtils.GetFirstToken(signClauseContext.LEADING()));
                }
                else
                {
                    entry.SignPosition = new SyntaxProperty<SignPosition>(SignPosition.Trailing,
                        ParseTreeUtils.GetFirstToken(signClauseContext.TRAILING()));
                }
                if (signClauseContext.SEPARATE() != null)
                {
                    entry.SignIsSeparate = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(signClauseContext.SEPARATE()));
                }
            }
            if (context.synchronizedClause() != null && context.synchronizedClause().Length > 0)
            {
                var synchronizedClauseContext = context.synchronizedClause()[0];
                if (synchronizedClauseContext.LEFT() != null)
                {
                    entry.Synchronized = new SyntaxProperty<SyncAlignment>(SyncAlignment.Left,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.LEFT()));
                }
                else if (synchronizedClauseContext.RIGHT() != null)
                {
                    entry.Synchronized = new SyntaxProperty<SyncAlignment>(SyncAlignment.Right,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.RIGHT()));
                }
                else
                {
                    entry.Synchronized = new SyntaxProperty<SyncAlignment>(SyncAlignment.None,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNCHRONIZED() ?? synchronizedClauseContext.SYNC()));
                }
            }
            if (context.usageClause() != null && context.usageClause().Length > 0)
            {
                entry.Usage = CreateUsageClause(context.usageClause()[0]);
            }
            if (context.valueClause() != null && context.valueClause().Length > 0 && context.valueClause()[0].value2() != null)
            {
                var valueClauseContext = context.valueClause()[0];
                entry.InitialValue = _cobolWordsBuilder.CreateValue(valueClauseContext.value2());
            }
	        if (context.valueClauseWithBoolean() != null && context.valueClauseWithBoolean().Length > 0)
	        {
	            if (context.valueClauseWithBoolean()[0].booleanValue() != null)
	            {
                    var valueClauseBooleanContext = context.valueClauseWithBoolean()[0].booleanValue();
                    entry.InitialValue = new Value(CobolWordsBuilder.CreateBooleanValue(valueClauseBooleanContext));
                }
                else if (context.valueClauseWithBoolean()[0].value2() != null)
	            {
                    var valueClauseContext = context.valueClauseWithBoolean()[0].value2();
                    entry.InitialValue = _cobolWordsBuilder.CreateValue(valueClauseContext);
                }
            }

            if (entry.DataType == DataType.Unknown)
            {
                // [COBOL 2002]
                if (entry.UserDefinedDataType != null)
                {
                    //Note we can't know here, if the type is strongly/strictly typed or not. This must be done during semantic phase (Node)
                    entry.DataType = DataType.CreateCustom(entry.UserDefinedDataType.Name, RestrictionLevel.WEAK, cobolLanguageLevel: CobolLanguageLevel.Cobol2002);
                }
                // [/COBOL 2002]
                else if (entry.Picture != null)
                { // only for a basic TYPEDEF <typename> PIC <picture>
                    entry.DataType = DataType.Create(entry.Picture.Value);
                }
                else if (entry.IsTableOccurence)
                {
                    //TODO an array can have a Picture. DataType.Occurs should be a separate property.
                    entry.DataType = DataType.Occurs;
                }
                else
                {
                    //No picture and no Type, so we assume it's a group item.
                    entry.DataType = DataType.Alphanumeric;
                }
            }
        }

        public override void EnterDataRenamesEntry(CodeElementsParser.DataRenamesEntryContext context) {
			var entry = new DataRenamesEntry();
			entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber);
			entry.DataName = _cobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
			if (context.renamesClause().qualifiedDataName() != null) {
				entry.RenamesFromDataName = _cobolWordsBuilder.CreateQualifiedDataName(context.renamesClause().qualifiedDataName());
			} else
			if (context.renamesClause().dataNamesRange() != null) {
				entry.RenamesFromDataName = _cobolWordsBuilder.CreateQualifiedDataName(context.renamesClause().dataNamesRange().startDataName);
				entry.RenamesToDataName = _cobolWordsBuilder.CreateQualifiedDataName(context.renamesClause().dataNamesRange().endDataName);
			}

			Context = context;
			CodeElement = entry;

            DataRenamesChecker.OnCodeElement(entry, context);
		}

		public override void EnterDataConditionEntry(CodeElementsParser.DataConditionEntryContext context) {
			var entry = new DataConditionEntry();
			entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber);
			entry.DataName = _cobolWordsBuilder.CreateConditionNameDefinition(context.conditionNameDefinition());
			SetConditionValues(entry, context.valueClauseForCondition());

			Context = context;
			CodeElement = entry;

		    DataConditionChecker.OnCodeElement(entry, context);
		}

		private void SetConditionValues(DataConditionEntry entry,CodeElementsParser.ValueClauseForConditionContext context) {
			if (context == null) return;
			if (context.value1() != null && context.value1().Length > 0) {
				entry.ConditionValues = new Value[context.value1().Length];
				for (int i = 0; i < context.value1().Length; i++)
					entry.ConditionValues[i] = _cobolWordsBuilder.CreateValue(context.value1()[i]);
			}
			if (context.valuesRange() != null && context.valuesRange().Length > 0) {
				entry.ConditionValuesRanges = new ValuesRange[context.valuesRange().Length];
				for (int i = 0; i < context.valuesRange().Length; i++) {
					var valuesRangeContext = context.valuesRange()[i];
					entry.ConditionValuesRanges[i] = new ValuesRange(
						_cobolWordsBuilder.CreateValue(valuesRangeContext.startValue),
						_cobolWordsBuilder.CreateValue(valuesRangeContext.endValue));
				}
			}
		}

		  ////////////////////////
		 // PROCEDURE DIVISION //
		////////////////////////

		public override void EnterProcedureDivisionHeader(CodeElementsParser.ProcedureDivisionHeaderContext context) {
		    // [TypeCobol]
		    FormalizedCommentDocumentation formalizedCommentDocumentation = null;
		    if (context.formalizedComment() != null)
		        formalizedCommentDocumentation = new FormalizedCommentDocumentation(context.formalizedComment().formalizedCommentLine());
			// [/TypeCobol]
			var procedureDivisionHeader = new ProcedureDivisionHeader(formalizedCommentDocumentation);
			Context = context;
			CodeElement = procedureDivisionHeader;

			_languageLevelChecker.Check(procedureDivisionHeader, context);
		}
		public override void EnterUsingPhrase(CodeElementsParser.UsingPhraseContext context) {
			var inputs = _cobolStatementsBuilder.CreateInputParameters(context.programInputParameters());
			((ProcedureDivisionHeader)CodeElement).UsingParameters = inputs;
		}
		public override void EnterReturningPhrase(CodeElementsParser.ReturningPhraseContext context) {
			var receiving = _cobolExpressionsBuilder.CreateSharedStorageArea(context.programOutputParameter().sharedStorageArea2());
			((Returning)CodeElement).ReturningParameter = new CallTargetParameter() { StorageArea = receiving };
        }

        public override void ExitProcedureDivisionHeader(CodeElementsParser.ProcedureDivisionHeaderContext context)
        {
            // Register call parameters (shared storage areas) information at the CodeElement level
            var procedureDivisionHeader = (ProcedureDivisionHeader)CodeElement;
            var callTarget = new CallTarget();
            int parametersCount =
                (procedureDivisionHeader.UsingParameters != null ? procedureDivisionHeader.UsingParameters.Count : 0)
                + (procedureDivisionHeader.ReturningParameter != null ? 1 : 0);
            callTarget.Parameters = new CallTargetParameter[parametersCount];
            int i = 0;
            if(procedureDivisionHeader.UsingParameters != null && procedureDivisionHeader.UsingParameters.Count > 0)
            {
                foreach(var param in procedureDivisionHeader.UsingParameters) {
                    callTarget.Parameters[i] = param;
                    i++;
                }
            }
            if (procedureDivisionHeader.ReturningParameter != null) {
                callTarget.Parameters[i] = procedureDivisionHeader.ReturningParameter;
            }
            procedureDivisionHeader.CallTarget = callTarget;
        }

        public override void EnterDeclarativesHeader(CodeElementsParser.DeclarativesHeaderContext context)
        {
            Context = context;
            CodeElement = new DeclarativesHeader();
        }

		public override void EnterUseStatementForDebuggingDeclarative(CodeElementsParser.UseStatementForDebuggingDeclarativeContext context) {
			Context = context;
			CodeElement = CreateUseStatementForDebuggingDeclarative(context);
		}
		internal UseForDebuggingProcedureStatement CreateUseStatementForDebuggingDeclarative(CodeElementsParser.UseStatementForDebuggingDeclarativeContext context) {
			var statement = new UseForDebuggingProcedureStatement();
			if (context.procedureName() != null && context.procedureName().Length > 0) {
				statement.ProcedureNames = new SymbolReference[context.procedureName().Length];
				for (int i = 0; i < context.procedureName().Length; i++)
					statement.ProcedureNames[i] = _cobolWordsBuilder.CreateProcedureName(context.procedureName()[i]);
			}
			if (context.ALL() != null)
				statement.AllProcedures = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.ALL()));
			return statement;
		}

		public override void EnterUseStatementForExceptionDeclarative(CodeElementsParser.UseStatementForExceptionDeclarativeContext context) {
			Context = context;
			CodeElement = CreateUseStatementForExceptionDeclarative(context);
		}
		internal UseAfterIOExceptionStatement CreateUseStatementForExceptionDeclarative(CodeElementsParser.UseStatementForExceptionDeclarativeContext context) {
			var statement = new UseAfterIOExceptionStatement();
			if (context.GLOBAL() != null)
				statement.IsGlobal = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.GLOBAL()));
			if (context.fileNameReference() != null && context.fileNameReference().Length > 0) {
				statement.FileNames = new SymbolReference[context.fileNameReference().Length];
				for (int i = 0; i < context.fileNameReference().Length; i++)
					statement.FileNames[i] = _cobolWordsBuilder.CreateFileNameReference(context.fileNameReference()[i]);
			}
			statement.OpenMode = CreateOpenMode(context);
			return statement;
		}
		private SyntaxProperty<OpenMode> CreateOpenMode(CodeElementsParser.UseStatementForExceptionDeclarativeContext context) {
			if (context.INPUT() != null) return new SyntaxProperty<OpenMode>(OpenMode.INPUT, ParseTreeUtils.GetFirstToken(context.INPUT()));
			if (context.OUTPUT() != null) return new SyntaxProperty<OpenMode>(OpenMode.OUTPUT, ParseTreeUtils.GetFirstToken(context.OUTPUT()));
			if (context.I_O() != null) return new SyntaxProperty<OpenMode>(OpenMode.IO, ParseTreeUtils.GetFirstToken(context.I_O()));
			if (context.EXTEND() != null) return new SyntaxProperty<OpenMode>(OpenMode.EXTEND, ParseTreeUtils.GetFirstToken(context.EXTEND()));
			return null;
		}

        public override void EnterDeclarativesEnd(CodeElementsParser.DeclarativesEndContext context)
        {
            Context = context;
            CodeElement = new DeclarativesEnd();
        }

        // -- Section --

        public override void EnterSectionHeader(CodeElementsParser.SectionHeaderContext context)
        {
            var sectionHeader = new SectionHeader();

            sectionHeader.SectionName = _cobolWordsBuilder.CreateSectionNameDefinition(context.sectionNameDefinition());
            if (context.priorityNumber() != null)
            {
                sectionHeader.PriorityNumber = CobolWordsBuilder.CreateIntegerValue(context.priorityNumber().IntegerLiteral());
            }

            Context = context;
            CodeElement = sectionHeader;
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CodeElementsParser.ParagraphHeaderContext context)
        {
            var paragraphHeader = new ParagraphHeader();

            paragraphHeader.ParagraphName = _cobolWordsBuilder.CreateParagraphNameDefinition(context.paragraphNameDefinition());

            Context = context;
            CodeElement = paragraphHeader;
        }

        // -- Sentence --

        public override void EnterSentenceEnd(CodeElementsParser.SentenceEndContext context)
        {
            Context = context;
            CodeElement = new SentenceEnd();
        }





		  ////////////////
		 // PARAGRAPHS //
		////////////////


		 // STATEMENTS
		////////////////

		// --- ARITHMETIC STATEMENTS ---

		public override void EnterAddStatement(CodeElementsParser.AddStatementContext context) {
			Context = context;
		    AddStatement addStatement = null;
			if(context.addSimple() != null) {
			    addStatement = _cobolStatementsBuilder.CreateAddStatement(context.addSimple());
			} else
			if (context.addGiving() != null) {
			    addStatement = _cobolStatementsBuilder.CreateAddGivingStatement(context.addGiving());
			} else
			if (context.addCorresponding() != null) {
			    addStatement = _cobolStatementsBuilder.CreateAddCorrespondingStatement(context.addCorresponding());
			} 
            else
            {
                addStatement = new AddSimpleStatement();
            }

		    CodeElement = addStatement;
		    AddStatementChecker.OnCodeElement(addStatement, context);
		}
		public override void EnterAddStatementEnd(CodeElementsParser.AddStatementEndContext context) {
			Context = context;
			CodeElement = new AddStatementEnd();
		}

		public override void EnterComputeStatement(CodeElementsParser.ComputeStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateComputeStatement(context);
		}
		public override void EnterComputeStatementEnd(CodeElementsParser.ComputeStatementEndContext context) {
			Context = context;
			CodeElement = new ComputeStatementEnd();
		}

		public override void EnterDivideStatement(CodeElementsParser.DivideStatementContext context) {
			Context = context;
			if (context.divideSimple() != null) {
				CodeElement = _cobolStatementsBuilder.CreateDivideStatement(context.divideSimple());
			} else if (context.divideGiving() != null) {
				CodeElement = _cobolStatementsBuilder.CreateDivideGivingStatement(context.divideGiving());
			} else if (context.divideRemainder() != null) {
				CodeElement = _cobolStatementsBuilder.CreateDivideRemainderStatement(context.divideRemainder());
			} else {
				//Default
				CodeElement = new DivideSimpleStatement() { SendingAndReceivingStorageAreas = new RoundedResult[0] };
			}
		}
		public override void EnterDivideStatementEnd(CodeElementsParser.DivideStatementEndContext context) {
			Context = context;
			CodeElement = new DivideStatementEnd();
		}

		public override void EnterMultiplyStatement(CodeElementsParser.MultiplyStatementContext context) {
			Context = context;
			if (context.multiplySimple() != null) {
				CodeElement = _cobolStatementsBuilder.CreateMultiplyStatement(context.multiplySimple());
			} else if (context.multiplyGiving() != null) {
				CodeElement = _cobolStatementsBuilder.CreateMultiplyGivingStatement(context.multiplyGiving());
			} else {
				//Default
				CodeElement = new MultiplySimpleStatement() { SendingAndReceivingStorageAreas = new RoundedResult[0] };
			}
		}
		public override void EnterMultiplyStatementEnd(CodeElementsParser.MultiplyStatementEndContext context) {
			Context = context;
			CodeElement = new MultiplyStatementEnd();
		}

		public override void EnterSubtractStatement(CodeElementsParser.SubtractStatementContext context) {
			Context = context;
			if (context.subtractSimple() != null) {
				CodeElement = _cobolStatementsBuilder.CreateSubtractStatement(context.subtractSimple());
			} else if (context.subtractGiving() != null) {
				CodeElement = _cobolStatementsBuilder.CreateSubtractGivingStatement(context.subtractGiving());
			} else if (context.subtractCorresponding() != null) {
				CodeElement = _cobolStatementsBuilder.CreateSubtractCorrespondingStatement(context.subtractCorresponding());
			} else {
				//Default
				CodeElement = new SubtractSimpleStatement() { VariablesTogether = new NumericVariable[0], SendingAndReceivingStorageAreas = new RoundedResult[0] };
			}
		}
		public override void EnterSubtractStatementEnd(CodeElementsParser.SubtractStatementEndContext context) {
			Context = context;
			CodeElement = new SubtractStatementEnd();
		}



		// --- FILE STATEMENTS ---
		
		public override void EnterOpenStatement(CodeElementsParser.OpenStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateOpenStatement(context);
		}

		public override void EnterCloseStatement(CodeElementsParser.CloseStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateCloseStatement(context);
		}

		public override void EnterReadStatement(CodeElementsParser.ReadStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateReadStatement(context);
		}

		public override void EnterReadStatementEnd(CodeElementsParser.ReadStatementEndContext context) {
			Context = context;
			CodeElement = new ReadStatementEnd();
		}

		public override void EnterWriteStatement(CodeElementsParser.WriteStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateWriteStatement(context);
		}

		public override void EnterWriteStatementEnd(CodeElementsParser.WriteStatementEndContext context) {
			Context = context;
			CodeElement = new WriteStatementEnd();
		}
		
		public override void EnterRewriteStatement(CodeElementsParser.RewriteStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateRewriteStatement(context);
		}

		public override void EnterRewriteStatementEnd(CodeElementsParser.RewriteStatementEndContext context) {
			Context = context;
			CodeElement = new RewriteStatementEnd();
		}



		// --- ACCEPT ---

		public override void EnterAcceptStatement(CodeElementsParser.AcceptStatementContext context)
        {
			Context = context;

            AcceptStatement acceptStatement;
            var receivingStorageArea = _cobolExpressionsBuilder.CreateAlphanumericStorageArea(context.alphanumericStorageArea());
            if (context.fromSystemDateTime() != null)
            {
                acceptStatement = _cobolStatementsBuilder.CreateAcceptSystemDateTime(receivingStorageArea, context.fromSystemDateTime());
            }
            else
            {
                acceptStatement = _cobolStatementsBuilder.CreateAcceptDataTransferStatement(receivingStorageArea, context.fromEnvironment());
            }

            AcceptStatementChecker.OnCodeElement(acceptStatement, context);
            CodeElement = acceptStatement;
        }


	    // --- ALTER ---

        public override void EnterAlterStatement(CodeElementsParser.AlterStatementContext context) {
            Context = context;
            AlterStatement alterStatement = _cobolStatementsBuilder.CreateAlterStatement(context);
            CodeElement = alterStatement; 
            AlterStatementChecker.OnCodeElement(alterStatement, context);
        }

        // --- CALL ---
        /// <summary>
        /// Generic and default CALL statement
        /// </summary>
        /// <param name="context"></param>
        public override void EnterCallStatement([NotNull] CodeElementsParser.CallStatementContext context)
        {
            Context = context;
            CallStatement callStatement = null;
            if (context.cobolCallStatement() != null)
            {
                callStatement = _cobolStatementsBuilder.CreateCallStatement(context.cobolCallStatement());
            }
            else if (context.tcCallStatement() != null)
            {
                //Let TypeCobolCodeElementBuilder do the work
            }
            else
            {
                callStatement = new CallStatement();
            }

            CodeElement = callStatement;
            CallStatementChecker.OnCodeElement(callStatement, context);
        }

		public override void EnterCallStatementEnd(CodeElementsParser.CallStatementEndContext context) {
			Context = context;
			CodeElement = new CallStatementEnd();
		}

		// --- CANCEL ---

		public override void EnterCancelStatement(CodeElementsParser.CancelStatementContext context) {
			Context = context;
		    CancelStatement cancelStatement = _cobolStatementsBuilder.CreateCancelStatement(context);
		    CodeElement = cancelStatement;
            
		    CancelStatementChecker.OnCodeElement(cancelStatement, context);
		}

		// --- CONTINUE ---

		public override void EnterContinueStatement(CodeElementsParser.ContinueStatementContext context) {
			Context = context;
			CodeElement = new ContinueStatement();
		}

		// --- DELETE ---

		public override void EnterDeleteStatement(CodeElementsParser.DeleteStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateDeleteStatement(context);
		}

		public override void EnterDeleteStatementEnd(CodeElementsParser.DeleteStatementEndContext context) {
			Context = context;
			CodeElement = new DeleteStatementEnd();
		}

		// --- DISPLAY ---

		public override void EnterDisplayStatement(CodeElementsParser.DisplayStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateDisplayStatement(context);
		}

		// --- ENTRY ---
		
		public override void EnterEntryStatement(CodeElementsParser.EntryStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateEntryStatement(context);
		}

        public override void ExitEntryStatement(CodeElementsParser.EntryStatementContext context)
        {
            // Register call parameters (shared storage areas) information at the CodeElement level
            var entryStatement = (EntryStatement)CodeElement;
            var callTarget = new CallTarget() { Name = entryStatement.ProgramEntry };
            int parametersCount = entryStatement.InputParameters != null ? entryStatement.InputParameters.Count : 0;
            callTarget.Parameters = new CallTargetParameter[parametersCount];
            int i = 0;
            if (entryStatement.InputParameters != null && entryStatement.InputParameters.Count > 0)
            {
                foreach (var param in entryStatement.InputParameters)
                {
                    callTarget.Parameters[i] = param;
                    i++;
                }
            }
            entryStatement.CallTarget = callTarget;
        }

        // --- EVALUATE ---

        public override void EnterEvaluateStatement(CodeElementsParser.EvaluateStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateEvaluateStatement(context); ;
		}

		public override void EnterEvaluateStatementEnd(CodeElementsParser.EvaluateStatementEndContext context) {
			Context = context;
			CodeElement = new EvaluateStatementEnd();
		}

        // --- EXEC ---
        public override void EnterExecStatement(CodeElementsParser.ExecStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateExecStatement(context);
		}

        /// <summary>
        /// Enter a parse tree produced by <see cref="CodeElementsParser.execStatementText"/>.
        /// <para>The default implementation does nothing.</para>
        /// </summary>
        /// <param name="context">The parse tree.</param>
        public override void EnterExecStatementText([NotNull] CodeElementsParser.ExecStatementTextContext context)
        {
            Context = context;            
            CodeElement = _cobolStatementsBuilder.CreateExecStatementText(context);
        }

        /// <summary>
        /// Enter a parse tree produced by <see cref="CodeElementsParser.execStatementEnd"/>.
        /// <para>The default implementation does nothing.</para>
        /// </summary>
        /// <param name="context">The parse tree.</param>
        public override void EnterExecStatementEnd([NotNull] CodeElementsParser.ExecStatementEndContext context)
        {
            Context = context;
            CodeElement = new ExecStatementEnd();
        }

        // --- EXIT ---

        public override void EnterExitStatement(CodeElementsParser.ExitStatementContext context) {
			Context = context;
			CodeElement = new ExitStatement();
		}

		public override void EnterExitMethodStatement(CodeElementsParser.ExitMethodStatementContext context) {
			Context = context;
			CodeElement = new ExitMethodStatement();
		}

		public override void EnterExitProgramStatement(CodeElementsParser.ExitProgramStatementContext context) {
			Context = context;
			CodeElement = new ExitProgramStatement();
		}

        // --- ALLOCATE ---
        public override void EnterAllocateStatement(CodeElementsParser.AllocateStatementContext context)
        {
            Context = context;
            CodeElement = _cobolStatementsBuilder.CreateAllocateStatement(context);
        }

        // --- FREE ---
        public override void EnterFreeStatement(CodeElementsParser.FreeStatementContext context)
        {
            Context = context;
            CodeElement = _cobolStatementsBuilder.CreateFreeStatement(context);
        }

        // --- GOBACK ---
				
		public override void EnterGobackStatement(CodeElementsParser.GobackStatementContext context) {
			Context = context;
			CodeElement = new GobackStatement();
		}

		// --- GOTO ---

		public override void EnterGotoStatement(CodeElementsParser.GotoStatementContext context) {
			Context = context;
			if (context.gotoSimple() != null) {
				CodeElement = _cobolStatementsBuilder.CreateGotoStatement(context.gotoSimple());
			}
			if (context.gotoConditional() != null) {
				var statement = _cobolStatementsBuilder.CreateGotoConditionalStatement(context.gotoConditional());
				CodeElement = statement;
				GotoConditionalStatementChecker.OnCodeElement(statement, context.gotoConditional());
			}
		}



		// --- IF ---

		public override void EnterIfStatement(CodeElementsParser.IfStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateIfStatement(context);
		}
		public override void EnterElseCondition(CodeElementsParser.ElseConditionContext context) {
			Context = context;
			CodeElement = new ElseCondition();
		}
		public override void EnterNextSentenceStatement(CodeElementsParser.NextSentenceStatementContext context) {
			Context = context;
			CodeElement = new NextSentenceStatement();
		}
		public override void EnterIfStatementEnd(CodeElementsParser.IfStatementEndContext context) {
			Context = context;
			CodeElement = new IfStatementEnd();
		}



		// --- INITIALIZE ---

		public override void EnterInitializeStatement(CodeElementsParser.InitializeStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateInitializeStatement(context);
		}

		// --- INSPECT ---

		public override void EnterInspectStatement(CodeElementsParser.InspectStatementContext context) {
			Context = context;
			var inspectStatement = _cobolStatementsBuilder.CreateInspectStatement(context);
		    CodeElement = inspectStatement;


		    var inscpectCorresponding = inspectStatement as InspectConvertingStatement;
		    if (inscpectCorresponding != null)
		        InspectConvertingChecker.OnCodeElement(inscpectCorresponding, context);
		}

		// --- INVOKE ---

		public override void EnterInvokeStatement(CodeElementsParser.InvokeStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateInvokeStatement(context);
		}

        // --- JSON Statements ---

        public override void EnterJsonGenerateStatement(CodeElementsParser.JsonGenerateStatementContext context)
        {
            Context = context;
            CodeElement = _cobolStatementsBuilder.CreateJsonGenerateStatement(context);
        }

        public override void EnterJsonStatementEnd(CodeElementsParser.JsonStatementEndContext context)
        {
            Context = context;
            CodeElement = new JsonStatementEnd();
        }

        // --- JSON PARSE Statements ---

        public override void EnterJsonParseStatement(CodeElementsParser.JsonParseStatementContext context)
        {
            Context = context;
            CodeElement = _cobolStatementsBuilder.CreateJsonParseStatement(context);
        }

        // --- MERGE ---

        public override void EnterMergeStatement(CodeElementsParser.MergeStatementContext context) {
			Context = context;
			var mergeStatement = _cobolStatementsBuilder.CreateMergeStatement(context);
		    CodeElement = mergeStatement;

		    MergeUsingChecker.OnCodeElement(mergeStatement, context);
		}



		// --- PERFORM ---

		public override void EnterPerformStatement(CodeElementsParser.PerformStatementContext context) {
			Context = context;
            if (context.procedureName() != null || context.proceduresRange() != null)
            {
                CodeElement = _cobolStatementsBuilder.CreatePerformProcedureStatement(context);
            }
            else
            {
                CodeElement = _cobolStatementsBuilder.CreatePerformStatement(context);
            }
		}

		public override void EnterPerformStatementEnd(CodeElementsParser.PerformStatementEndContext context) {
			Context = context;
			CodeElement = new PerformStatementEnd();
		}



		// --- RELEASE	 ---

		public override void EnterReleaseStatement(CodeElementsParser.ReleaseStatementContext context) {            
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateReleaseStatement(context);
		}

		// --- RETURN ---

		public override void EnterReturnStatement(CodeElementsParser.ReturnStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateReturnStatement(context);
		}
		public override void EnterReturnStatementEnd(CodeElementsParser.ReturnStatementEndContext context) {
			Context = context;
			CodeElement = new ReturnStatementEnd();
		}

		// --- SEARCH ---

		public override void EnterSearchStatement(CodeElementsParser.SearchStatementContext context) {
			Context = context;
		    SearchStatement searchStatement = null;
			if (context.serialSearch() != null) {
			    searchStatement = _cobolStatementsBuilder.CreateSerialSearchStatement(context.serialSearch());
			} else
			if (context.binarySearch() != null) {
			    searchStatement = _cobolStatementsBuilder.CreateBinarySearchStatement(context.binarySearch());
			}
		    CodeElement = searchStatement;
		    if (searchStatement != null)
		        SearchStatementChecker.OnCodeElement(searchStatement, context);
		}
		public override void EnterSearchStatementEnd(CodeElementsParser.SearchStatementEndContext context) {
			Context = context;
			CodeElement = new SearchStatementEnd();
		}

		// --- SORT ---

		public override void EnterSortStatement(CodeElementsParser.SortStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateSortStatement(context);
		}

		// --- START ---

		public override void EnterStartStatement(CodeElementsParser.StartStatementContext context) {
			Context = context;
			var startStatement = _cobolStatementsBuilder.CreateStartStatement(context);
		    CodeElement = startStatement;
		    StartStatementChecker.OnCodeElement(startStatement, context);
		}
		public override void EnterStartStatementEnd(CodeElementsParser.StartStatementEndContext context) {
			Context = context;
			CodeElement = new StartStatementEnd();
		}
    
		// --- STOP ---

		public override void EnterStopStatement(CodeElementsParser.StopStatementContext context) {
		    Context = context;
		    var stopStatement = _cobolStatementsBuilder.CreateStopStatement(context);
		    CodeElement = stopStatement;
		    StopStatementChecker.OnCodeElement(stopStatement, context);
		}

		// --- STRING ---

		public override void EnterStringStatement(CodeElementsParser.StringStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateStringStatement(context);
		}
		public override void EnterStringStatementEnd(CodeElementsParser.StringStatementEndContext context) {
			Context = context;
			CodeElement = new StringStatementEnd();
		}

		// --- UNSTRING ---

		public override void EnterUnstringStatement(CodeElementsParser.UnstringStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateUnstringStatement(context);
		}
		public override void EnterUnstringStatementEnd(CodeElementsParser.UnstringStatementEndContext context) {
			Context = context;
			CodeElement = new UnstringStatementEnd();
		}

		// --- XML STATEMENTS ---

		public override void EnterXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateXmlGenerateStatement(context);
		}

		public override void EnterXmlParseStatement(CodeElementsParser.XmlParseStatementContext context) {
			Context = context;
			CodeElement = _cobolStatementsBuilder.CreateXmlParseStatement(context);
		}

		public override void EnterXmlStatementEnd(CodeElementsParser.XmlStatementEndContext context) {
			Context = context;
			CodeElement = new XmlStatementEnd();
		}

		// --- MOVE STATEMENT ---

	    public override void EnterMoveStatement(CodeElementsParser.MoveStatementContext context)
	    {
	        Context = context;
	        CodeElement = null;
            if (context == null)
	            return;

	        if (context.moveSimple() != null)
	        {
                CodeElement = _cobolStatementsBuilder.CreateMoveStatement(context.moveSimple());
	        }
	        else if (context.moveCorresponding() != null)
	        {
                CodeElement = _cobolStatementsBuilder.CreateMoveStatement(context.moveCorresponding());
	        }
	        else
	            CodeElement = new MoveSimpleStatement(null, null, null);
	    }

		// --- SET STATEMENT ---

		public override void EnterSetStatement(CodeElementsParser.SetStatementContext context) {
			if (context.setStatementForAssignment() != null) {
				Context = context.setStatementForAssignment();
				var setStatementForAssignment = _cobolStatementsBuilder.CreateSetStatementForAssignment(context.setStatementForAssignment());
			    CodeElement = setStatementForAssignment;
			    SetStatementForAssignmentChecker.OnCodeElement(setStatementForAssignment, context.setStatementForAssignment());
			} else
			if (context.setStatementForIndexes() != null) {
				Context = context.setStatementForIndexes();
				var setStatementForIndexes = _cobolStatementsBuilder.CreateSetStatementForIndexes(context.setStatementForIndexes());
			    CodeElement = setStatementForIndexes;
			    SetStatementForIndexesChecker.OnCodeElement(setStatementForIndexes, context.setStatementForIndexes());
			} else
			if (context.setStatementForSwitches() != null) {
				Context = context.setStatementForSwitches();
				CodeElement = _cobolStatementsBuilder.CreateSetStatementForSwitches(context.setStatementForSwitches());
			} else
			if (context.setStatementForConditions() != null) {
				Context = context.setStatementForConditions();
				CodeElement = _cobolStatementsBuilder.CreateSetStatementForConditions(context.setStatementForConditions());
			}
            else
		    {
		        Context = context;
		        CodeElement = new SetStatementPartial();
		    }
		}



		  //////////////////////////
		 // STATEMENT CONDITIONS //
		//////////////////////////

		public override void EnterAtEndCondition(CodeElementsParser.AtEndConditionContext context) {
			Context = context;
			CodeElement = new AtEndCondition();
		}
		public override void EnterNotAtEndCondition(CodeElementsParser.NotAtEndConditionContext context) {
			Context = context;
			CodeElement = new NotAtEndCondition();
		}

		public override void EnterAtEndOfPageCondition(CodeElementsParser.AtEndOfPageConditionContext context) {
			Context = context;
			CodeElement = new AtEndOfPageCondition();
		}
		public override void EnterNotAtEndOfPageCondition(CodeElementsParser.NotAtEndOfPageConditionContext context) {
			Context = context;
			CodeElement = new NotAtEndOfPageCondition();
		}

		public override void EnterWhenCondition(CodeElementsParser.WhenConditionContext context) {
			Context = context;
			var whenCondition = _cobolStatementsBuilder.CreateWhenCondition(context); ;
			CodeElement = whenCondition;
			WhenConditionStatementChecker.OnCodeElement(whenCondition, context);
		}
		public override void EnterWhenOtherCondition(CodeElementsParser.WhenOtherConditionContext context) {
			Context = context;
			CodeElement = new WhenOtherCondition();
		}

		public override void EnterWhenSearchCondition(CodeElementsParser.WhenSearchConditionContext context) {
			Context = context;
			//Translate WhenSearchConditionContext into a WhenCondition CodeElement (used for both EVALUATE and SEARCH)
			CodeElement = _cobolStatementsBuilder.CreateWhenCondition(context);
		}

		public override void EnterInvalidKeyCondition(CodeElementsParser.InvalidKeyConditionContext context) {
			Context = context;
			CodeElement = new InvalidKeyCondition();
		}
		public override void EnterNotInvalidKeyCondition(CodeElementsParser.NotInvalidKeyConditionContext context) {
			Context = context;
			CodeElement = new NotInvalidKeyCondition();
		}

		public override void EnterOnExceptionCondition(CodeElementsParser.OnExceptionConditionContext context) {
			Context = context;
			CodeElement = new OnExceptionCondition();
		}

		public override void EnterNotOnExceptionCondition(CodeElementsParser.NotOnExceptionConditionContext context) {
			Context = context;
			CodeElement = new NotOnExceptionCondition();
		}

		public override void EnterOnOverflowCondition(CodeElementsParser.OnOverflowConditionContext context) {
			Context = context;
			CodeElement = new OnOverflowCondition();
		}
		public override void EnterNotOnOverflowCondition(CodeElementsParser.NotOnOverflowConditionContext context) {
			Context = context;
			CodeElement = new NotOnOverflowCondition();
		}

		public override void EnterOnSizeErrorCondition(CodeElementsParser.OnSizeErrorConditionContext context) {
			Context = context;
			CodeElement = new OnSizeErrorCondition();
		}
		public override void EnterNotOnSizeErrorCondition(CodeElementsParser.NotOnSizeErrorConditionContext context) {
			Context = context;
			CodeElement = new NotOnSizeErrorCondition();
		}

        // FOR SQL

        /// <summary>
        /// Enter a parse tree produced by <see cref="CodeElementsParser.commitStatement"/>.
        /// <para>The default implementation does nothing.</para>
        /// </summary>
        /// <param name="context">The parse tree.</param>
        public override void EnterCommitStatement([NotNull] CodeElementsParser.CommitStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateCommitStatement(context);
        }

        public override void EnterSelectStatement([NotNull] CodeElementsParser.SelectStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateSelectStatement(context);
        }

        public override void EnterRollbackStatement([NotNull] CodeElementsParser.RollbackStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateRollbackStatement(context);
        }

        public override void EnterTruncateStatement([NotNull] CodeElementsParser.TruncateStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateTruncateStatement(context);
        }

        public override void EnterWhenEverStatement([NotNull] CodeElementsParser.WhenEverStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateWhenEverStatement(context);
        }
        public override void EnterLockTableStatement([NotNull] CodeElementsParser.LockTableStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateLockTableStatement(context);
        }

        public override void EnterReleaseSavepointStatement([NotNull] CodeElementsParser.ReleaseSavepointStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateReleaseSavepointStatement(context);
        }

        public override void EnterSavepointStatement([NotNull] CodeElementsParser.SavepointStatementContext context)
        {
            Context = context;
            var savepointStatement = _sqlCodeElementBuilder.CreateSavepointStatement(context);
            CodeElement = savepointStatement;
            SavepointStatementChecker.OnCodeElement(savepointStatement, context);
        }

        public override void EnterConnectStatement([NotNull] CodeElementsParser.ConnectStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateConnectStatement(context);
        }

        public override void EnterDropTableStatement([NotNull] CodeElementsParser.DropTableStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateDropTableStatement(context);
        }

        public override void EnterSetAssignmentStatement(
            [NotNull] CodeElementsParser.SetAssignmentStatementContext context)
        {
            Context = context;
            var statement = _sqlCodeElementBuilder.CreateSetAssignmentStatement(context);
            CodeElement = statement;
            SetAssignmentStatementChecker.OnCodeElement(statement, context);
        }

        public override void EnterGetDiagnosticsStatement(
            [NotNull] CodeElementsParser.GetDiagnosticsStatementContext context)
        {
            Context = context;
            var statement = _sqlCodeElementBuilder.CreateGetDiagnosticsStatement(context);
            CodeElement = statement;
            GetDiagnosticsStatementChecker.OnCodeElement(statement, context);
        }

        public override void EnterAlterSequenceStatement(
            [NotNull] CodeElementsParser.AlterSequenceStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateAlterSequenceStatement(context);
        }

        public override void EnterExecuteImmediateStatement([NotNull] CodeElementsParser.ExecuteImmediateStatementContext context)
        {
            Context = context;
            var executeImmediateStatement = _sqlCodeElementBuilder.CreateExecuteImmediateStatement(context);
            CodeElement = executeImmediateStatement;
            ExecuteImmediateStatementChecker.OnCodeElement(executeImmediateStatement, context);
        }
    }
}
