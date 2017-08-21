using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Diagnostics;
using Antlr4.Runtime.Misc;

namespace TypeCobol.Compiler.Parser
{
	/// <summary>Builds a CodeElement object while visiting its parse tree.</summary>
	internal partial class CodeElementBuilder: CodeElementsBaseListener {

		private ParserRuleContext Context;

		/// <summary>CodeElement object resulting of the visit the parse tree</summary>
		public CodeElement CodeElement { get; set; }
		public CodeElementDispatcher Dispatcher { get; internal set; }


		private CobolWordsBuilder CobolWordsBuilder { get; set; }
		private CobolExpressionsBuilder CobolExpressionsBuilder { get; set; }
		private CobolStatementsBuilder CobolStatementsBuilder { get; set; }

		/// <summary>Initialization code run before parsing each new COBOL CodeElement</summary>
		public override void EnterCodeElement(CodeElementsParser.CodeElementContext context) {
			CodeElement = null;
			Context = null;
			CobolWordsBuilder = new CobolWordsBuilder(new Dictionary<Token, SymbolInformation>());
			CobolExpressionsBuilder = new CobolExpressionsBuilder(CobolWordsBuilder);
			CobolStatementsBuilder = new CobolStatementsBuilder(CobolWordsBuilder, CobolExpressionsBuilder);
		}

		/// <summary>Code run after parsing each new CodeElement</summary>
		public override void ExitCodeElement(CodeElementsParser.CodeElementContext context) {
			if(CodeElement != null)
            {
               

                if (CobolWordsBuilder.symbolInformationForTokens.Keys.Count > 0) {
                    CodeElement.SymbolInformationForTokens = CobolWordsBuilder.symbolInformationForTokens;
                }
                if (CobolExpressionsBuilder.storageAreaDefinitions.Count > 0) {
                    CodeElement.StorageAreaDefinitions = CobolExpressionsBuilder.storageAreaDefinitions;
                }
                if (CobolExpressionsBuilder.storageAreaReads.Count > 0) {
                    CodeElement.StorageAreaReads = CobolExpressionsBuilder.storageAreaReads;
                }
                if (CobolExpressionsBuilder.storageAreaWrites.Count > 0) {
                    CodeElement.StorageAreaWrites = CobolExpressionsBuilder.storageAreaWrites;
                }
                if (CobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact != null) {
                    CodeElement.StorageAreaGroupsCorrespondingImpact = CobolExpressionsBuilder.storageAreaGroupsCorrespondingImpact;
                }
                if (CobolExpressionsBuilder.callTarget != null) {
                    CodeElement.CallTarget = CobolExpressionsBuilder.callTarget;
                }
                if (CobolExpressionsBuilder.callSites.Count > 0) {
                    CodeElement.CallSites = CobolExpressionsBuilder.callSites;
                }

                Dispatcher.OnCodeElement(CodeElement, Context);

                // Attach all tokens consumed by the parser for this code element
                // Collect all error messages encoutered while parsing this code element
                IList<Diagnostic> diagnostics = CodeElement.Diagnostics ?? new List<Diagnostic>();
                AddTokensConsumedAndDiagnosticsAttachedInContext(CodeElement.ConsumedTokens, diagnostics, Context);
                if (diagnostics.Count > 0)
                {
                    CodeElement.Diagnostics = diagnostics;
                }
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

        private void AddTokensConsumedAndDiagnosticsAttachedInContext(IList<Token> consumedTokens, IList<Diagnostic> diagnostics, ParserRuleContext context)
        {
            var ruleNodeWithDiagnostics = (ParserRuleContextWithDiagnostics)context;
            if (ruleNodeWithDiagnostics != null && ruleNodeWithDiagnostics.Diagnostics != null)
            {
                foreach (var ruleDiagnostic in ruleNodeWithDiagnostics.Diagnostics)
                {
                    diagnostics.Add(ruleDiagnostic);
                }
            }
            if (context.children != null)
            {
                foreach(var childNode in context.children)
                {
                    if(childNode is IRuleNode)
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
			program.ProgramName = CobolWordsBuilder.CreateProgramNameDefinition(context.programNameDefinition());
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
		}

		public override void EnterProgramEnd(CodeElementsParser.ProgramEndContext context) {
			var programEnd = new ProgramEnd();
			programEnd.ProgramName = CobolWordsBuilder.CreateProgramNameReference(context.programNameReference2());

			Context = context;
			CodeElement = programEnd;
		}



		 // CLASS IDENTIFICATION
		//////////////////////////

		public override void EnterClassIdentification(CodeElementsParser.ClassIdentificationContext context) {
			var classIdentification = new ClassIdentification();
			classIdentification.ClassName = CobolWordsBuilder.CreateClassNameDefinition(context.classNameDefinition());
			classIdentification.InheritsFrom = CobolWordsBuilder.CreateClassNameReference(context.inheritsFromClassName);
			classIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = classIdentification;
		}

		public override void EnterClassEnd(CodeElementsParser.ClassEndContext context) {
			var classEnd = new ClassEnd();
			classEnd.ClassName = CobolWordsBuilder.CreateClassNameReference(context.classNameReference());

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
			methodIdentification.MethodName = CobolWordsBuilder.CreateMethodNameDefinition(context.methodNameDefinition());
			methodIdentification.AuthoringProperties = CreateAuthoringProperties(context.authoringProperties());

			Context = context;
			CodeElement = methodIdentification;
		}

		public override void EnterMethodEnd(CodeElementsParser.MethodEndContext context) {
			var methodEnd = new MethodEnd();
			methodEnd.MethodName = CobolWordsBuilder.CreateMethodNameReference(context.methodNameReference());

			Context = context;
			CodeElement = methodEnd;
		}

		// --- Authoring properties common to all identification divisions ---

		internal AuthoringProperties CreateAuthoringProperties(CodeElementsParser.AuthoringPropertiesContext context) {
			var authoringProperties = new AuthoringProperties();
			if (context.authorParagraph().Length > 0) {
				var alphanumericValueContexts = context.authorParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.Author = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.dateCompiledParagraph().Length > 0) {
				var alphanumericValueContexts = context.dateCompiledParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.DateCompiled = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.dateWrittenParagraph().Length > 0) {
				var alphanumericValueContexts = context.dateWrittenParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.DateWritten = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.installationParagraph().Length > 0) {
				var alphanumericValueContexts = context.installationParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.Installation = CreateAlphanumericValues(alphanumericValueContexts);
			}
			if (context.securityParagraph().Length > 0) {
				var alphanumericValueContexts = context.securityParagraph().SelectMany(p => p.alphanumericValue6()).ToArray();
				authoringProperties.Security = CreateAlphanumericValues(alphanumericValueContexts);
			}
			return authoringProperties;
		}

		private AlphanumericValue[] CreateAlphanumericValues(CodeElementsParser.AlphanumericValue6Context[] contexts) {
			AlphanumericValue[] alphanumericValues = new AlphanumericValue[contexts.Length];
			for (int i = 0; i < contexts.Length; i++) {
				alphanumericValues[i] = CobolWordsBuilder.CreateAlphanumericValue(contexts[i]);
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
			if(context.computerName != null) {
				paragraph.ComputerName = CobolWordsBuilder.CreateAlphanumericValue(context.computerName);
			}
			if(context.DEBUGGING() != null) {
				paragraph.DebuggingMode = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.DEBUGGING()));
			}

			Context = context;
			CodeElement = paragraph;
		}

		// --- OBJECT-COMPUTER PARAGRAPH ---

		public override void EnterObjectComputerParagraph(CodeElementsParser.ObjectComputerParagraphContext context) {
			var paragraph = new ObjectComputerParagraph();
			if(context.computerName != null) {
				paragraph.ComputerName = CobolWordsBuilder.CreateAlphanumericValue(context.computerName);
			}
			if(context.memorySizeClause() != null) {
				var memorySizeClauseContext = context.memorySizeClause();
				paragraph.MemorySize = CobolWordsBuilder.CreateIntegerValue(memorySizeClauseContext.integerValue());
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
				paragraph.CollatingSequence = CobolWordsBuilder.CreateAlphabetName(collatingSeqClauseContext.alphabetName());
			}
			if(context.segmentLimitClause() != null) {
				var segmentLimitClauseContext = context.segmentLimitClause();
				paragraph.SegmentLimit = CobolWordsBuilder.CreateIntegerValue(segmentLimitClauseContext.priorityNumber().integerValue());
			}

			Context = context;
			CodeElement = paragraph;
		}

		// --- SPECIAL-NAMES PARAGRAPH ---

		public override void EnterSpecialNamesParagraph(CodeElementsParser.SpecialNamesParagraphContext context)
		{
			var paragraph = new SpecialNamesParagraph();
			
			if(context.upsiSwitchNameClause() != null && context.upsiSwitchNameClause().Length > 0)
			{
				foreach (var upsiSwitchNameContext in context.upsiSwitchNameClause())
				{
					var upsiSwitchName = CobolWordsBuilder.CreateUPSISwitchName(upsiSwitchNameContext.upsiSwitchName());
					if (upsiSwitchNameContext.mnemonicForUPSISwitchNameDefinition() != null)
					{
						var mnemonicForUPSISwitchName = CobolWordsBuilder.CreateMnemonicForUPSISwitchNameDefinition(
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
							var conditionForUPSISwitchName = CobolWordsBuilder.CreateConditionForUPSISwitchNameDefinition(
								upsiSwitchNameContext.conditionNamesForUPSISwitch().offConditionNameForUPSISwitch().conditionForUPSISwitchNameDefinition());
							paragraph.ConditionNamesForUPSISwitchStatus.Add(conditionForUPSISwitchName,
								new Tuple<ExternalName, UPSISwitchStatus>(upsiSwitchName, UPSISwitchStatus.Off));
						}
						if (upsiSwitchNameContext.conditionNamesForUPSISwitch().onConditionNameForUPSISwitch() != null)
						{
							var conditionForUPSISwitchName = CobolWordsBuilder.CreateConditionForUPSISwitchNameDefinition(
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
					var environmentName = CobolWordsBuilder.CreateEnvironmentName(
						environmentNameContext.environmentName());
					var mnemonicForEnvironmentName = CobolWordsBuilder.CreateMnemonicForEnvironmentNameDefinition(
						environmentNameContext.mnemonicForEnvironmentNameDefinition());
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
					var alphabetName = CobolWordsBuilder.CreateAlphabetNameDefinition(alphabetContext.alphabetNameDefinition());
					if(alphabetContext.intrinsicAlphabetNameReference() != null)
					{
						var intrinsicCollatingSequence = new InstrinsicCollatingSequence();
						intrinsicCollatingSequence.IntrinsicAlphabetName = CobolWordsBuilder.CreateIntrinsicAlphabetNameReference(
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
							if (userDefinedCSContext.charactersInCollatingSequence() != null)
							{
								var charsInCSContext = userDefinedCSContext.charactersInCollatingSequence();
								var characters = CreateCharactersInCollatingSequence(charsInCSContext);
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
						alphabetName = CobolWordsBuilder.CreateAlphabetNameReference(symbolicCharactersContext.alphabetNameReference());
					}
                    foreach(var symbolicCharOPContext in symbolicCharactersContext.symbolicCharactersOrdinalPositions())
                    {
                        for (int i = 0; i < Math.Min(symbolicCharOPContext.symbolicCharacterDefinition().Length, symbolicCharOPContext.ordinalPositionInCollatingSequence().Length); i++)
						{
							var symbolicCharacter = CobolWordsBuilder.CreateSymbolicCharacterDefinition(symbolicCharOPContext.symbolicCharacterDefinition()[i]);
							var ordinalPosition = CobolWordsBuilder.CreateIntegerValue(symbolicCharOPContext.ordinalPositionInCollatingSequence()[i].integerValue());
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
					var characterClassName = CobolWordsBuilder.CreateCharacterClassNameDefinition(classContext.characterClassNameDefinition());
					var userDefinedCharacterClass = new UserDefinedCollatingSequence();
					userDefinedCharacterClass.CharacterSets = new CharacterSetInCollatingSequence[classContext.userDefinedCharacterClass().Length];
					for (int i = 0; i < classContext.userDefinedCharacterClass().Length; i++)
					{
						var userDefinedCCContext = classContext.userDefinedCharacterClass()[i];
						if (userDefinedCCContext.charactersInCollatingSequence() != null)
						{
							userDefinedCharacterClass.CharacterSets[i] = CreateCharactersInCollatingSequence(
								userDefinedCCContext.charactersInCollatingSequence());
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
					var currencySign = CobolWordsBuilder.CreateAlphanumericValue(currencySignContext.alphanumericValue1());
					CharacterValue characterValue = null;
					if (currencySignContext.characterValue1() != null)
					{
						characterValue = CobolWordsBuilder.CreateCharacterValue(currencySignContext.characterValue1());
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
					var xmlSchemName = CobolWordsBuilder.CreateXmlSchemaNameDefinition(xmlSchemaContext.xmlSchemaNameDefinition());
					var assignmentName = CobolWordsBuilder.CreateAssignmentName(xmlSchemaContext.assignmentName());
					paragraph.XmlSchemaNames.Add(xmlSchemName, assignmentName);
				}
			}

			Context = context;
			CodeElement = paragraph;
		}

		private CharactersRangeInCollatingSequence CreateCharactersRange(CodeElementsParser.CharactersRangeContext context) {
			var charactersRange = new CharactersRangeInCollatingSequence();
			charactersRange.StartCharacter = CreateCharacterInCollatingSequence(context.startCharacter);
			charactersRange.EndCharacter = CreateCharacterInCollatingSequence(context.endCharacter);
			return charactersRange;
		}

		private CharactersInCollatingSequence CreateCharactersInCollatingSequence(CodeElementsParser.CharactersInCollatingSequenceContext context) {
			var chars = new CharactersInCollatingSequence();
			if (context.alphanumericValue1() != null) {
				chars.CharactersInAlphanmericValue = CobolWordsBuilder.CreateAlphanumericValue(context.alphanumericValue1());
			} else
			if (context.ordinalPositionInCollatingSequence() != null) {
				chars.OrdinalPositionInCollatingSequence = CobolWordsBuilder.CreateIntegerValue(context.ordinalPositionInCollatingSequence().integerValue());
			}
			return chars;
		}

		private CharacterInCollatingSequence CreateCharacterInCollatingSequence(CodeElementsParser.CharacterInCollatingSequenceContext context) {
			var chars = new CharacterInCollatingSequence();
			if (context.characterValue2() != null) {
				chars.CharacterValue = CobolWordsBuilder.CreateCharacterValue(context.characterValue2());
			} else
			if (context.ordinalPositionInCollatingSequence() != null) {
				chars.OrdinalPositionInCollatingSequence = CobolWordsBuilder.CreateIntegerValue(context.ordinalPositionInCollatingSequence().integerValue());
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
					var className = CobolWordsBuilder.CreateClassNameDefOrRef(c.classNameDefOrRef());
					SymbolDefinitionOrReference externalClassName = null;
					if(c.externalClassNameDefOrRef() != null) {
						externalClassName = CobolWordsBuilder.CreateExternalClassNameDefOrRef(c.externalClassNameDefOrRef());
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
				entry.FileName = CobolWordsBuilder.CreateFileNameDefinition(context.selectClause().fileNameDefinition());
				if (context.selectClause().OPTIONAL() != null)
				{
					entry.IsOptional = new SyntaxProperty<bool>(true,
						ParseTreeUtils.GetFirstToken(context.selectClause().OPTIONAL()));
				}
			}
			if (context.assignClause() != null && context.assignClause().Length > 0)
			{
				var assignClauseContext = context.assignClause()[0];
				entry.ExternalDataSet = CobolWordsBuilder.CreateAssignmentName(assignClauseContext.assignmentName()[0]);
			}
			if (context.reserveClause() != null && context.reserveClause().Length > 0)
			{
				var reserveClauseContext = context.reserveClause()[0];
				entry.ReserveIOBuffersCount = CobolWordsBuilder.CreateIntegerValue(reserveClauseContext.integerValue());
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
				entry.FileStatus = CobolExpressionsBuilder.CreateStorageArea(fileStatusClauseContext.fileStatus);
				if (fileStatusClauseContext.vsamReturnCode != null)
				{
					entry.VSAMReturnCode = CobolExpressionsBuilder.CreateStorageArea(fileStatusClauseContext.vsamReturnCode);
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
				paddingCharacter = CobolExpressionsBuilder.CreateCharacterVariable(paddingCharacterClauseContext.characterVariable());
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
				recordKey = CobolWordsBuilder.CreateDataNameReference(recordKeyClauseContext.dataNameReference());
			}
			if (context.alternateRecordKeyClause() != null && context.alternateRecordKeyClause().Length > 0)
			{
				alternateRecordKeys = new AlternateRecordKey[context.alternateRecordKeyClause().Length];
				for (int i = 0; i < context.alternateRecordKeyClause().Length; i++)
				{
					var alternateRecordKeyClauseContext = context.alternateRecordKeyClause()[i];
					alternateRecordKeys[i] = new AlternateRecordKey();
					alternateRecordKeys[i].RecordKey = CobolWordsBuilder.CreateDataNameReference(alternateRecordKeyClauseContext.recordKey);
					if (alternateRecordKeyClauseContext.DUPLICATES() != null)
					{
						alternateRecordKeys[i].AllowDuplicates = new SyntaxProperty<bool>(true,
							ParseTreeUtils.GetFirstToken(alternateRecordKeyClauseContext.DUPLICATES()));
					}
					if (alternateRecordKeyClauseContext.password != null)
					{
						alternateRecordKeys[i].Password = CobolWordsBuilder.CreateDataNameReference(alternateRecordKeyClauseContext.password);
					}
				}
			}
			if (context.relativeKeyClause() != null && context.relativeKeyClause().Length > 0)
			{
				var relativeKeyClauseContext = context.relativeKeyClause()[0];
				relativeKey = CobolWordsBuilder.CreateDataNameReference(relativeKeyClauseContext.dataNameReference());
			}
			if (context.passwordClause() != null && context.passwordClause().Length > 0)
			{
				var passwordClauseContext = context.passwordClause()[0];
				password = CobolWordsBuilder.CreateDataNameReference(passwordClauseContext.dataNameReference());
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
				rerunEntry.OnExternalDataSetOrFileName = CobolWordsBuilder.CreateAssignmentNameOrFileNameReference(
					rerunClauseContext.assignmentNameOrFileNameReference());
				if (rerunClauseContext.RECORDS() != null)
				{
					rerunEntry.CheckPointFrequency = new SyntaxProperty<CheckPointFrequency>(CheckPointFrequency.EveryRecordCount,
						ParseTreeUtils.GetFirstToken(rerunClauseContext.RECORDS()));
					rerunEntry.EveryRecordCount = CobolWordsBuilder.CreateIntegerValue(rerunClauseContext.integerValue());
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
					rerunEntry.OfFileName = CobolWordsBuilder.CreateFileNameReference(rerunClauseContext.fileNameReference());
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
					sameAreaEntry.FileNames[i] = CobolWordsBuilder.CreateFileNameReference(fileNameReferenceContext);
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
					physicalReelOfTape.FileName = CobolWordsBuilder.CreateFileNameReference(physicalReelOfTapeContext.fileNameReference());
					if (physicalReelOfTapeContext.integerValue() != null)
					{
						physicalReelOfTape.FilePosition = CobolWordsBuilder.CreateIntegerValue(physicalReelOfTapeContext.integerValue());
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
					applyWOEntry.FileNames[i] = CobolWordsBuilder.CreateFileNameReference(fileNameReferenceContext);
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

			entry.FileName = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference());

			if (context.externalClause() != null && context.externalClause().Length > 0) {
				var externalClauseContext = context.externalClause()[0];
				entry.IsExternal = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(externalClauseContext.EXTERNAL()));
			}
			if (context.globalClause() != null && context.globalClause().Length > 0) {
				var globalClauseContext = context.globalClause()[0];
				entry.IsGlobal = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(globalClauseContext.GLOBAL()));
			}
			if (context.blockContainsClause() != null && context.blockContainsClause().Length > 0) {
				var blockContainsClauseContext = context.blockContainsClause()[0];
				entry.MaxBlockSize = CobolWordsBuilder.CreateIntegerValue(blockContainsClauseContext.maxNumberOfBytes);
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
						entry.RecordSizeDependingOn = CobolWordsBuilder.CreateDataNameReference(recordClauseContext.dataNameReference());
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
						entry.LabelRecords[i] = CobolWordsBuilder.CreateDataNameReference(labelRecordClauseContext.dataNameReference()[i]);
				}
			}
			if (context.valueOfClause() != null && context.valueOfClause().Length > 0) {
				var valueOfClauseContext = context.valueOfClause()[0];
				entry.ValueOfLabelRecords = new Dictionary<SymbolReference, Variable>();
				for (int i = 0; i < valueOfClauseContext.qualifiedDataName().Length; i++) {
					entry.ValueOfLabelRecords.Add(
						CobolWordsBuilder.CreateQualifiedDataName(valueOfClauseContext.qualifiedDataName()[i]),
						CobolExpressionsBuilder.CreateVariable(valueOfClauseContext.variable5()[i]));
				}
			}
			if (context.dataRecordsClause() != null && context.dataRecordsClause().Length > 0) {
				var dataRecordClauseContext = context.dataRecordsClause()[0];
				entry.DataRecords = new SymbolReference[dataRecordClauseContext.dataNameReference().Length];
				for (int i = 0; i < dataRecordClauseContext.dataNameReference().Length; i++)
					entry.DataRecords[i] = CobolWordsBuilder.CreateDataNameReference(dataRecordClauseContext.dataNameReference()[i]);
			}
			if (context.linageClause() != null && context.linageClause().Length > 0) {
				var linageClauseContext = context.linageClause()[0];
				if (linageClauseContext.numberOfLinesInPage != null)
					entry.LogicalPageBodyLineCount = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInPage);
				if (linageClauseContext.firstLineNumberOfFootingArea != null)
					entry.LogicalPageFootingLineNumber = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.firstLineNumberOfFootingArea);
				if (linageClauseContext.numberOfLinesInTopMargin != null)
					entry.LogicalPageTopMarginLineCount = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInTopMargin);
				if (linageClauseContext.numberOfLinesInBottomMargin != null)
					entry.LogicalPageBottomMarginLineCount = CobolExpressionsBuilder.CreateIntegerVariable(linageClauseContext.numberOfLinesInBottomMargin);
			}
			if (context.recordingModeClause() != null && context.recordingModeClause().Length > 0) {
				var recordingModeClauseContext = context.recordingModeClause()[0];
				entry.RecordingMode = CobolWordsBuilder.CreateRecordingMode(recordingModeClauseContext.recordingMode());
			}

			Context = context;
			CodeElement = entry;
		}

		public override void EnterDataDescriptionEntry(CodeElementsParser.DataDescriptionEntryContext context) {
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
                typedef.DataTypeName = CobolWordsBuilder.CreateDataTypeNameDefinition(context.dataNameDefinition());
                var strong = context.cobol2002TypedefClause().STRONG();
                var strict = context.cobol2002TypedefClause().STRICT();
           
                typedef.Strong = new SyntaxProperty<bool>(strong != null, ParseTreeUtils.GetFirstToken(strong));
                typedef.Strict = new SyntaxProperty<bool>(strict != null, ParseTreeUtils.GetFirstToken(strict));
                //TCTYPE_DEFAULT_ACCESS_MODIFIER  rule is respected here. 
                //By default a TYPE is private even if PRIVATE keyword is not given. 
                //If PUBLIC keyword is set, the TYPE as to be set PUBLIC.  
                typedef.Visibility = context.cobol2002TypedefClause().PUBLIC() != null ? AccessModifier.Public : AccessModifier.Private;

                var restrictionLevel = typedef.Strong.Value ? RestrictionLevel.STRONG 
                                        : typedef.Strict.Value ? RestrictionLevel.STRICT 
                                        : RestrictionLevel.WEAK;
                entry = typedef;
                entry.DataName = typedef.DataTypeName;
                entry.DataType = new DataType(typedef.DataTypeName.Name, restrictionLevel, CobolLanguageLevel.Cobol2002);               
            }
// [/COBOL 2002]
            else {               
                entry = new DataDescriptionEntry();
                entry.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
                entry.DataType = DataType.Unknown;
            }
            

			if (context.externalClause() != null && context.externalClause().Length > 0) {
				entry.External = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.externalClause()[0].EXTERNAL()));
			}

            EnterCommonDataDescriptionAndDataRedefines(entry, context);

            Context = context;
			CodeElement = entry;
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
			entry.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
			
			if (context.redefinesClause() != null) {
				entry.RedefinesDataName = CobolWordsBuilder.CreateDataNameReference(context.redefinesClause().dataNameReference());
			}

            EnterCommonDataDescriptionAndDataRedefines(entry, context);

            Context = context;
			CodeElement = entry;
		}

	    private void EnterCommonDataDescriptionAndDataRedefines(CommonDataDescriptionAndDataRedefines entry, CodeElementsParser.DataDescriptionEntryContext context) {
            if (context.levelNumber != null)
                entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber);
            if (context.FILLER() != null) entry.Filler = new SyntaxProperty<bool>(true, ParseTreeUtils.GetFirstToken(context.FILLER()));
            else entry.Filler = new SyntaxProperty<bool>(entry.DataName == null, null);

            if (context.pictureClause() != null && context.pictureClause().Length > 0)
            {
                var pictureClauseContext = context.pictureClause()[0];
                entry.Picture = CobolWordsBuilder.CreateAlphanumericValue(pictureClauseContext.pictureCharacterString);
            }

// [COBOL 2002]
            //Variable declared with a Type
            if (context.cobol2002TypeClause() != null && context.cobol2002TypeClause().Length > 0) {
                entry.UserDefinedDataType = CobolWordsBuilder.CreateQualifiedDataTypeReference(context.cobol2002TypeClause()[0]);
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
                entry.IsJustified = new SyntaxProperty<bool>(true,
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
                    entry.OccursDependingOn = CobolExpressionsBuilder.CreateNumericVariable(occursClauseContext.varNumberOfOccurences);
                }
                if (occursClauseContext.tableSortingKeys() != null && occursClauseContext.tableSortingKeys().Length > 0)
                {
                    int keysCount = 0;
                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
                    {
                        keysCount += tableSortingKeysContext.dataNameReference().Length;
                    }
                    entry.TableSortingKeys = new TableSortingKey[keysCount];
                    int keyIndex = 0;
                    foreach (var tableSortingKeysContext in occursClauseContext.tableSortingKeys())
                    {
                        SyntaxProperty<SortDirection> sortDirection = null;
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
                            SymbolReference sortKey = CobolWordsBuilder.CreateDataNameReference(dataNameReference);
                            entry.TableSortingKeys[keyIndex] = new TableSortingKey(sortKey, sortDirection);
                            keyIndex++;
                        }
                    }
                }
                if (occursClauseContext.indexNameDefinition() != null && occursClauseContext.indexNameDefinition().Length > 0)
                {
                    entry.Indexes = new SymbolDefinition[occursClauseContext.indexNameDefinition().Length];
                    for (int i = 0; i < occursClauseContext.indexNameDefinition().Length; i++)
                    {
                        var indexNameDefinition = occursClauseContext.indexNameDefinition()[i];
                        entry.Indexes[i] = CobolWordsBuilder.CreateIndexNameDefinition(indexNameDefinition);
                    }
                }
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
                if (synchronizedClauseContext.SYNCHRONIZED() != null)
                {
                    entry.IsSynchronized = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNCHRONIZED()));
                }
                else
                {
                    entry.IsSynchronized = new SyntaxProperty<bool>(true,
                        ParseTreeUtils.GetFirstToken(synchronizedClauseContext.SYNC()));
                }
            }
            if (context.usageClause() != null && context.usageClause().Length > 0)
            {
                entry.Usage = CreateUsageClause(context.usageClause()[0]);
            }
            if (context.valueClause() != null && context.valueClause().Length > 0)
            {
                var valueClauseContext = context.valueClause()[0];
                entry.InitialValue = CobolWordsBuilder.CreateValue(valueClauseContext.value2());
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
			entry.DataName = CobolWordsBuilder.CreateDataNameDefinition(context.dataNameDefinition());
			if (context.renamesClause().qualifiedDataName() != null) {
				entry.RenamesFromDataName = CobolWordsBuilder.CreateQualifiedDataName(context.renamesClause().qualifiedDataName());
			} else
			if (context.renamesClause().dataNamesRange() != null) {
				entry.RenamesFromDataName = CobolWordsBuilder.CreateQualifiedDataName(context.renamesClause().dataNamesRange().startDataName);
				entry.RenamesToDataName = CobolWordsBuilder.CreateQualifiedDataName(context.renamesClause().dataNamesRange().endDataName);
			}

			Context = context;
			CodeElement = entry;
		}

		public override void EnterDataConditionEntry(CodeElementsParser.DataConditionEntryContext context) {
			var entry = new DataConditionEntry();
			entry.LevelNumber = CobolWordsBuilder.CreateIntegerValue(context.levelNumber);
			entry.DataName = CobolWordsBuilder.CreateConditionNameDefinition(context.conditionNameDefinition());
			SetConditionValues(entry, context.valueClauseForCondition());

			Context = context;
			CodeElement = entry;
		}

		private void SetConditionValues(DataConditionEntry entry,CodeElementsParser.ValueClauseForConditionContext context) {
			if (context == null) return;
			if (context.value1() != null && context.value1().Length > 0) {
				entry.ConditionValues = new Value[context.value1().Length];
				for (int i = 0; i < context.value1().Length; i++)
					entry.ConditionValues[i] = CobolWordsBuilder.CreateValue(context.value1()[i]);
			}
			if (context.valuesRange() != null && context.valuesRange().Length > 0) {
				entry.ConditionValuesRanges = new ValuesRange[context.valuesRange().Length];
				for (int i = 0; i < context.valuesRange().Length; i++) {
					var valuesRangeContext = context.valuesRange()[i];
					entry.ConditionValuesRanges[i] = new ValuesRange(
						CobolWordsBuilder.CreateValue(valuesRangeContext.startValue),
						CobolWordsBuilder.CreateValue(valuesRangeContext.endValue));
				}
			}
		}

		  ////////////////////////
		 // PROCEDURE DIVISION //
		////////////////////////

		public override void EnterProcedureDivisionHeader(CodeElementsParser.ProcedureDivisionHeaderContext context) {
            Context = context;
            CodeElement = new ProcedureDivisionHeader();
		}
		public override void EnterUsingPhrase(CodeElementsParser.UsingPhraseContext context) {
			var inputs = CobolStatementsBuilder.CreateInputParameters(context.programInputParameters());
			((ProcedureDivisionHeader)CodeElement).UsingParameters = inputs;
		}
		public override void EnterReturningPhrase(CodeElementsParser.ReturningPhraseContext context) {
			var receiving = CobolExpressionsBuilder.CreateSharedStorageArea(context.programOutputParameter().sharedStorageArea2());
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
					statement.ProcedureNames[i] = CobolWordsBuilder.CreateProcedureName(context.procedureName()[i]);
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
					statement.FileNames[i] = CobolWordsBuilder.CreateFileNameReference(context.fileNameReference()[i]);
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

            sectionHeader.SectionName = CobolWordsBuilder.CreateSectionNameDefinition(context.sectionNameDefinition());
            if (context.priorityNumber() != null)
            {
                sectionHeader.PriorityNumber = CobolWordsBuilder.CreateIntegerValue(context.priorityNumber().integerValue());
            }

            Context = context;
            CodeElement = sectionHeader;
        }

        // -- Paragraph --

        public override void EnterParagraphHeader(CodeElementsParser.ParagraphHeaderContext context)
        {
            var paragraphHeader = new ParagraphHeader();

            paragraphHeader.ParagraphName = CobolWordsBuilder.CreateParagraphNameDefinition(context.paragraphNameDefinition());

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
			if(context.addSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateAddStatement(context.addSimple());
			} else
			if (context.addGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateAddGivingStatement(context.addGiving());
			} else
			if (context.addCorresponding() != null) {
				CodeElement = CobolStatementsBuilder.CreateAddCorrespondingStatement(context.addCorresponding());
			} 
            else
            {
                CodeElement = new AddSimpleStatement();
            }
                
		}
		public override void EnterAddStatementEnd(CodeElementsParser.AddStatementEndContext context) {
			Context = context;
			CodeElement = new AddStatementEnd();
		}

		public override void EnterComputeStatement(CodeElementsParser.ComputeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateComputeStatement(context);
		}
		public override void EnterComputeStatementEnd(CodeElementsParser.ComputeStatementEndContext context) {
			Context = context;
			CodeElement = new ComputeStatementEnd();
		}

		public override void EnterDivideStatement(CodeElementsParser.DivideStatementContext context) {
			Context = context;
			if (context.divideSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateDivideStatement(context.divideSimple());
			} else
			if (context.divideGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateDivideGivingStatement(context.divideGiving());
			} else
			if (context.divideRemainder() != null) {
				CodeElement = CobolStatementsBuilder.CreateDivideRemainderStatement(context.divideRemainder());
			}
		}
		public override void EnterDivideStatementEnd(CodeElementsParser.DivideStatementEndContext context) {
			Context = context;
			CodeElement = new DivideStatementEnd();
		}

		public override void EnterMultiplyStatement(CodeElementsParser.MultiplyStatementContext context) {
			Context = context;
			if (context.multiplySimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateMultiplyStatement(context.multiplySimple());
			} else
			if (context.multiplyGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateMultiplyGivingStatement(context.multiplyGiving());
			}
		}
		public override void EnterMultiplyStatementEnd(CodeElementsParser.MultiplyStatementEndContext context) {
			Context = context;
			CodeElement = new MultiplyStatementEnd();
		}

		public override void EnterSubtractStatement(CodeElementsParser.SubtractStatementContext context) {
			Context = context;
			if (context.subtractSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateSubtractStatement(context.subtractSimple());
			}
			else
			if (context.subtractGiving() != null) {
				CodeElement = CobolStatementsBuilder.CreateSubtractGivingStatement(context.subtractGiving());
			} else
			if (context.subtractCorresponding() != null) {
				CodeElement = CobolStatementsBuilder.CreateSubtractCorrespondingStatement(context.subtractCorresponding());
			}
		}
		public override void EnterSubtractStatementEnd(CodeElementsParser.SubtractStatementEndContext context) {
			Context = context;
			CodeElement = new SubtractStatementEnd();
		}



		// --- FILE STATEMENTS ---
		
		public override void EnterOpenStatement(CodeElementsParser.OpenStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateOpenStatement(context);
		}

		public override void EnterCloseStatement(CodeElementsParser.CloseStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateCloseStatement(context);
		}

		public override void EnterReadStatement(CodeElementsParser.ReadStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateReadStatement(context);
		}

		public override void EnterReadStatementEnd(CodeElementsParser.ReadStatementEndContext context) {
			Context = context;
			CodeElement = new ReadStatementEnd();
		}

		public override void EnterWriteStatement(CodeElementsParser.WriteStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateWriteStatement(context);
		}

		public override void EnterWriteStatementEnd(CodeElementsParser.WriteStatementEndContext context) {
			Context = context;
			CodeElement = new WriteStatementEnd();
		}
		
		public override void EnterRewriteStatement(CodeElementsParser.RewriteStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateRewriteStatement(context);
		}

		public override void EnterRewriteStatementEnd(CodeElementsParser.RewriteStatementEndContext context) {
			Context = context;
			CodeElement = new RewriteStatementEnd();
		}



		// --- ACCEPT ---

		public override void EnterAcceptStatement(CodeElementsParser.AcceptStatementContext context) {
			Context = context;
			if (context.acceptDataTransfer() != null) {
				CodeElement = CobolStatementsBuilder.CreateAcceptDataTransferStatement(context.acceptDataTransfer());
			} else
			if(context.acceptSystemDateTime() != null) {
				CodeElement = CobolStatementsBuilder.CreateAcceptSystemDateTime(context.acceptSystemDateTime());
			}
		}

		// --- ALTER ---

		public override void EnterAlterStatement(CodeElementsParser.AlterStatementContext context) {            
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateAlterStatement(context);
		}

        // --- CALL ---
        /// <summary>
        /// Generic and default CALL statement
        /// </summary>
        /// <param name="context"></param>
        public override void EnterCallStatement([NotNull] CodeElementsParser.CallStatementContext context)
        {
            Context = context;
            if (context.cobolCallStatement() != null)
            {
                CodeElement = CobolStatementsBuilder.CreateCallStatement(context.cobolCallStatement());
            }
            else if (context.tcCallStatement() != null)
            {
                //Let TypeCobolCodeElementBuilder do the work
            }
            else
            {
                CodeElement = new CallStatement();
            }
        }

		public override void EnterCallStatementEnd(CodeElementsParser.CallStatementEndContext context) {
			Context = context;
			CodeElement = new CallStatementEnd();
		}

		// --- CANCEL ---

		public override void EnterCancelStatement(CodeElementsParser.CancelStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateCancelStatement(context);
		}

		// --- CONTINUE ---

		public override void EnterContinueStatement(CodeElementsParser.ContinueStatementContext context) {
			Context = context;
			CodeElement = new ContinueStatement();
		}

		// --- DELETE ---

		public override void EnterDeleteStatement(CodeElementsParser.DeleteStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateDeleteStatement(context);
		}

		public override void EnterDeleteStatementEnd(CodeElementsParser.DeleteStatementEndContext context) {
			Context = context;
			CodeElement = new DeleteStatementEnd();
		}

		// --- DISPLAY ---

		public override void EnterDisplayStatement(CodeElementsParser.DisplayStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateDisplayStatement(context);
		}

		// --- ENTRY ---
		
		public override void EnterEntryStatement(CodeElementsParser.EntryStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateEntryStatement(context);
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
			CodeElement = CobolStatementsBuilder.CreateEvaluateStatement(context); ;
		}

		public override void EnterEvaluateStatementEnd(CodeElementsParser.EvaluateStatementEndContext context) {
			Context = context;
			CodeElement = new EvaluateStatementEnd();
		}

		// --- EXEC ---

		public override void EnterExecStatement(CodeElementsParser.ExecStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateExecStatement(context);
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

		// --- GOBACK ---
				
		public override void EnterGobackStatement(CodeElementsParser.GobackStatementContext context) {
			Context = context;
			CodeElement = new GobackStatement();
		}

		// --- GOTO ---

		public override void EnterGotoStatement(CodeElementsParser.GotoStatementContext context) {
			Context = context;
			if (context.gotoSimple() != null) {
				CodeElement = CobolStatementsBuilder.CreateGotoStatement(context.gotoSimple());
			}
			if (context.gotoConditional() != null) {
				CodeElement = CobolStatementsBuilder.CreateGotoConditionalStatement(context.gotoConditional());
			}
		}



		// --- IF ---

		public override void EnterIfStatement(CodeElementsParser.IfStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateIfStatement(context);
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
			CodeElement = CobolStatementsBuilder.CreateInitializeStatement(context);
		}

		// --- INSPECT ---

		public override void EnterInspectStatement(CodeElementsParser.InspectStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateInspectStatement(context);
		}

		// --- INVOKE ---

		public override void EnterInvokeStatement(CodeElementsParser.InvokeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateInvokeStatement(context);
		}

		// --- MERGE ---

		public override void EnterMergeStatement(CodeElementsParser.MergeStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateMergeStatement(context);
		}



		// --- PERFORM ---

		public override void EnterPerformStatement(CodeElementsParser.PerformStatementContext context) {
			Context = context;
            if (context.procedureName() != null || context.proceduresRange() != null)
            {
                CodeElement = CobolStatementsBuilder.CreatePerformProcedureStatement(context);
            }
            else
            {
                CodeElement = CobolStatementsBuilder.CreatePerformStatement(context);
            }
		}

		public override void EnterPerformStatementEnd(CodeElementsParser.PerformStatementEndContext context) {
			Context = context;
			CodeElement = new PerformStatementEnd();
		}



		// --- RELEASE	 ---

		public override void EnterReleaseStatement(CodeElementsParser.ReleaseStatementContext context) {            
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateReleaseStatement(context);
		}

		// --- RETURN ---

		public override void EnterReturnStatement(CodeElementsParser.ReturnStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateReturnStatement(context);
		}
		public override void EnterReturnStatementEnd(CodeElementsParser.ReturnStatementEndContext context) {
			Context = context;
			CodeElement = new ReturnStatementEnd();
		}

		// --- SEARCH ---

		public override void EnterSearchStatement(CodeElementsParser.SearchStatementContext context) {
			Context = context;
			if (context.serialSearch() != null) {
				CodeElement = CobolStatementsBuilder.CreateSerialSearchStatement(context.serialSearch());
			} else
			if (context.binarySearch() != null) {
				CodeElement = CobolStatementsBuilder.CreateBinarySearchStatement(context.binarySearch());
			}
		}
		public override void EnterSearchStatementEnd(CodeElementsParser.SearchStatementEndContext context) {
			Context = context;
			CodeElement = new SearchStatementEnd();
		}

		// --- SORT ---

		public override void EnterSortStatement(CodeElementsParser.SortStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateSortStatement(context);
		}

		// --- START ---

		public override void EnterStartStatement(CodeElementsParser.StartStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateStartStatement(context);
		}
		public override void EnterStartStatementEnd(CodeElementsParser.StartStatementEndContext context) {
			Context = context;
			CodeElement = new StartStatementEnd();
		}

		// --- STOP ---

		public override void EnterStopStatement(CodeElementsParser.StopStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateStopStatement(context);
		}

		// --- STRING ---

		public override void EnterStringStatement(CodeElementsParser.StringStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateStringStatement(context);
		}
		public override void EnterStringStatementEnd(CodeElementsParser.StringStatementEndContext context) {
			Context = context;
			CodeElement = new StringStatementEnd();
		}

		// --- UNSTRING ---

		public override void EnterUnstringStatement(CodeElementsParser.UnstringStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateUnstringStatement(context);
		}
		public override void EnterUnstringStatementEnd(CodeElementsParser.UnstringStatementEndContext context) {
			Context = context;
			CodeElement = new UnstringStatementEnd();
		}

		// --- XML STATEMENTS ---

		public override void EnterXmlGenerateStatement(CodeElementsParser.XmlGenerateStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateXmlGenerateStatement(context);
		}

		public override void EnterXmlParseStatement(CodeElementsParser.XmlParseStatementContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateXmlParseStatement(context);
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
                CodeElement = CobolStatementsBuilder.CreateMoveStatement(context.moveSimple());
            }
	        else if (context.moveCorresponding() != null)
	        {
                CodeElement = CobolStatementsBuilder.CreateMoveStatement(context.moveCorresponding());
            }
            else 
                CodeElement = new MoveSimpleStatement(null, null, null);
	    }

		// --- SET STATEMENT ---

		public override void EnterSetStatement(CodeElementsParser.SetStatementContext context) {
			if (context.setStatementForAssignment() != null) {
				Context = context.setStatementForAssignment();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForAssignment(context.setStatementForAssignment());
			} else
			if (context.setStatementForIndexes() != null) {
				Context = context.setStatementForIndexes();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForIndexes(context.setStatementForIndexes());
			} else
			if (context.setStatementForSwitches() != null) {
				Context = context.setStatementForSwitches();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForSwitches(context.setStatementForSwitches());
			} else
			if (context.setStatementForConditions() != null) {
				Context = context.setStatementForConditions();
				CodeElement = CobolStatementsBuilder.CreateSetStatementForConditions(context.setStatementForConditions());
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
			CodeElement = CobolStatementsBuilder.CreateWhenCondition(context);
		}
		public override void EnterWhenOtherCondition(CodeElementsParser.WhenOtherConditionContext context) {
			Context = context;
			CodeElement = new WhenOtherCondition();
		}

		public override void EnterWhenSearchCondition(CodeElementsParser.WhenSearchConditionContext context) {
			Context = context;
			CodeElement = CobolStatementsBuilder.CreateWhenSearchCondition(context);
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


/*
        // -- Symbols --

        // ** Program names and Program entries **

        public override void EnterProgramNameDefinition(CodeElementsParser.ProgramNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramEntryDefinition(CodeElementsParser.ProgramEntryDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ProgramEntry);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReference1(CodeElementsParser.ProgramNameReference1Context context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReference2(CodeElementsParser.ProgramNameReference2Context context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ProgramName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterProgramNameReferenceOrProgramEntryReference(CodeElementsParser.ProgramNameReferenceOrProgramEntryReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.ProgramName, SymbolType.ProgramEntry };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // + runtime references
        // => programNameFromDataOrProgramEntryFromData
        // => programNameFromDataOrProgramEntryFromDataOrProcedurePointerOrFunctionPointer

        // ** Section names and Paragraph names **

        public override void EnterSectionNameDefinition(CodeElementsParser.SectionNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.SectionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameDefinition(CodeElementsParser.ParagraphNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ParagraphName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSectionNameReference(CodeElementsParser.SectionNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.SectionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterParagraphNameReference(CodeElementsParser.ParagraphNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ParagraphName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void ExitQualifiedParagraphNameReference(CodeElementsParser.QualifiedParagraphNameReferenceContext context)
        {
            if (context.sectionNameReference() != null)
            {
                Token qualifiedToken = ParseTreeUtils.GetFirstToken(context.paragraphNameReference());
                Token[] qualifierTokens = new Token[] { ParseTreeUtils.GetFirstToken(context.sectionNameReference()) };
                UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
            }
        }

        // ** Class names and Method names **

        public override void EnterClassNameDefinition(CodeElementsParser.ClassNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMethodNameDefinition(CodeElementsParser.MethodNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MethodName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterClassNameReference(CodeElementsParser.ClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterClassNameDefOrRef(CodeElementsParser.ClassNameDefOrRefContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinitionOrReference, SymbolType.ClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.ClassName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
        
        public override void EnterExternalClassNameDefOrRef(CodeElementsParser.ExternalClassNameDefOrRefContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinitionOrReference, SymbolType.ExternalClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMethodNameReference(CodeElementsParser.MethodNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MethodName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // + runtime references 
        // => methodNameFromData

        // ** Environment names, UPSI switch names and associated Mnemonics, Condition  **

        public override void EnterEnvironmentName(CodeElementsParser.EnvironmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.EnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterUpsiSwitchName(CodeElementsParser.UpsiSwitchNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.UPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameDefinition(CodeElementsParser.MnemonicForEnvironmentNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MnemonicForEnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForUPSISwitchNameDefinition(CodeElementsParser.MnemonicForUPSISwitchNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.MnemonicForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterConditionForUPSISwitchNameDefinition(CodeElementsParser.ConditionForUPSISwitchNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ConditionForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameReference(CodeElementsParser.MnemonicForEnvironmentNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MnemonicForEnvironmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForEnvironmentNameReferenceOrEnvironmentName(CodeElementsParser.MnemonicForEnvironmentNameReferenceOrEnvironmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.MnemonicForEnvironmentName, SymbolType.EnvironmentName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterMnemonicForUPSISwitchNameReference(CodeElementsParser.MnemonicForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.MnemonicForUPSISwitchName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.FileName, SymbolType.MnemonicForUPSISwitchName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Character sets **

        public override void EnterSymbolicCharacterDefinition(CodeElementsParser.SymbolicCharacterDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.SymbolicCharacter);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSymbolicCharacterReference(CodeElementsParser.SymbolicCharacterReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.SymbolicCharacter);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAlphabetNameDefinition(CodeElementsParser.AlphabetNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.AlphabetName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAlphabetNameReference(CodeElementsParser.AlphabetNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.AlphabetName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterCharacterClassNameDefinition(CodeElementsParser.CharacterClassNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.CharacterClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterCharacterClassNameReference(CodeElementsParser.CharacterClassNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.CharacterClassName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Data item, Data conditions and Indexes **

        public override void EnterDataNameDefinition(CodeElementsParser.DataNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReference(CodeElementsParser.DataNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterSpecialRegister(CodeElementsParser.SpecialRegisterContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.DataName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // Data name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)
        
        // Data name reference already handled above :
        // => public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        
        public override void EnterConditionNameDefinition(CodeElementsParser.ConditionNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.ConditionName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // Condition name reference already handled above :
        // => public override void EnterConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.ConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)

        // Condition name reference already handled above :
        // => public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceContext context)
        
		public override void ExitQualifiedDataName(CodeElementsParser.QualifiedDataNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReference(),
					legacy.dataNameReferenceOrFileNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReference(),
					context.qDataOrFile());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrIndexName(CodeElementsParser.QualifiedDataNameOrIndexNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrIndexName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrIndexNameReference(),
					legacy.dataNameReferenceOrFileNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrIndexNameReference(),
					context.qDataOrFile());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedConditionName(CodeElementsParser.QualifiedConditionNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedConditionName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.conditionNameReferenceOrConditionForUPSISwitchNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.conditionNameReferenceOrConditionForUPSISwitchNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrConditionName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrIndexName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrIndexNameContext context) {
			Token qualifiedToken = null;
			Token[] qualifierTokens = null;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrFileName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrFileNameContext context) {
			Token qualifiedToken;
			Token[] qualifierTokens;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrFileName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}
		public override void ExitQualifiedDataNameOrQualifiedConditionNameOrClassName(CodeElementsParser.QualifiedDataNameOrQualifiedConditionNameOrClassNameContext context) {
			Token qualifiedToken;
			Token[] qualifierTokens;
			var legacy = context.legacyQualifiedDataNameOrQualifiedConditionNameOrClassName();
			if (legacy != null) {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					legacy.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(),
					legacy.dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference());
			} else {
				GetTokensForSymbolInformation(out qualifiedToken, out qualifierTokens,
					context.dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference(),
					context.qDataOrFileOrUPSI());
			}
			UpdateSymbolInformationForQualifiedNames(qualifiedToken, qualifierTokens);
		}

		private void GetTokensForSymbolInformation(out Token qualifiedToken, out Token[] qualifierTokens, IParseTree node, IParseTree[] nodes) {
			if (node != null)  qualifiedToken  = ParseTreeUtils.GetFirstToken(node);
			else qualifiedToken = null;
			if (nodes != null) qualifierTokens = nodes.Select(ctx => ParseTreeUtils.GetFirstToken(ctx)).ToArray();
			else qualifierTokens = null;
		}

		private void UpdateSymbolInformationForQualifiedNames(Token qualifiedToken, Token[] qualifierTokens) {
			if (qualifiedToken != null) {
				SymbolInformation qualifiedSymbolInfo = CodeElement.SymbolInformationForTokens[qualifiedToken];
				qualifiedSymbolInfo.QualifedBy = qualifierTokens;
			}
			if (qualifierTokens != null)
			foreach (var qualifierToken in qualifierTokens) {
				SymbolInformation qualifierSymbolInfo = CodeElement.SymbolInformationForTokens[qualifierToken];
				qualifierSymbolInfo.QualifierFor = qualifiedToken;
			}
		}

        public override void EnterIndexNameDefinition(CodeElementsParser.IndexNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.IndexName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterIndexNameReference(CodeElementsParser.IndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.IndexName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrIndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.IndexName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.IndexName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
               
        // ** Files **

        public override void EnterFileNameDefinition(CodeElementsParser.FileNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.FileName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterFileNameReference(CodeElementsParser.FileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.FileName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // File name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceContext context)

        // File name reference already handled above :
        // => public override void EnterDataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference(CodeElementsParser.DataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReferenceContext context)

        public override void EnterDataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference(CodeElementsParser.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.DataName, SymbolType.ConditionName, SymbolType.ConditionForUPSISwitchName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterXmlSchemaNameDefinition(CodeElementsParser.XmlSchemaNameDefinitionContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolDefinition, SymbolType.XmlSchemaName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterXmlSchemaNameReference(CodeElementsParser.XmlSchemaNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, SymbolType.XmlSchemaName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAssignmentName(CodeElementsParser.AssignmentNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.AssignmentName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterAssignmentNameOrFileNameReference(CodeElementsParser.AssignmentNameOrFileNameReferenceContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolType[] candidateSymbolTypes = new SymbolType[] { SymbolType.AssignmentName, SymbolType.FileName };
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.SymbolReference, candidateSymbolTypes);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        // ** Runtime functions **

		public override void EnterIntrinsicFunctionName(CodeElementsParser.IntrinsicFunctionNameContext context) {
			ITerminalNode node = null;
			if (context.UserDefinedWord() != null) node = context.UserDefinedWord();
			else
			if (context.LENGTH() != null) node = context.LENGTH();
			else
			if (context.RANDOM() != null) node = context.RANDOM();
			else
			if (context.WHEN_COMPILED() != null) node = context.WHEN_COMPILED();
			Token symbolToken = ParseTreeUtils.GetFirstToken(node);
			SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.TCFunctionName);
			CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }

        public override void EnterExecTranslatorName(CodeElementsParser.ExecTranslatorNameContext context)
        {
            Token symbolToken = ParseTreeUtils.GetFirstToken(context);
            SymbolInformation symbolInfo = new SymbolInformation(symbolToken, SymbolRole.ExternalName, SymbolType.ExecTranslatorName);
            CodeElement.SymbolInformationForTokens[symbolToken] = symbolInfo;
        }
*/

    }
}
