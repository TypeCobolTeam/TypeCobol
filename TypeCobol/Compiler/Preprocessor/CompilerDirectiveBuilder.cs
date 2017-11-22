using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor.Generated;
using TypeCobol.Compiler.Scanner;
using Antlr4.Runtime.Misc;
using TypeCobol.Tools;
using Analytics;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Build a CompilerDirective object while visiting its parse tree
    /// </summary>
    internal class CompilerDirectiveBuilder : CobolCompilerDirectivesBaseListener
    {

        public CompilerDirectiveBuilder(TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            TypeCobolOptions = compilerOptions;
            CopyTextNameVariations = copyTextNameVariations;
        }

        public TypeCobolOptions TypeCobolOptions { get; set; }
        public List<RemarksDirective.TextNameVariation> CopyTextNameVariations { get; set; }
        /// <summary>
        /// CompilerDirective object resulting of the visit the parse tree
        /// </summary>
        public CompilerDirective CompilerDirective { get; private set; }
        
        // --- Visiting the tree ---

        public override void EnterCompilerDirectingStatement(CobolCompilerDirectivesParser.CompilerDirectingStatementContext context)
        {
            CompilerDirective = null;
        }

        public override void ExitCompilerDirectingStatement([Antlr4.Runtime.Misc.NotNull] CobolCompilerDirectivesParser.CompilerDirectingStatementContext context)
        {
            if (CompilerDirective != null)
            {
                // Collect all error messages encoutered while parsing this compiler directive
                AddDiagnosticsAttachedInContext(context);
            }
        }

        private void AddDiagnosticsAttachedInContext(ParserRuleContext context)
        {
            var ruleNodeWithDiagnostics = (ParserRuleContextWithDiagnostics)context;
            if (ruleNodeWithDiagnostics.Diagnostics != null)
            {
                foreach (var ruleDiagnostic in ruleNodeWithDiagnostics.Diagnostics)
                {
                    CompilerDirective.AddDiagnostic(ruleDiagnostic);
                }
            }
            if (context.children != null)
            {
                foreach (var childNode in context.children)
                {
                    if (childNode is IRuleNode)
                    {                        
                        AddDiagnosticsAttachedInContext((ParserRuleContext)((IRuleNode)childNode).RuleContext);
                    }
                }
            }
        }

        public override void EnterBasisCompilerStatement(CobolCompilerDirectivesParser.BasisCompilerStatementContext context) 
        {
            var basisDirective = new BasisDirective();
            CompilerDirective = basisDirective;

            if (context.textName() != null)
            {
                basisDirective.BasisName = GetTextName(context.textName());
                basisDirective.TextNameSymbol = ParseTreeUtils.GetFirstToken(context.textName());
            }
        }
        
        public override void EnterControlCblCompilerStatement(CobolCompilerDirectivesParser.ControlCblCompilerStatementContext context) 
        {
            CompilerDirective = new ControlCblDirective(context.ASTERISK_CONTROL() != null ? CompilerDirectiveType.ASTERISK_CONTROL : CompilerDirectiveType.ASTERISK_CBL);
        }

        public override void EnterControlCblOption(CobolCompilerDirectivesParser.ControlCblOptionContext context) 
        {
            string option = null;
            TryGetUserDefinedWord(context.enumeratedValue1().UserDefinedWord(), ref option);
            if (option != null)
            {
                ControlCblDirective.ControlCblOption optionValue;
                if(Enum.TryParse<ControlCblDirective.ControlCblOption>(option, out optionValue))
                {
                    ((ControlCblDirective)CompilerDirective).OptionsList.Add(optionValue);
                }
                else
                {
                    Token errorToken = ParseTreeUtils.GetTokenFromTerminalNode(context.enumeratedValue1().UserDefinedWord());
                    Diagnostic diag = new Diagnostic(
                        MessageCode.InvalidControlCblCompilerStatementOption, 
                        errorToken.Column, errorToken.EndColumn,
                        errorToken.Line, option);
                    CompilerDirective.AddDiagnostic(diag);
                }
            }
        }
        
		public override void EnterCopyCompilerStatement(CobolCompilerDirectivesParser.CopyCompilerStatementContext context) {
			CompilerDirective = new CopyDirective(CompilerDirectiveType.COPY, ParseTreeUtils.GetFirstToken(context.COPY()));

          
        }

		private string GetTextName(CobolCompilerDirectivesParser.TextNameContext context) {
			if (context == null) return null;
			return GetName(context.externalName5().alphanumericValue5());
        }
		private string GetLibraryName(CobolCompilerDirectivesParser.LibraryNameContext context) {
			if (context == null) return null;
			return GetName(context.externalName5().alphanumericValue5());
        }
		private string GetName(CobolCompilerDirectivesParser.AlphanumericValue5Context context) {
			if (context == null) return null;
			string result = null;
			TryGetAlphanumericLiteralValue(context.alphanumericLiteralToken(), ref result);
			TryGetUserDefinedWord(context.UserDefinedWord(), ref result);
			if (result != null) result = result.Trim('\'').Trim('\"');
			return result;
		}
		internal static void TryGetAlphanumericLiteralValue(Preprocessor.Generated.CobolCompilerDirectivesParser.AlphanumericLiteralTokenContext context, ref string property) {
			if (context == null) return;
			TryGetUserDefinedWord(context.AlphanumericLiteral(), ref property);
			TryGetUserDefinedWord(context.HexadecimalAlphanumericLiteral(), ref property);
			TryGetUserDefinedWord(context.NullTerminatedAlphanumericLiteral(), ref property);
		}
		public static void TryGetUserDefinedWord(ITerminalNode node, ref string property) {
			if (node != null && property == null) property = node.GetText();
		}

        public override void EnterCopyCompilerStatementBody(
            CobolCompilerDirectivesParser.CopyCompilerStatementBodyContext context)
        {

            var copy = (CopyDirective) CompilerDirective;

            var copyParentContext = context.parent as CobolCompilerDirectivesParser.CopyCompilerStatementContext; //Get the parent context as CopyCompilerStatement. 
            //If null it means the parent is certainly CobolCompilerDirectivesParser.ExecSqlIncludeStatementContext 
            //If not null we are going to check is the PeriodSeperator is present. 

            if (context.qualifiedTextName() != null && (copyParentContext == null || copyParentContext != null && copyParentContext.PeriodSeparator() != null))
            {
                var ctxt = context.qualifiedTextName();
                copy.TextName = GetTextName(ctxt.textName());
                copy.TextNameSymbol = ParseTreeUtils.GetFirstToken(ctxt.textName());
#if EUROINFO_LEGACY_REPLACING_SYNTAX
                if (copy.TextName != null)
                {

                    // Find the list of copy text names variations declared by previous REMARKS compiler directives
                    var variations = CopyTextNameVariations;
                    if (TypeCobolOptions.AutoRemarksEnable &&
                        (variations == null ||
                         !variations.Any(
                             v =>
                                 string.Equals(v.TextNameWithSuffix, copy.TextName,
                                     StringComparison.InvariantCultureIgnoreCase))))
                        //If it does not exists, create the text variation (AutoRemarks mechanism Issue #440)
                    {
                        AnalyticsWrapper.Telemetry.TrackEvent("[Copy-Missing] " + copy.TextName);

                        variations = new List<RemarksDirective.TextNameVariation>
                        {
                            new RemarksDirective.TextNameVariation(copy.TextName)
                        };

                        CopyTextNameVariations.AddRange(variations);
                    }

                    if (variations != null)
                    {
                        var declaration = variations.Find(d => String.Equals(d.TextNameWithSuffix, copy.TextName,
                                        StringComparison.InvariantCultureIgnoreCase));
                        if (declaration != null && copy.TextName.StartsWith("Y", StringComparison.InvariantCultureIgnoreCase))
                        {
                            // Declaration found and copy name starts with Y => apply the legacy REPLACING semantics to the copy directive
                            copy.RemoveFirst01Level = true;
                            if (declaration.HasSuffix)
                            {
                                copy.TextName = declaration.TextName;
                                copy.InsertSuffixChar = true;
                                copy.Suffix = declaration.Suffix;
                                copy.PreSuffix = declaration.PreSuffix;
                            }
                        }
                    }


                }
#endif
                copy.LibraryName = GetLibraryName(ctxt.libraryName());
                copy.LibraryNameSymbol = ParseTreeUtils.GetFirstToken(ctxt.libraryName());
            }


            copy.Suppress = (context.SUPPRESS() != null);

            // REPLACING
            if (context.copyReplacingOperand() != null)
            {
                // Data used to build the current replace operation             
                Token comparisonToken = null;
                Token[] followingComparisonTokens = null;
                Token replacementToken = null;
                Token[] replacementTokens = null;

                // Used to distinguish pseudo-text1 and pseudo-text2
                int pseudoTextIndex = 0;

                foreach (
                    CobolCompilerDirectivesParser.CopyReplacingOperandContext replacingOperandContext in
                    context.copyReplacingOperand())
                {
                    // Get relevant tokens
                    IList<IToken> operandTokens = null;
                    if (replacingOperandContext.pseudoText() != null)
                    {
                        // Pseudo-text => List of tokens
                        if (replacingOperandContext.pseudoText()._pseudoTextTokens != null)
                            operandTokens = replacingOperandContext.pseudoText()._pseudoTextTokens;
                    }
                    else
                    {
                        // Single token
                        if (replacingOperandContext.literalOrUserDefinedWordOReservedWordExceptCopy() != null)
                        {
                            var terminalNode =
                                ParseTreeUtils.GetFirstTerminalNode(
                                    replacingOperandContext.literalOrUserDefinedWordOReservedWordExceptCopy());
                            operandTokens = new List<IToken>(1);
                            operandTokens.Add(terminalNode.Symbol);
                        }
                    }
                    BuildReplaceOperation(copy.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, ref pseudoTextIndex, operandTokens);
                }
            }
        }

        private static void BuildReplaceOperation(IList<ReplaceOperation> replaceOperations, ref Token comparisonToken, ref Token[] followingComparisonTokens, ref Token replacementToken, ref Token[] replacementTokens, ref int pseudoTextIndex, IList<IToken> operandTokens)
        {
            // Comparison tokens
            if (pseudoTextIndex % 2 == 0)
            {
                if (operandTokens != null && operandTokens.Count > 0)
                {
                    comparisonToken = (Token)operandTokens[0];
                    if (operandTokens.Count > 1)
                    {
                        followingComparisonTokens = new Token[operandTokens.Count - 1];
                        for (int i = 1; i < operandTokens.Count; i++)
                        {
                            followingComparisonTokens[i - 1] = (Token)operandTokens[i];
                        }
                    }
                }

                pseudoTextIndex++;
            }
            // Replacement tokens
            else
            {
                if (operandTokens != null && operandTokens.Count > 0)
                {
                    if (followingComparisonTokens == null && operandTokens.Count == 1)
                    {
                        replacementToken = (Token)operandTokens[0];
                    }
                    else
                    {
                        replacementTokens = new Token[operandTokens.Count];
                        for (int i = 0; i < operandTokens.Count; i++)
                        {
                            replacementTokens[i] = (Token)operandTokens[i];
                        }
                    }
                }

                // Build replace operation
                ReplaceOperation replaceOperation = null;
                if (followingComparisonTokens == null)
                {
                    if (replacementTokens == null)
                    {
                        if (comparisonToken == null || comparisonToken.TokenType != TokenType.PartialCobolWord)
                        {
                            replaceOperation = new SingleTokenReplaceOperation(comparisonToken, replacementToken);
                        }
                        else
                        {
                            replaceOperation = new PartialWordReplaceOperation(comparisonToken, replacementToken);
                        }
                    }
                    else
                    {
                        replaceOperation = new SingleToMultipleTokensReplaceOperation(comparisonToken, replacementTokens);
                    }
                }
                else
                {
                    replaceOperation = new MultipleTokensReplaceOperation(comparisonToken, followingComparisonTokens, replacementTokens);
                }
                replaceOperations.Add(replaceOperation);

                // Reset everything for the next replace operation    
                pseudoTextIndex = 0;
                comparisonToken = null;
                followingComparisonTokens = null;
                replacementToken = null;
                replacementTokens = null;
            }
        }

        public override void EnterDeleteCompilerStatement(CobolCompilerDirectivesParser.DeleteCompilerStatementContext context) 
        {
            CompilerDirective = new DeleteDirective();
        }
        
        public override void EnterSequenceNumberField(CobolCompilerDirectivesParser.SequenceNumberFieldContext context)
        {
            DeleteDirective deleteDirective = (DeleteDirective)CompilerDirective;

            bool isFirst = true;
            int previous = -42;
            foreach(ITerminalNode node in context.IntegerLiteral()) {
                int current = (int)ParseTreeUtils.GetIntegerLiteral(node);
                if (isFirst) {
                    previous = current;
                    isFirst = false;
                } else {
                    DeleteDirective.SequenceNumberRange range = new DeleteDirective.SequenceNumberRange();
                    range.From = previous;
                    if(current<0) {
                        range.To = -current;
                        isFirst = true;
                    } else {
                        range.To = previous;
                        previous = current;
                    }
                    deleteDirective.SequenceNumberRangesList.Add(range);
                }
            }
            if (!isFirst && previous >= 0) {
                DeleteDirective.SequenceNumberRange range = new DeleteDirective.SequenceNumberRange();
                range.From = previous;
                range.To = previous;
                deleteDirective.SequenceNumberRangesList.Add(range);
            }
        }

        public override void EnterEjectCompilerStatement(CobolCompilerDirectivesParser.EjectCompilerStatementContext context) 
        {
            CompilerDirective = new EjectDirective();
        }
        
        public override void EnterEnterCompilerStatement(CobolCompilerDirectivesParser.EnterCompilerStatementContext context) 
        {
            EnterDirective enterDirective = new EnterDirective();
            CompilerDirective = enterDirective;

            if(context.languageName() != null)
            {
                string languageName = null;
                TryGetUserDefinedWord(context.languageName().UserDefinedWord(), ref languageName);
                enterDirective.LanguageName = languageName;
            }
            if(context.routineName() != null)
            {
                string routineName = null;
                TryGetUserDefinedWord(context.routineName().UserDefinedWord(), ref routineName);
                enterDirective.RoutineName = routineName;
            }
        }

        public override void EnterExecSqlIncludeStatement(CobolCompilerDirectivesParser.ExecSqlIncludeStatementContext context)
        {
            var copyDirective = new CopyDirective(CompilerDirectiveType.EXEC_SQL_INCLUDE, ParseTreeUtils.GetFirstToken(context.EXEC()));
            CompilerDirective = copyDirective;

            if (context.copyCompilerStatementBody() != null)
            {
                var textNameContext = context.copyCompilerStatementBody().qualifiedTextName().textName();
                if (textNameContext != null)
                {
                    string textName = GetTextName(textNameContext);
                    copyDirective.TextName = textName;
                    copyDirective.TextNameSymbol = ParseTreeUtils.GetFirstToken(textNameContext);
                }

                var libraryNameContext = context.copyCompilerStatementBody().qualifiedTextName().libraryName();
                if (libraryNameContext != null)
                {
                    copyDirective.LibraryName = GetLibraryName(libraryNameContext);
                    copyDirective.LibraryNameSymbol = ParseTreeUtils.GetFirstToken(libraryNameContext);
                }
            }
        }

        public override void EnterInsertCompilerStatement(CobolCompilerDirectivesParser.InsertCompilerStatementContext context) 
        {
            InsertDirective insertDirective = new InsertDirective();
            CompilerDirective = insertDirective;

            if (context.sequenceNumber() != null && context.sequenceNumber().IntegerLiteral() != null)
            {
                insertDirective.SequenceNumber = (int)ParseTreeUtils.GetIntegerLiteral(context.sequenceNumber().IntegerLiteral());
                if (insertDirective.SequenceNumber < 0) {
                    Token errorToken = ParseTreeUtils.GetTokenFromTerminalNode(context.sequenceNumber().IntegerLiteral());
                    Diagnostic error = new Diagnostic(
                        MessageCode.InvalidNumericLiteralFormat,
                        errorToken.Column, errorToken.EndColumn,
                        errorToken.Line, "TODO");
                    CompilerDirective.AddDiagnostic(error);//TODO proper diagnostic error
                }
            }
        }
        
        public override void EnterReadyOrResetTraceCompilerStatement(CobolCompilerDirectivesParser.ReadyOrResetTraceCompilerStatementContext context)
        {
            CompilerDirective = new ReadyOrResetTraceDirective(context.READY() != null ? CompilerDirectiveType.READY_TRACE : CompilerDirectiveType.RESET_TRACE);
        }
        
        public override void EnterReplaceCompilerStatement(CobolCompilerDirectivesParser.ReplaceCompilerStatementContext context) 
        {
            ReplaceDirective replaceDirective = new ReplaceDirective(context.OFF() == null ? CompilerDirectiveType.REPLACE : CompilerDirectiveType.REPLACE_OFF);
            CompilerDirective = replaceDirective;

            if(context.pseudoText() !=null)
            {
                // Data used to build the current replace operation             
                Token comparisonToken = null;
                Token[] followingComparisonTokens = null;
                Token replacementToken = null;
                Token[] replacementTokens = null;

                // Used to distinguish pseudo-text1 and pseudo-text2
                int pseudoTextIndex = 0;

                foreach(CobolCompilerDirectivesParser.PseudoTextContext pseudoTextContext in context.pseudoText())
                {
                    /*// Comparison tokens
                    if (pseudoTextIndex % 2 == 0)
                    {
                        if (pseudoTextContext._pseudoTextTokens != null && pseudoTextContext._pseudoTextTokens.Count > 0)
                        {
                            comparisonToken = (Token)pseudoTextContext._pseudoTextTokens[0];
                            if(pseudoTextContext._pseudoTextTokens.Count > 1)
                            {
                                followingComparisonTokens = new Token[pseudoTextContext._pseudoTextTokens.Count - 1];
                                for(int i = 1 ; i < pseudoTextContext._pseudoTextTokens.Count ; i++)
                                {
                                    followingComparisonTokens[i - 1] = (Token)pseudoTextContext._pseudoTextTokens[i];
                                }
                            }
                        }
                        
                        pseudoTextIndex++;
                    }
                    // Replacement tokens
                    else
                    {
                        if (pseudoTextContext._pseudoTextTokens != null && pseudoTextContext._pseudoTextTokens.Count > 0)
                        {                            
                            if (followingComparisonTokens == null && pseudoTextContext._pseudoTextTokens.Count == 1)
                            {
                                replacementToken = (Token)pseudoTextContext._pseudoTextTokens[0];
                            }
                            else
                            {
                                replacementTokens = new Token[pseudoTextContext._pseudoTextTokens.Count];
                                for (int i = 0; i < pseudoTextContext._pseudoTextTokens.Count; i++)
                                {
                                    replacementTokens[i] = (Token)pseudoTextContext._pseudoTextTokens[i];
                                }
                            }
                        }

                        // Build replace operation
                        ReplaceOperation replaceOperation = null;
                        if(followingComparisonTokens == null)
                        {                            
                            if(replacementTokens == null)
                            {
                                if (comparisonToken == null || comparisonToken.TokenType != TokenType.PartialCobolWord)
                                {
                                    replaceOperation = new SingleTokenReplaceOperation(comparisonToken, replacementToken);
                                }
                                else
                                {
                                    replaceOperation = new PartialWordReplaceOperation(comparisonToken, replacementToken);
                                }
                            }
                            else
                            {
                                replaceOperation = new SingleToMultipleTokensReplaceOperation(comparisonToken, replacementTokens);
                            }
                        }
                        else
                        {
                            replaceOperation = new MultipleTokensReplaceOperation(comparisonToken, followingComparisonTokens, replacementTokens);
                        }
                        replaceDirective.ReplaceOperations.Add(replaceOperation);

                        // Reset everything for the next replace operation    
                        pseudoTextIndex = 0;
                        comparisonToken = null;
                        followingComparisonTokens = null;
                        replacementToken = null;
                        replacementTokens = null;
                    }*/
                    BuildReplaceOperation(replaceDirective.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens, ref replacementToken, ref replacementTokens, ref pseudoTextIndex, pseudoTextContext._pseudoTextTokens);
                }
            }
        }
        
        public override void EnterServiceLabelCompilerStatement(CobolCompilerDirectivesParser.ServiceLabelCompilerStatementContext context) 
        {
            CompilerDirective = new ServiceLabelDirective();
        }
        
        public override void EnterServiceReloadCompilerStatement(CobolCompilerDirectivesParser.ServiceReloadCompilerStatementContext context) 
        {
            ServiceReloadDirective serviceReloadDirective = new ServiceReloadDirective();
            CompilerDirective = serviceReloadDirective;

            string userDefinedWord = null;
            TryGetUserDefinedWord(context.UserDefinedWord(), ref userDefinedWord);
            serviceReloadDirective.UserDefinedWord = userDefinedWord;
        }
                
        public override void EnterSkipCompilerStatement(CobolCompilerDirectivesParser.SkipCompilerStatementContext context) 
        { 
            if(context.SKIP1() != null)
            {
                CompilerDirective = new SkipDirective(CompilerDirectiveType.SKIP1);
            }
            else if (context.SKIP2() != null)
            {
                CompilerDirective = new SkipDirective(CompilerDirectiveType.SKIP2);
            }
            else if (context.SKIP3() != null)
            {
                CompilerDirective = new SkipDirective(CompilerDirectiveType.SKIP3);
            }
        }
                
        public override void EnterTitleCompilerStatement(CobolCompilerDirectivesParser.TitleCompilerStatementContext context) 
        {
            TitleDirective titleDirective = new TitleDirective();
            CompilerDirective = titleDirective;

            string title =  ParseTreeUtils.GetAlphanumericLiteral(context.alphanumericValue2());
            titleDirective.Title = title;
        }
    }
}
