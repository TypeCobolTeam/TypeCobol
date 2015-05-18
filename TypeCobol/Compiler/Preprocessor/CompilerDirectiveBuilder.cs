using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Build a CompilerDirective object while visiting its parse tree
    /// </summary>
    internal class CompilerDirectiveBuilder : CobolDirectivesBaseListener
    {
        /// <summary>
        /// CompilerDirective object resulting of the visit the parse tree
        /// </summary>
        public CompilerDirective CompilerDirective { get; private set; }

        /// <summary>
        /// List of syntax diagnostics gathered while transversing the parse tree
        /// </summary>
        public IList<Diagnostic> Diagnostics { get; private set; }
        
        // --- Visiting the tree ---

        public override void EnterCompilerDirectingStatement(CobolDirectivesParser.CompilerDirectingStatementContext context)
        {
            CompilerDirective = null;
            Diagnostics = new List<Diagnostic>();
        }

        public override void EnterBasisCompilerStatement(CobolDirectivesParser.BasisCompilerStatementContext context) 
        {
            CompilerDirective = new BasisDirective();
        }
        
        public override void EnterBasisName(CobolDirectivesParser.BasisNameContext context) 
        {
            string basisName = null;
            ParseTreeUtils.TryGetAlphanumericLiteralValue(context.AlphanumericLiteral(), ref basisName);
            ParseTreeUtils.TryGetUserDefinedWord(context.UserDefinedWord(), ref basisName);

            ((BasisDirective)CompilerDirective).BasisName = basisName;
        }
        
        public override void EnterControlCblCompilerStatement(CobolDirectivesParser.ControlCblCompilerStatementContext context) 
        {
            CompilerDirective = new ControlCblDirective(context.ASTERISK_CONTROL() != null ? CompilerDirectiveType.ASTERISK_CONTROL : CompilerDirectiveType.ASTERISK_CBL);
        }

        public override void EnterControlCblOption(CobolDirectivesParser.ControlCblOptionContext context) 
        {
            string option = null;
            ParseTreeUtils.TryGetUserDefinedWord(context.UserDefinedWord(), ref option);
            if (option != null)
            {
                ControlCblDirective.ControlCblOption optionValue;
                if(Enum.TryParse<ControlCblDirective.ControlCblOption>(option, out optionValue))
                {
                    ((ControlCblDirective)CompilerDirective).OptionsList.Add(optionValue);
                }
                else
                {
                    Token errorToken = ParseTreeUtils.GetToken(context.UserDefinedWord());
                    Diagnostic diag = new Diagnostic(
                        MessageCode.InvalidControlCblCompilerStatementOption, 
                        errorToken.Column, errorToken.EndColumn,
                        option);
                    Diagnostics.Add(diag);
                }
            }
        }
        
        public override void EnterCopyCompilerStatement(CobolDirectivesParser.CopyCompilerStatementContext context) 
        {
            CompilerDirective = new CopyDirective(CompilerDirectiveType.COPY);
        }
        
        public override void EnterTextName(CobolDirectivesParser.TextNameContext context)
        {
            CopyDirective copyDirective = CompilerDirective as CopyDirective;
            if (copyDirective != null)
            {
                string textName = null;
                ParseTreeUtils.TryGetAlphanumericLiteralValue(context.AlphanumericLiteral(), ref textName);
                ParseTreeUtils.TryGetUserDefinedWord(context.UserDefinedWord(), ref textName);
                copyDirective.TextName = textName;
            }
        }

        public override void EnterLibraryName(CobolDirectivesParser.LibraryNameContext context)
        {
            CopyDirective copyDirective = CompilerDirective as CopyDirective;
            if (copyDirective != null)
            {
                string libraryName = null;
                ParseTreeUtils.TryGetAlphanumericLiteralValue(context.AlphanumericLiteral(), ref libraryName);
                ParseTreeUtils.TryGetUserDefinedWord(context.UserDefinedWord(), ref libraryName);
                copyDirective.LibraryName = libraryName;
            }
        }

        public override void EnterCopyCompilerStatementBody(CobolDirectivesParser.CopyCompilerStatementBodyContext context) 
        {
            CopyDirective copyDirective = (CopyDirective)CompilerDirective;

            // SUPPRESS
            if(context.SUPPRESS() != null)
            {
                copyDirective.Suppress = true;
            }
            else
            {
                copyDirective.Suppress = false;
            }

            // REPLACING
            if(context.copyReplacingOperand() != null)
            {
                // Data used to build the current replace operation             
                Token comparisonToken = null;
                Token[] followingComparisonTokens = null;
                Token replacementToken = null;
                Token[] replacementTokens = null;

                // Used to distinguish pseudo-text1 and pseudo-text2
                int pseudoTextIndex = 0;

                foreach (CobolDirectivesParser.CopyReplacingOperandContext replacingOperandContext in context.copyReplacingOperand())
                {
                    // Get relevant tokens
                    IList<IToken> operandTokens = null;
                    // Pseudo-text => List of tokens
                    if (replacingOperandContext.pseudoText() != null)
                    {
                        if (replacingOperandContext.pseudoText()._pseudoTextTokens != null)
                        {
                            operandTokens = replacingOperandContext.pseudoText()._pseudoTextTokens;
                        }
                    }
                    // Single token 
                    else
                    {
                        ITerminalNode terminalNode = null;
                        if (replacingOperandContext.literal() != null)
                        {
                            terminalNode = ParseTreeUtils.GetFirstTerminalNode(replacingOperandContext.literal());
                        }
                        else if (replacingOperandContext.UserDefinedWord() != null)
                        {
                            terminalNode = replacingOperandContext.UserDefinedWord();
                        }
                        else if (replacingOperandContext.FunctionName() != null)
                        {
                            terminalNode = replacingOperandContext.FunctionName();
                        }
                        else if (replacingOperandContext.reservedWord() != null)
                        {
                            terminalNode = ParseTreeUtils.GetFirstTerminalNode(replacingOperandContext.reservedWord());
                        }
                        if (terminalNode != null)
                        {
                            operandTokens = new List<IToken>(1);
                            operandTokens.Add(terminalNode.Symbol);
                        }
                    }

                    BuildReplaceOperation(copyDirective.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens, ref replacementToken, ref replacementTokens, ref pseudoTextIndex, operandTokens);
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

        public override void EnterDeleteCompilerStatement(CobolDirectivesParser.DeleteCompilerStatementContext context) 
        {
            CompilerDirective = new DeleteDirective();
        }
        
        public override void EnterSequenceNumberElement(CobolDirectivesParser.SequenceNumberElementContext context) 
        {
            DeleteDirective deleteDirective = (DeleteDirective)CompilerDirective;
            DeleteDirective.SequenceNumberRange sequenceNumberRange = new DeleteDirective.SequenceNumberRange();

            // Single number
            if (context.DecimalLiteral() == null)
            {
                int sequenceNumber = (int)ParseTreeUtils.GetIntegerLiteralValue(context.IntegerLiteral());
                sequenceNumberRange.From = sequenceNumber;
                sequenceNumberRange.To = sequenceNumber;
            }
            // Real range
            else
            {
                sequenceNumberRange.From = (int)ParseTreeUtils.GetIntegerLiteralValue(context.IntegerLiteral());
                sequenceNumberRange.To = -(int)((DecimalLiteralValue)((Token)(context.DecimalLiteral().Symbol)).LiteralValue).IntegerValue;
            }

            deleteDirective.SequenceNumberRangesList.Add(sequenceNumberRange);
        }

        public override void EnterEjectCompilerStatement(CobolDirectivesParser.EjectCompilerStatementContext context) 
        {
            CompilerDirective = new EjectDirective();
        }
        
        public override void EnterEnterCompilerStatement(CobolDirectivesParser.EnterCompilerStatementContext context) 
        {
            EnterDirective enterDirective = new EnterDirective();
            CompilerDirective = enterDirective;

            if(context.languageName() != null)
            {
                string languageName = null;
                ParseTreeUtils.TryGetUserDefinedWord(context.languageName().UserDefinedWord(), ref languageName);
                enterDirective.LanguageName = languageName;
            }
            if(context.routineName() != null)
            {
                string routineName = null;
                ParseTreeUtils.TryGetUserDefinedWord(context.routineName().UserDefinedWord(), ref routineName);
                enterDirective.RoutineName = routineName;
            }
        }
        
        public override void EnterExecSqlIncludeStatement(CobolDirectivesParser.ExecSqlIncludeStatementContext context) 
        {
            CompilerDirective = new CopyDirective(CompilerDirectiveType.EXEC_SQL_INCLUDE);
        }

        public override void EnterInsertCompilerStatement(CobolDirectivesParser.InsertCompilerStatementContext context) 
        {
            InsertDirective insertDirective = new InsertDirective();
            CompilerDirective = insertDirective;

            if (context.sequenceNumber() != null && context.sequenceNumber().IntegerLiteral() != null)
            {
                insertDirective.SequenceNumber = (int)ParseTreeUtils.GetIntegerLiteralValue(context.sequenceNumber().IntegerLiteral());
            }
        }
        
        public override void EnterReadyOrResetTraceCompilerStatement(CobolDirectivesParser.ReadyOrResetTraceCompilerStatementContext context)
        {
            CompilerDirective = new ReadyOrResetTraceDirective(context.READY() != null ? CompilerDirectiveType.READY_TRACE : CompilerDirectiveType.RESET_TRACE);
        }
        
        public override void EnterReplaceCompilerStatement(CobolDirectivesParser.ReplaceCompilerStatementContext context) 
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

                foreach(CobolDirectivesParser.PseudoTextContext pseudoTextContext in context.pseudoText())
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
        
        public override void EnterServiceLabelCompilerStatement(CobolDirectivesParser.ServiceLabelCompilerStatementContext context) 
        {
            CompilerDirective = new ServiceLabelDirective();
        }
        
        public override void EnterServiceReloadCompilerStatement(CobolDirectivesParser.ServiceReloadCompilerStatementContext context) 
        {
            ServiceReloadDirective serviceReloadDirective = new ServiceReloadDirective();
            CompilerDirective = serviceReloadDirective;

            string userDefinedWord = null;
            ParseTreeUtils.TryGetUserDefinedWord(context.UserDefinedWord(), ref userDefinedWord);
            serviceReloadDirective.UserDefinedWord = userDefinedWord;
        }
                
        public override void EnterSkipCompilerStatement(CobolDirectivesParser.SkipCompilerStatementContext context) 
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
                
        public override void EnterTitleCompilerStatement(CobolDirectivesParser.TitleCompilerStatementContext context) 
        {
            TitleDirective titleDirective = new TitleDirective();
            CompilerDirective = titleDirective;

            string title = null;
            ParseTreeUtils.TryGetAlphanumericLiteralValue(context.AlphanumericLiteral(), ref title);
            ParseTreeUtils.TryGetAlphanumericLiteralValue(context.NationalLiteral(), ref title);
            ParseTreeUtils.TryGetAlphanumericLiteralValue(context.DBCSLiteral(), ref title);
            titleDirective.Title = title;
        }
    }
}
