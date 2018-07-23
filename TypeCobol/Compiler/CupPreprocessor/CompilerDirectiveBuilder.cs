using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CupCommon;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// CompilerDirectiveBuilder
    /// </summary>
    public class CompilerDirectiveBuilder : ICompilerDirectiveBuilder
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

        private string GetName(TypeCobol.Compiler.Scanner.Token name)
        {
            return name?.Text.Trim('\'').Trim('\"');
        }

        public virtual void StartBasisCompilerStatement()
        {
            var basisDirective = new BasisDirective();
            CompilerDirective = basisDirective;
        }
        public virtual void EnterBasisCompilerStatement(TypeCobol.Compiler.Scanner.Token textName)
        {
            var basisDirective = (BasisDirective)CompilerDirective;            

            if (textName != null)
            {
                basisDirective.BasisName = GetName(textName);
                basisDirective.TextNameSymbol = textName;
            }
        }

        public virtual void StartControlCblCompilerStatement(CompilerDirectiveType type)
        {
            CompilerDirective = new ControlCblDirective(type);
        }

        public virtual void EnterControlCblCompilerStatement(TypeCobol.Compiler.Scanner.Token controlCbl)
        {            
        }

        public virtual void EnterCopyCompilerStatement(TypeCobol.Compiler.Scanner.Token copy)
        {
            CompilerDirective = new CopyDirective(CompilerDirectiveType.COPY, copy);
        }

        public virtual void EnterCopyCompilerStatementBody(QualifiedTextName qualifiedTextName, 
            TypeCobol.Compiler.Scanner.Token suppress, PairTokenListList replacingOperands)
        {
            var copy = (CopyDirective)CompilerDirective;
            bool isCopy = copy.COPYToken.TokenType == TokenType.COPY;
            copy.TextName = GetName(qualifiedTextName.TextName);
            copy.TextNameSymbol = qualifiedTextName.TextName;
            if (isCopy)
            {                
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
            }
            copy.LibraryName = GetName(qualifiedTextName.LibraryName);
            copy.LibraryNameSymbol = qualifiedTextName.LibraryName;

            copy.Suppress = suppress != null;

            // REPLACING
            if (replacingOperands != null)
            {
                // Data used to build the current replace operation             
                Token comparisonToken = null;
                Token[] followingComparisonTokens = null;
                Token replacementToken = null;
                Token[] replacementTokens = null;

                foreach (Tuple<List<Token>,List<Token>> copyReplacingOperands in replacingOperands)
                {
                    // Get relevant tokens
                    List<Token> relevantTokens = copyReplacingOperands.Item1;
                    List<Token> replaceTokens = copyReplacingOperands.Item2;
                    BuildReplaceOperation(copy.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, false, relevantTokens);

                    BuildReplaceOperation(copy.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, true, replaceTokens);

                }
            }
        }

        private static void BuildReplaceOperation(IList<ReplaceOperation> replaceOperations, ref Token comparisonToken, ref Token[] followingComparisonTokens, ref Token replacementToken, ref Token[] replacementTokens, bool replaceTokens, List<Token> operandTokens)
        {
            // Comparison tokens
            if (!replaceTokens)
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
                comparisonToken = null;
                followingComparisonTokens = null;
                replacementToken = null;
                replacementTokens = null;
            }
        }

        public virtual void EnterDeleteCompilerStatement(Token delToken)
        {
            CompilerDirective = new DeleteDirective();
        }

        public void EnterSequenceNumberField(List<Token> seqNumField)
        {
            DeleteDirective deleteDirective = (DeleteDirective)CompilerDirective;

            bool isFirst = true;
            int previous = -42;
            foreach(Token integerLiteralToken in seqNumField)
            {
                System.Diagnostics.Debug.Assert(integerLiteralToken.TokenType == TokenType.IntegerLiteral);
                int current = (int)((IntegerLiteralTokenValue)integerLiteralToken.LiteralValue).Number;

                if (isFirst)
                {
                    previous = current;
                    isFirst = false;
                }
                else
                {
                    DeleteDirective.SequenceNumberRange range = new DeleteDirective.SequenceNumberRange();
                    range.From = previous;
                    if (current < 0)
                    {
                        range.To = -current;
                        isFirst = true;
                    }
                    else
                    {
                        range.To = previous;
                        previous = current;
                    }
                    deleteDirective.SequenceNumberRangesList.Add(range);
                }
            }
            if (!isFirst && previous >= 0)
            {
                DeleteDirective.SequenceNumberRange range = new DeleteDirective.SequenceNumberRange();
                range.From = previous;
                range.To = previous;
                deleteDirective.SequenceNumberRangesList.Add(range);
            }
        }

        public void EnterEjectCompilerStatement(Token ejectToken)
        {
            CompilerDirective = new EjectDirective();
        }

        public void EnterEnterCompilerStatement(Token enterToken, Token languageName, Token routineName)
        {
            EnterDirective enterDirective = new EnterDirective();
            CompilerDirective = enterDirective;
            enterDirective.LanguageName = languageName.Text;
            if (routineName != null)
                enterDirective.RoutineName = routineName.Text;
        }

        public virtual void EnterExecSqlIncludeStatement(Token execToken)
        {
            var copyDirective = new CopyDirective(CompilerDirectiveType.EXEC_SQL_INCLUDE, execToken);
            CompilerDirective = copyDirective;
        }

        public virtual void EnterInsertCompilerStatement(Token insertToken, Token sequenceNumber)
        {
            InsertDirective insertDirective = new InsertDirective();
            CompilerDirective = insertDirective;

            System.Diagnostics.Debug.Assert(sequenceNumber.TokenType == TokenType.IntegerLiteral);

            insertDirective.SequenceNumber = (int)((IntegerLiteralTokenValue)sequenceNumber.LiteralValue).Number;
            if (insertDirective.SequenceNumber < 0)
            {
                Token errorToken = sequenceNumber;
                Diagnostic error = new Diagnostic(
                    MessageCode.InvalidNumericLiteralFormat,
                    errorToken.Column, errorToken.EndColumn,
                    errorToken.Line, "TODO");
                CompilerDirective.AddDiagnostic(error);//TODO proper diagnostic error
            }
        }

        public virtual void EnterReadyOrResetTraceCompilerStatement(Token readyOrResetToken)
        {
            CompilerDirective = new ReadyOrResetTraceDirective(readyOrResetToken.TokenType == TokenType.READY ? CompilerDirectiveType.READY_TRACE : CompilerDirectiveType.RESET_TRACE);
        }

        public virtual void EnterReplaceCompilerStatement(Token replaceTokn, Token offToken, PairTokenListList replacingOperands)
        {
            ReplaceDirective replaceDirective = new ReplaceDirective(offToken == null ? CompilerDirectiveType.REPLACE : CompilerDirectiveType.REPLACE_OFF);
            CompilerDirective = replaceDirective;

            if (replacingOperands != null)
            {
                // Data used to build the current replace operation             
                Token comparisonToken = null;
                Token[] followingComparisonTokens = null;
                Token replacementToken = null;
                Token[] replacementTokens = null;

                foreach (Tuple<List<Token>, List<Token>> copyReplacingOperands in replacingOperands)
                {
                    // Get relevant tokens
                    List<Token> relevantTokens = copyReplacingOperands.Item1;
                    List<Token> replaceTokens = copyReplacingOperands.Item2;
                    BuildReplaceOperation(replaceDirective.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, false, relevantTokens);

                    BuildReplaceOperation(replaceDirective.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, true, replaceTokens);
                }
            }
        }

        public virtual void EnterServiceLabelCompilerStatement(Token serviceToken, Token labelToken)
        {
            CompilerDirective = new ServiceLabelDirective();
        }

        public virtual void EnterServiceReloadCompilerStatement(Token serviceToken, Token reloadToken, Token userDefinedWord)
        {
            ServiceReloadDirective serviceReloadDirective = new ServiceReloadDirective();
            CompilerDirective = serviceReloadDirective;

            serviceReloadDirective.UserDefinedWord = userDefinedWord.Text;
        }

        public virtual void EnterSkipCompilerStatement(Token skipTolen)
        {
            switch (skipTolen.TokenType)
            {
                case TokenType.SKIP1:
                    CompilerDirective = new SkipDirective(CompilerDirectiveType.SKIP1);
                    break;
                case TokenType.SKIP2:
                    CompilerDirective = new SkipDirective(CompilerDirectiveType.SKIP2);
                    break;
                case TokenType.SKIP3:
                    CompilerDirective = new SkipDirective(CompilerDirectiveType.SKIP3);
                    break;
            }
        }

        public virtual void EnterTitleCompilerStatement(Token titleToken, Token titleLiteral)
        {
            TitleDirective titleDirective = new TitleDirective();
            CompilerDirective = titleDirective;

            System.Diagnostics.Debug.Assert(titleLiteral.TokenFamily == TokenFamily.AlphanumericLiteral);
            string title = ((AlphanumericLiteralTokenValue)titleLiteral.LiteralValue).Text;
            titleDirective.Title = title;
        }
    }
}
