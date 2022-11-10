using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CupCommon;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// CompilerDirectiveBuilder
    /// </summary>
    public class CompilerDirectiveBuilder : ICompilerDirectiveBuilder
    {
        public CompilerDirectiveBuilder(CompilationDocument document)
        {
            _document = document;
        }

        private readonly CompilationDocument _document;
        private TypeCobolOptions TypeCobolOptions => _document.CompilerOptions;
        private List<RemarksDirective.TextNameVariation> CopyTextNameVariations => _document.CopyTextNamesVariations;
        /// <summary>
        /// CompilerDirective object resulting of the visit the parse tree
        /// </summary>
        public CompilerDirective CompilerDirective { get; private set; }

        public void ResetCompilerDirective()
        {
            CompilerDirective = null;
        }

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

        public virtual void EnterControlCblOption(Token optionToken)
        {
            string option = optionToken.Text;
            ControlCblDirective.ControlCblOption optionValue;
            if (Enum.TryParse<ControlCblDirective.ControlCblOption>(option, out optionValue))
            {
                ((ControlCblDirective)CompilerDirective).OptionsList.Add(optionValue);
            }
            else
            {
                Token errorToken = optionToken;
                Diagnostic diag = new Diagnostic(MessageCode.InvalidControlCblCompilerStatementOption, errorToken.Position(), option);
                CompilerDirective.AddParsingDiagnostic(diag);
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
            copy.TextName = GetName(qualifiedTextName.TextName);
            copy.TextNameSymbol = qualifiedTextName.TextName;
#if EUROINFO_RULES
            if (copy.TextName != null)
            {
                if (TypeCobolOptions.UseEuroInformationLegacyReplacingSyntax)
                {
                    // Find the text name variation declared by previous REMARKS compiler directives (if parsed)
                    var variation = RemarksDirective.TextNameVariation.FindOrAdd(CopyTextNameVariations, copy);

                    if (this.TypeCobolOptions.IsCpyCopy(variation.TextName))
                    {
                        // Declaration found and copy name starts with Y => apply the legacy REPLACING semantics to the copy directive
                        copy.RemoveFirst01Level = true;
                        if (variation.HasSuffix)
                        {
                            copy.TextName = variation.TextName;
                            copy.Suffix = variation.Suffix;
                            copy.PreSuffix = variation.PreSuffix;
                        }
                    }
                }
                _document.CollectUsedCopy(copy);
            }
#endif
            copy.LibraryName = GetName(qualifiedTextName.LibraryName);
            copy.LibraryNameSymbol = qualifiedTextName.LibraryName;

            copy.Suppress = suppress != null;
            if (suppress != null)
            {
                Diagnostic error = new Diagnostic(MessageCode.Warning, suppress.Position(), "\"COPY SUPPRESS\" should not be used");
                CompilerDirective.AddParsingDiagnostic(error);
            }

            // REPLACING
            if (replacingOperands != null)
            {
                // Data used to build the current replace operation             
                Token comparisonToken = null;
                Token[] followingComparisonTokens = null;
                Token replacementToken = null;
                Token[] replacementTokens = null;

                bool bReported = false;
                foreach (Tuple<List<Token>,List<Token>> copyReplacingOperands in replacingOperands)
                {
                    // Get relevant tokens
                    List<Token> relevantTokens = copyReplacingOperands.Item1;
                    List<Token> replaceTokens = copyReplacingOperands.Item2;
                    if (!BuildReplaceOperation(copy.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, false, relevantTokens))
                    {
                        if (!bReported)
                        {
                            Diagnostic error = new Diagnostic(MessageCode.SyntaxErrorInParser, qualifiedTextName.TextName.Position(), "\"REPLACE\" Empty Comparison Pseudo Text.");
                            CompilerDirective.AddParsingDiagnostic(error);
                            bReported = true;
                        }
                    }

                    BuildReplaceOperation(copy.ReplaceOperations, ref comparisonToken, ref followingComparisonTokens,
                        ref replacementToken, ref replacementTokens, true, replaceTokens);
                }
            }
        }

        private static bool BuildReplaceOperation(IList<ReplaceOperation> replaceOperations, ref Token comparisonToken, ref Token[] followingComparisonTokens, ref Token replacementToken, ref Token[] replacementTokens, bool replaceTokens, List<Token> operandTokens)
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
                else
                {//It cannot be empty
                    return false;
                }
            }
            // Replacement tokens
            else
            {
                if (operandTokens != null && operandTokens.Count > 0)
                {
                    if (followingComparisonTokens == null && operandTokens.Count == 1)
                    {
                        replacementToken = operandTokens[0];
                    }
                    else
                    {
                        replacementTokens = operandTokens.Where(t => t.TokenFamily != TokenFamily.Comments).ToArray();
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
            return true;
        }

        public virtual void StartDeleteCompilerStatement()
        {
            CompilerDirective = new DeleteDirective();
        }
        public virtual void EnterDeleteCompilerStatement(Token delToken)
        {            
        }

        public void EnterSequenceNumberField(List<Token> seqNumField)
        {
            DeleteDirective deleteDirective = (DeleteDirective)CompilerDirective;


            foreach (Token token in seqNumField)
            {
                if (token.TokenType == TokenType.IntegerLiteral)
                {
                    int current = (int)((IntegerLiteralTokenValue)token.LiteralValue).Number;

                    //Report error for negative number and ignore them
                    if (current < 0)
                    {
                        AddInvalidIntegerDiagnostic(current.ToString());
                    }
                    else
                    {
                        DeleteDirective.SequenceNumberRange range = new DeleteDirective.SequenceNumberRange();
                        range.From = current;
                        range.To = current;
                        deleteDirective.SequenceNumberRangesList.Add(range);
                    }
                }
                else if (token.TokenType == TokenType.UserDefinedWord)
                {
                    var numbers = token.Text.Split('-');

                    if (numbers.Length != 2)
                    {
                        Diagnostic error = new Diagnostic(MessageCode.SyntaxErrorInParser, token.Position(),
                            "Invalid range format");
                        CompilerDirective.AddParsingDiagnostic(error);
                    }
                    else
                    {
                        DeleteDirective.SequenceNumberRange range = new DeleteDirective.SequenceNumberRange();
                        if (TryParseIntOrAddDiagnostic(numbers[0], out var temp))
                        {
                            range.From = temp;
                            if (TryParseIntOrAddDiagnostic(numbers[1], out temp))
                            {
                                range.To = temp;
                                deleteDirective.SequenceNumberRangesList.Add(range);
                            }
                        } //else ignore values

                        bool TryParseIntOrAddDiagnostic(string integerText, out int result)
                        {
                            if (!int.TryParse(integerText, out result))
                            {
                                AddInvalidIntegerDiagnostic(integerText);
                                return false;
                            }
                            return true;
                        }
                    }
                }
                else
                {
                    Diagnostic error = new Diagnostic(MessageCode.SyntaxErrorInParser, token.Position(),
                        $"Unexpected token: ${token} of type ${token.TokenType}");
                    CompilerDirective.AddParsingDiagnostic(error);
                }

                void AddInvalidIntegerDiagnostic(string invalidInteger)
                {
                    string errorMessage;
                    if (string.IsNullOrWhiteSpace(invalidInteger))
                    {
                        errorMessage = "No value provided. Expected a positive integer.";
                    }
                    else
                    {
                        errorMessage = "Invalid value: " + invalidInteger + ". Expected a positive integer.";
                    }

                    Diagnostic error = new Diagnostic(MessageCode.SyntaxErrorInParser, token.Position(), errorMessage);
                    CompilerDirective.AddParsingDiagnostic(error);
                }
            }
        }

        public virtual void StartEjectCompilerStatement()
        {
            CompilerDirective = new EjectDirective();
        }

        public void EnterEjectCompilerStatement(Token ejectToken)
        {            
        }

        public virtual void StartEnterCompilerStatement()
        {
            EnterDirective enterDirective = new EnterDirective();
            CompilerDirective = enterDirective;
        }
        public void EnterEnterCompilerStatement(Token enterToken, Token languageName, Token routineName)
        {
            EnterDirective enterDirective = (EnterDirective)CompilerDirective;
            enterDirective.LanguageName = languageName.Text;
            if (routineName != null)
                enterDirective.RoutineName = routineName.Text;
        }

        public virtual void EnterExecSqlIncludeStatement(Token execToken)
        {
            var copyDirective = new CopyDirective(CompilerDirectiveType.EXEC_SQL_INCLUDE, execToken);
            CompilerDirective = copyDirective;
        }

        public virtual void StartInsertCompilerStatement()
        {
            InsertDirective insertDirective = new InsertDirective();
            CompilerDirective = insertDirective;
        }

        public virtual void EnterInsertCompilerStatement(Token insertToken, Token sequenceNumber)
        {
            InsertDirective insertDirective = (InsertDirective)CompilerDirective;

            System.Diagnostics.Debug.Assert(sequenceNumber.TokenType == TokenType.IntegerLiteral);

            insertDirective.SequenceNumber = (int)((IntegerLiteralTokenValue)sequenceNumber.LiteralValue).Number;
            if (insertDirective.SequenceNumber < 0)
            {
                Token errorToken = sequenceNumber;
                Diagnostic error = new Diagnostic(MessageCode.InvalidNumericLiteralFormat, errorToken.Position());
                CompilerDirective.AddParsingDiagnostic(error);//TODO proper diagnostic error
            }
        }

        public virtual void StartReadyOrResetTraceCompilerStatement(CompilerDirectiveType type)
        {
            CompilerDirective = new ReadyOrResetTraceDirective(type);
        }

        public virtual void EnterReadyOrResetTraceCompilerStatement(Token readyOrResetToken)
        {            
        }

        public virtual void StartReplaceCompilerStatement(CompilerDirectiveType type)
        {
            ReplaceDirective replaceDirective = new ReplaceDirective(type);
            CompilerDirective = replaceDirective;
        }
        public virtual void EnterReplaceCompilerStatement(Token replaceTokn, Token offToken, PairTokenListList replacingOperands)
        {
            ReplaceDirective replaceDirective = (ReplaceDirective)CompilerDirective;            

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

        public virtual void StartServiceLabelCompilerStatement()
        {
            CompilerDirective = new ServiceLabelDirective();
        }

        public virtual void EnterServiceLabelCompilerStatement(Token serviceToken, Token labelToken)
        {            
        }

        public virtual void StartServiceReloadCompilerStatement(Token serviceToken, Token reloadToken,
            Token userDefinedWord)
        {
        }

        public virtual void StartServiceReloadCompilerStatement()
        {
            ServiceReloadDirective serviceReloadDirective = new ServiceReloadDirective();
            CompilerDirective = serviceReloadDirective;
        }

        public virtual void EnterServiceReloadCompilerStatement(Token serviceToken, Token reloadToken, Token userDefinedWord)
        {
            ServiceReloadDirective serviceReloadDirective =  (ServiceReloadDirective)CompilerDirective;

            serviceReloadDirective.UserDefinedWord = userDefinedWord.Text;
        }

        public virtual void StartSkipCompilerStatement(CompilerDirectiveType type)
        {
            System.Diagnostics.Debug.Assert(type == CompilerDirectiveType.SKIP1 || 
                type == CompilerDirectiveType.SKIP2 ||
                type == CompilerDirectiveType.SKIP3);
            CompilerDirective = new SkipDirective(type);
        }

        public virtual void EnterSkipCompilerStatement(Token skipTolen)
        {
        }

        public virtual void StartTitleCompilerStatement()
        {
            TitleDirective titleDirective = new TitleDirective();
            CompilerDirective = titleDirective;
        }

        public virtual void EnterTitleCompilerStatement(Token titleToken, Token titleLiteral)
        {
            TitleDirective titleDirective = (TitleDirective)CompilerDirective;

            System.Diagnostics.Debug.Assert(titleLiteral.TokenFamily == TokenFamily.AlphanumericLiteral);
            string title = ((AlphanumericLiteralTokenValue)titleLiteral.LiteralValue).Text;
            titleDirective.Title = title;
        }
    }
}
