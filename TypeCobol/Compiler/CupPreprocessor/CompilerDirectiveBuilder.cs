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
            _replaceOperationBuilder = new ReplaceOperationBuilder(this);
        }

        private readonly CompilationDocument _document;
        private readonly ReplaceOperationBuilder _replaceOperationBuilder;

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
            TypeCobol.Compiler.Scanner.Token suppress, CupReplaceOperations replacingOperands)
        {
            var copy = (CopyDirective)CompilerDirective;
            copy.TextName = GetName(qualifiedTextName.TextName);
            copy.TextNameSymbol = qualifiedTextName.TextName;

            if (copy.TextName != null)
            {
                // Find the text name variation declared by previous REMARKS compiler directives (if parsed) or add new text name.
                var variation = RemarksDirective.TextNameVariation.FindOrAdd(CopyTextNameVariations, copy);

#if  EUROINFO_RULES
                bool removeFirst01Level = TypeCobolOptions.EILegacy_RemoveFirst01Level;
                bool applyCopySuffixing = TypeCobolOptions.EILegacy_ApplyCopySuffixing;
                if (removeFirst01Level || applyCopySuffixing)
                {
                    if (this.TypeCobolOptions.IsCpyCopy(variation.TextName))
                    {
                        // Declaration found and copy name starts with Y => apply the legacy REPLACING semantics to the copy directive
                        copy.RemoveFirst01Level = removeFirst01Level;
                        if (applyCopySuffixing && variation.HasSuffix)
                        {
                            copy.TextName = variation.TextName;
                            copy.Suffix = variation.Suffix;
                            copy.PreSuffix = variation.PreSuffix;
                        }
                    }
                }

                _document.CollectUsedCopy(copy);
#endif
            }

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
                foreach (CupReplaceOperation cupReplaceOperation in replacingOperands)
                {
                    _replaceOperationBuilder.BuildFromCupReplaceOperation(copy.ReplaceOperations, cupReplaceOperation, copy.COPYToken);
                }
            }
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
        public virtual void EnterReplaceCompilerStatement(Token replaceToken, Token offToken, CupReplaceOperations cupReplacingOperands)
        {
            ReplaceDirective replaceDirective = (ReplaceDirective)CompilerDirective;
            if (cupReplacingOperands != null)
            {
                foreach (var cupReplaceOperation in cupReplacingOperands)
                {
                    _replaceOperationBuilder.BuildFromCupReplaceOperation(replaceDirective.ReplaceOperations, cupReplaceOperation, replaceToken);
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
