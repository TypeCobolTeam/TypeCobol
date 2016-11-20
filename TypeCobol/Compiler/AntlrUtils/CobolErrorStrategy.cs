using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Customized error recovery strategy optimized for the Cobol programming language :
    /// the idea is that we want to benefit as much as possible from the default Antlr
    /// error recovery strategy, while leveraging insights specific to the Cobol language.
    /// A Cobol program is really structured as a list of CodeElements, and in many cases
    /// the next token type is enough to detect the start of the next CodeElement.
    /// The goal of this customized strategy is then to always avoid going further than
    /// the start of the next CodeElement when we try to resynchronize with the 
    /// underlying token stream after a syntax error.
    /// </summary>
    public class CobolErrorStrategy : DefaultErrorStrategy
    {
        // --- Override the method below to customize Antlr Token deleting strategy ---

        /// <summary>
        /// Should return true for all the tokens we don't want to be consumed
        /// by the error recovery strategy when resynchronizing
        /// </summary>
        protected virtual bool ErrorStrategyShouldNotConsumeThisToken(Token lastConsumedToken, Token currentInvalidToken)
        {
            return false;
        }

        // --- Override the five methods below to customize Antlr error messages ---
        
        protected virtual string ErrorMessageForUnexpectedToken(Antlr4.Runtime.Parser recognizer, IToken t)
        {
            string tokenName = GetTokenErrorDisplay(t);
            IntervalSet expecting = GetExpectedTokens(recognizer);
            string msg = "extraneous input " + tokenName + " expecting " + expecting.ToString(recognizer.Vocabulary);
            return msg;
        }

        protected virtual string ErrorMessageForMissingToken(Antlr4.Runtime.Parser recognizer, IToken t)
        {
            IntervalSet expecting = GetExpectedTokens(recognizer);
            string msg = "missing " + expecting.ToString(recognizer.Vocabulary) + " at " + GetTokenErrorDisplay(t);
            return msg;
        }

        protected virtual string ErrorMessageForNoViableAlternative(NoViableAltException e, Antlr4.Runtime.Parser recognizer)
        {
            ITokenStream tokens = ((ITokenStream)recognizer.InputStream);
            string input;
            if (tokens != null) {
                if (e.StartToken.Type == TokenConstants.Eof) {
                    input = "<EOF>";
                }
                else {
                    input = tokens.GetText(e.StartToken, e.OffendingToken);
                }
            }
            else {
                input = "<unknown input>";
            }
            string msg = "no viable alternative at input " + EscapeWSAndQuote(input);
            return msg;
        }

        protected virtual string ErrorMessageForInputMismatch(InputMismatchException e, Antlr4.Runtime.Parser recognizer)
        {
            string msg = "mismatched input " + GetTokenErrorDisplay(e.OffendingToken) + " expecting " + e.GetExpectedTokens().ToString(recognizer.Vocabulary);
            return msg;
        }

        protected virtual string ErrorMessageForFailedPredicate(FailedPredicateException e, Antlr4.Runtime.Parser recognizer)
        {
            string ruleName = recognizer.RuleNames[recognizer.RuleContext.RuleIndex];
            string msg = "rule " + ruleName + " " + e.Message;
            return msg;
        }

        // --- Please do not touch the methods below which reproduce exactly the default Antlr runtime behaviour --

        /// <summary>
        /// Single token deletion recovery strategy
        /// </summary>
        protected override IToken SingleTokenDeletion(Antlr4.Runtime.Parser recognizer)
        {
            Token lastConsumedToken = (Token)((ITokenStream)recognizer.InputStream).Lt(-1);
            Token nextToken = (Token)((ITokenStream)recognizer.InputStream).Lt(1);
            if (ErrorStrategyShouldNotConsumeThisToken(lastConsumedToken, nextToken))
            {
                return null;
            }
            else
            {
                return base.SingleTokenDeletion(recognizer);
            }
        }

        /// <summary>
        /// Multiple token deletion resynchronization strategy
        /// </summary>
        protected override void ConsumeUntil(Antlr4.Runtime.Parser recognizer, IntervalSet set)
        {
            Token lastConsumedToken = (Token)((ITokenStream)recognizer.InputStream).Lt(-1);
            Token nextToken = (Token)((ITokenStream)recognizer.InputStream).Lt(1);
            int ttype = nextToken.Type;
            while (ttype != TokenConstants.Eof && !set.Contains(ttype) &&
                   !ErrorStrategyShouldNotConsumeThisToken(lastConsumedToken, nextToken))
            {
                recognizer.Consume();
                lastConsumedToken = nextToken;
                nextToken = (Token)((ITokenStream)recognizer.InputStream).Lt(1);
                ttype = nextToken.Type;
            }
        }

        /// <summary>
        /// This is called by
        /// <see cref="ReportError(Parser, RecognitionException)"/>
        /// when the exception is a
        /// <see cref="NoViableAltException"/>
        /// </summary>
        protected override void ReportNoViableAlternative(Antlr4.Runtime.Parser recognizer, NoViableAltException e)
        {            
            string msg = ErrorMessageForNoViableAlternative(e, recognizer);
            NotifyErrorListeners(recognizer, msg, e);
        }
        
        /// <summary>
        /// This is called by
        /// <see cref="ReportError(Parser, RecognitionException)"/>
        /// when the exception is an
        /// <see cref="InputMismatchException"/>
        /// </summary>
        protected override void ReportInputMismatch(Antlr4.Runtime.Parser recognizer, InputMismatchException e)
        {
            string msg = ErrorMessageForInputMismatch(e, recognizer);
            NotifyErrorListeners(recognizer, msg, e);
        }

        /// <summary>
        /// This is called by
        /// <see cref="ReportError(Parser, RecognitionException)"/>
        /// when the exception is a
        /// <see cref="FailedPredicateException"/>
        /// </summary>
        protected override void ReportFailedPredicate(Antlr4.Runtime.Parser recognizer, FailedPredicateException e)
        {
            string msg = ErrorMessageForFailedPredicate(e, recognizer);
            NotifyErrorListeners(recognizer, msg, e);
        }

        /// <summary>
        /// This method is called to report a syntax error which requires the removal
        /// of a token from the input stream.
        /// </summary>
        protected override void ReportUnwantedToken(Antlr4.Runtime.Parser recognizer)
        {
            if (InErrorRecoveryMode(recognizer))
            {
                return;
            }
            BeginErrorCondition(recognizer);
            IToken t = recognizer.CurrentToken;
            string msg= ErrorMessageForUnexpectedToken(recognizer, t);
            recognizer.NotifyErrorListeners(t, msg, null);
        }
        
        /// <summary>
        /// This method is called to report a syntax error which requires the
        /// insertion of a missing token into the input stream.
        /// </summary>
        protected override void ReportMissingToken(Antlr4.Runtime.Parser recognizer)
        {
            if (InErrorRecoveryMode(recognizer))
            {
                return;
            }
            BeginErrorCondition(recognizer);
            IToken t = recognizer.CurrentToken;            
            string msg = ErrorMessageForMissingToken(recognizer, t);
            recognizer.NotifyErrorListeners(t, msg, null);
        }
    }

    /// <summary>
    /// Customized strategy in case of syntax error for the CompilerDirective parser :
    /// the idea is that we only want to avoid Antlr "eating away" the next token 
    /// when trying to resynchronize after an error 
    /// - if it is starting the next CompilerDirective or the next CodeElement 
    /// - if it is not on the same line (many compiler directives are written on a single line)
    /// - if it follows a PeriodSeparator token (a period always ends a compiler directive)
    /// </summary>
    public class CompilerDirectiveErrorStrategy : CobolErrorStrategy
    {
        /// <summary>
        /// Should return true for all the tokens we don't want to be consumed
        /// by the error recovery strategy when resynchronizing
        /// </summary>
        protected override bool ErrorStrategyShouldNotConsumeThisToken(Token lastConsumedToken, Token nextToken)
        {
            return nextToken.TokenFamily == TokenFamily.CompilerDirectiveStartingKeyword ||
                   nextToken.TokenFamily == TokenFamily.StatementStartingKeyword ||
                   nextToken.TokenFamily == TokenFamily.StatementEndingKeyword ||
                   nextToken.TokenFamily == TokenFamily.CodeElementStartingKeyword ||
                   (lastConsumedToken != null && (
                        nextToken.TokensLine != lastConsumedToken.TokensLine ||
                        lastConsumedToken.TokenType == TokenType.PeriodSeparator
                   ));
        }
    }


    /// <summary>
    /// Customized strategy in case of syntax error for the CodeElements parser :
    /// the idea is that we only want to avoid Antlr "eating away" the token 
    /// starting the next CodeElement when trying to resynchronize after an error
    /// </summary>
    public class CodeElementErrorStrategy : CobolErrorStrategy
    {
        /// <summary>
        /// Should return true for all the tokens we don't want to be consumed
        /// by the error recovery strategy when resynchronizing
        /// </summary>
        protected override bool ErrorStrategyShouldNotConsumeThisToken(Token lastConsumedToken, Token nextToken)
        {
            return nextToken.TokenFamily == TokenFamily.StatementStartingKeyword ||
                   nextToken.TokenFamily == TokenFamily.StatementEndingKeyword ||
                   nextToken.TokenFamily == TokenFamily.CodeElementStartingKeyword;
        }
    }
}
