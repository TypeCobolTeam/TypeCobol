using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    // --> TO DO : need to customize this strategy ! --

    /// <summary>
    /// Customized strategy in case of syntax error for the Cobol parser
    /// </summary>
    public class CobolErrorStrategy : DefaultErrorStrategy
    {
        /// <summary>
        /// When the parser encounters an invalid token before the end of the rule :
        /// consume all tokens until a PeriodSeparator, the end of the line, or the next statement starting keyword.
        /// </summary>
        public override void Recover(Antlr4.Runtime.Parser recognizer, RecognitionException e)
        {
            if (lastErrorIndex == ((ITokenStream)recognizer.InputStream).Index && lastErrorStates != null && lastErrorStates.Contains(recognizer.State))
            {
                // uh oh, another error at same token index and previously-visited
                // state in ATN; must be a case where LT(1) is in the recovery
                // token set so nothing got consumed. Consume a single token
                // at least to prevent an infinite loop; this is a failsafe.
                recognizer.Consume();
            }
            lastErrorIndex = ((ITokenStream)recognizer.InputStream).Index;
            if (lastErrorStates == null)
            {
                lastErrorStates = new IntervalSet();
            }
            lastErrorStates.Add(recognizer.State);

            // Consume until next statement starting keyword (excluded), PeriodSeparator (included), or the end of line
            IToken lastConsumedToken = ((ITokenStream)recognizer.InputStream).Lt(-1);
            IToken currentInvalidToken = ((ITokenStream)recognizer.InputStream).Lt(1);
            while ((lastConsumedToken == null || currentInvalidToken.Line == lastConsumedToken.Line) && currentInvalidToken.Type != TokenConstants.Eof)
            {
                if (((Token)currentInvalidToken).TokenFamily == TokenFamily.StatementStartingKeyword ||
                    ((Token)currentInvalidToken).TokenFamily == TokenFamily.StatementEndingKeyword ||
                    ((Token)currentInvalidToken).TokenFamily == TokenFamily.CodeElementStartingKeyword)
                {
                    break;
                }
                recognizer.Consume();
                if (currentInvalidToken.Type == (int)TokenType.PeriodSeparator)
                {
                    break;
                }
                currentInvalidToken = ((ITokenStream)recognizer.InputStream).Lt(1);
            }
        }

        /// <summary>
        /// When parsing a compiler directive, single token deletion should never
        /// eat the statement starting keyword 
        /// </summary>
        public override IToken RecoverInline(Antlr4.Runtime.Parser recognizer)
        {
            // SINGLE TOKEN DELETION
            Token nextToken = (Token)((ITokenStream)recognizer.InputStream).Lt(1);
            if (nextToken.TokenFamily != TokenFamily.StatementStartingKeyword &&
                nextToken.TokenFamily != TokenFamily.StatementEndingKeyword &&
                nextToken.TokenFamily != TokenFamily.CodeElementStartingKeyword)
            {
                IToken matchedSymbol = SingleTokenDeletion(recognizer);
                if (matchedSymbol != null)
                {
                    // we have deleted the extra token.
                    // now, move past ttype token as if all were ok
                    recognizer.Consume();
                    return matchedSymbol;
                }
            }
            // SINGLE TOKEN INSERTION
            if (SingleTokenInsertion(recognizer))
            {
                return GetMissingSymbol(recognizer);
            }
            // even that didn't work; must throw the exception
            throw new InputMismatchException(recognizer);
        }

        /// <summary>
        /// If we attempt attempt to recover from problems while matching subrules,
        /// we end up consuming the whole file for matching one compiler directive
        /// </summary>
        public override void Sync(Antlr4.Runtime.Parser recognizer)
        {
        }
    }
}
