using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
namespace TypeCobol.Compiler.SqlScanner
{
    /**
     * Parses a stream into a set of defined tokens, one at a time. The different
     * types of tokens that can be found are numbers, identifiers, quoted strings,
     * and different comment styles. The class can be used for limited processing
     * of source code of programming languages like Java, although it is nowhere
     * near a full parser.
     */
    public class StreamTokenizer
    {
        /**
         * Contains a number if the current token is a number ({@code ttype} ==
         * {@code TT_NUMBER}).
         */
        public double nval;

        /**
         * Contains a string if the current token is a word ({@code ttype} ==
         * {@code TT_WORD}).
         */
        public String sval;

        /**
         * The constant representing the end of the stream.
         */
        public const int TT_EOF = -1;

        /**
         * The constant representing the end of the line.
         */
        public const int TT_EOL = '\n';

        /**
         * The constant representing a number token.
         */
        public const int TT_NUMBER = -2;

        /**
         * The constant representing a word token.
         */
        public const int TT_WORD = -3;

        /**
         * Internal representation of unknown state.
         */
        private const int TT_UNKNOWN = -4;

        /**
         * After calling {@code nextToken()}, {@code ttype} contains the type of
         * token that has been read. When a single character is read, its value
         * converted to an integer is stored in {@code ttype}. For a quoted string,
         * the value is the quoted character. Otherwise, its value is one of the
         * following:
         * <ul>
         * <li> {@code TT_WORD} - the token is a word.</li>
         * <li> {@code TT_NUMBER} - the token is a number.</li>
         * <li> {@code TT_EOL} - the end of line has been reached. Depends on
         * whether {@code eolIsSignificant} is {@code true}.</li>
         * <li> {@code TT_EOF} - the end of the stream has been reached.</li>
         * </ul>
         */
        public int ttype = TT_UNKNOWN;

        /**
         * Internal character meanings, 0 implies TOKEN_ORDINARY
         */
        private byte[] tokenTypes = new byte[256];

        private const byte TOKEN_COMMENT = 1;

        private const byte TOKEN_QUOTE = 2;

        private const byte TOKEN_WHITE = 4;

        private const byte TOKEN_WORD = 8;

        private const byte TOKEN_DIGIT = 16;

        private int LineNumber = 1;
        private int ColumnNumber = -1;

        private bool forceLowercase;

        private bool isEOLSignificant;

        private bool slashStarComments;

        private bool slashSlashComments;

        private bool pushBackToken;

        private bool lastCr;

        /* One of these will have the stream */

        private char[] Buffer;

        /// <summary>
        /// Pos in the buffer
        /// </summary>
        private int pos;

        private int peekChar = -2;

        /**
         * Private constructor to initialize the default values according to the
         * specification.
         */
        private StreamTokenizer()
        {
            /*
             * Initialize the default state per specification. All byte values 'A'
             * through 'Z', 'a' through 'z', and '\u00A0' through '\u00FF' are
             * considered to be alphabetic.
             */
            WordChars('A', 'Z');
            WordChars('a', 'z');
            WordChars(160, 255);
            /**
             * All byte values '\u0000' through '\u0020' are considered to be white
             * space.
             */
            WhitespaceChars(0, 32);
            /**
             * '/' is a comment character. Single quote '\'' and double quote '"'
             * are string quote characters.
             */
            CommentChar('/');
            QuoteChar('"');
            QuoteChar('\'');
            /**
             * Numbers are parsed.
             */
            ParseNumbers();
            /**
             * Ends of lines are treated as white space, not as separate tokens.
             * C-style and C++-style comments are not recognized. These are the
             * defaults and are not needed in constructor.
             */
        }

        /**
         * Constructs a new {@code StreamTokenizer} with {@code r} as source reader.
         * The tokenizer's initial state is as follows:
         * <ul>
         * <li>All byte values 'A' through 'Z', 'a' through 'z', and '&#92;u00A0'
         * through '&#92;u00FF' are considered to be alphabetic.</li>
         * <li>All byte values '&#92;u0000' through '&#92;u0020' are considered to
         * be white space. '/' is a comment character.</li>
         * <li>Single quote '\'' and double quote '"' are string quote characters.
         * </li>
         * <li>Numbers are parsed.</li>
         * <li>End of lines are considered to be white space rather than separate
         * tokens.</li>
         * <li>C-style and C++-style comments are not recognized.</LI>
         * </ul>
         * 
         * @param r
         *            the source reader from which to parse tokens.
         */
        public StreamTokenizer(char[] buffer) : this()
        {
            pos = 0;
            Buffer = buffer;
        }

        /**
         * Specifies that the character {@code ch} shall be treated as a comment
         * character.
         * 
         * @param ch
         *            the character to be considered a comment character.
         */
        public void CommentChar(int ch)
        {
            if (0 <= ch && ch < tokenTypes.Length)
            {
                tokenTypes[ch] = TOKEN_COMMENT;
            }
        }

        /**
         * Specifies whether the end of a line is significant and should be returned
         * as {@code TT_EOF} in {@code ttype} by this tokenizer.
         * 
         * @param flag
         *            {@code true} if EOL is significant, {@code false} otherwise.
         */
        public void EolIsSignificant(bool flag)
        {
            isEOLSignificant = flag;
        }

        /**
         * Returns the current line number.
         * 
         * @return this tokenizer's current line number.
         */
        public int Lineno()
        {
            return LineNumber;
        }

        /**
         * Specifies whether word tokens should be converted to lower case when they
         * are stored in {@code sval}.
         * 
         * @param flag
         *            {@code true} if {@code sval} should be converted to lower
         *            case, {@code false} otherwise.
         */
        public void LowerCaseMode(bool flag)
        {
            forceLowercase = flag;
        }

        /**
         * Parses the next token from this tokenizer's source stream or reader. The
         * type of the token is stored in the {@code ttype} field, additional
         * information may be stored in the {@code nval} or {@code sval} fields.
         * 
         * @return the value of {@code ttype}.
         * @throws IOException
         *             if an I/O error occurs while parsing the next token.
         */
        public int NextToken(out int lineNumber, out int starColumn, out int endColumn)
        {
            lineNumber = LineNumber;
            starColumn = ColumnNumber;
            endColumn = ColumnNumber;
            if (pushBackToken)
            {
                pushBackToken = false;
                if (ttype != TT_UNKNOWN)
                {
                    return ttype;
                }
            }
            sval = null; // Always reset sval to null
            int currentChar = peekChar == -2 ? Read() : peekChar;
            starColumn = ColumnNumber;
            endColumn = ColumnNumber;


            if (lastCr && currentChar == '\n')
            {
                lastCr = false;
                currentChar = Read();
            }
            if (currentChar == -1)
            {
                return (ttype = TT_EOF);
            }

            byte currentType = currentChar > 255 ? TOKEN_WORD
                    : tokenTypes[currentChar];
            while ((currentType & TOKEN_WHITE) != 0)
            {
                /**
                 * Skip over white space until we hit a new line or a real token
                 */
                if (currentChar == '\r')
                {                    
                    LineNumber++;
                    ColumnNumber = 1;
                    starColumn = endColumn = ColumnNumber;
                    lineNumber = LineNumber;
                    if (isEOLSignificant)
                    {
                        lastCr = true;
                        peekChar = -2;
                        return (ttype = TT_EOL);
                    }
                    if ((currentChar = Read()) == '\n')
                    {
                        currentChar = Read();
                    }
                }
                else if (currentChar == '\n')
                {
                    LineNumber++;
                    ColumnNumber = 1;
                    starColumn = endColumn = ColumnNumber;
                    lineNumber = LineNumber;
                    if (isEOLSignificant)
                    {
                        peekChar = -2;
                        return (ttype = TT_EOL);
                    }
                    currentChar = Read();
                }
                else
                {
                    // Advance over this white space character and try again.
                    currentChar = Read();
                }
                if (currentChar == -1)
                {
                    endColumn = ColumnNumber;
                    return (ttype = TT_EOF);
                }
                currentType = currentChar > 255 ? TOKEN_WORD
                        : tokenTypes[currentChar];
            }
            starColumn = ColumnNumber;
            /**
             * Check for digits before checking for words since digits can be
             * contained within words.
             */
            if ((currentType & TOKEN_DIGIT) != 0)
            {
                StringBuilder digits = new StringBuilder(20);
                bool haveDecimal = false, checkJustNegative = currentChar == '-';
                while (true)
                {
                    if (currentChar == '.')
                    {
                        haveDecimal = true;
                    }
                    digits.Append((char)currentChar);
                    currentChar = Read();
                    if ((currentChar < '0' || currentChar > '9')
                            && (haveDecimal || currentChar != '.'))
                    {
                        break;
                    }
                }
                peekChar = currentChar;
                if (checkJustNegative && digits.Length == 1)
                {
                    // Didn't get any other digits other than '-'
                    return (ttype = '-');
                }
                try
                {
                    nval = Double.Parse(digits.ToString());
                }
                catch (FormatException)
                {
                    // Unsure what to do, will write test.
                    nval = 0;
                }
                endColumn = ColumnNumber;
                return (ttype = TT_NUMBER);
            }
            // Check for words
            if ((currentType & TOKEN_WORD) != 0)
            {
                starColumn = ColumnNumber;
                StringBuilder word = new StringBuilder(20);
                while (true)
                {
                    word.Append((char)currentChar);
                    currentChar = Read();
                    if (currentChar == -1
                            || (currentChar < 256 && (tokenTypes[currentChar] & (TOKEN_WORD | TOKEN_DIGIT)) == 0))
                    {
                        break;
                    }
                }
                peekChar = currentChar;
                sval = forceLowercase ? word.ToString().ToUpper() : word
                       .ToString();
                endColumn = ColumnNumber;
                return (ttype = TT_WORD);
            }
            // Check for quoted character
            if (currentType == TOKEN_QUOTE)
            {
                starColumn = ColumnNumber;
                int matchQuote = currentChar;
                StringBuilder quoteString = new StringBuilder();
                int peekOne = Read();
                while (peekOne >= 0 && peekOne != matchQuote && peekOne != '\r'
                        && peekOne != '\n')
                {
                    bool readPeek = true;
                    if (peekOne == '\\')
                    {
                        int c1 = Read();
                        // Check for quoted octal IE: \377
                        if (c1 <= '7' && c1 >= '0')
                        {
                            int digitValue = c1 - '0';
                            c1 = Read();
                            if (c1 > '7' || c1 < '0')
                            {
                                readPeek = false;
                            }
                            else
                            {
                                digitValue = digitValue * 8 + (c1 - '0');
                                c1 = Read();
                                // limit the digit value to a byte
                                if (digitValue > 037 || c1 > '7' || c1 < '0')
                                {
                                    readPeek = false;
                                }
                                else
                                {
                                    digitValue = digitValue * 8 + (c1 - '0');
                                }
                            }
                            if (!readPeek)
                            {
                                // We've consumed one to many
                                quoteString.Append((char)digitValue);
                                peekOne = c1;
                            }
                            else
                            {
                                peekOne = digitValue;
                            }
                        }
                        else
                        {
                            switch (c1)
                            {
                                case 'a':
                                    peekOne = 0x7;
                                    break;
                                case 'b':
                                    peekOne = 0x8;
                                    break;
                                case 'f':
                                    peekOne = 0xc;
                                    break;
                                case 'n':
                                    peekOne = 0xA;
                                    break;
                                case 'r':
                                    peekOne = 0xD;
                                    break;
                                case 't':
                                    peekOne = 0x9;
                                    break;
                                case 'v':
                                    peekOne = 0xB;
                                    break;
                                default:
                                    peekOne = c1;
                                    break;
                            }
                        }
                    }
                    if (readPeek)
                    {
                        quoteString.Append((char)peekOne);
                        peekOne = Read();
                    }
                }
                if (peekOne == matchQuote)
                {
                    peekOne = Read();
                }
                peekChar = peekOne;
                ttype = matchQuote;
                sval = quoteString.ToString();
                endColumn = ColumnNumber;
                return ttype;
            }
            // Do comments, both "//" and "/*stuff*/"
            if (currentChar == '/' && (slashSlashComments || slashStarComments))
            {
                if ((currentChar = Read()) == '*' && slashStarComments)
                {
                    int peekOne = Read();
                    while (true)
                    {
                        currentChar = peekOne;
                        peekOne = Read();
                        if (currentChar == -1)
                        {
                            peekChar = -1;
                            return (ttype = TT_EOF);
                        }
                        if (currentChar == '\r')
                        {
                            if (peekOne == '\n')
                            {
                                peekOne = Read();
                            }
                            LineNumber++;
                            ColumnNumber = 1;
                        }
                        else if (currentChar == '\n')
                        {
                            LineNumber++;
                            ColumnNumber = 1;
                        }
                        else if (currentChar == '*' && peekOne == '/')
                        {
                            peekChar = Read();
                            return NextToken(out lineNumber, out starColumn, out endColumn);
                        }
                    }
                }
                else if (currentChar == '/' && slashSlashComments)
                {
                    // Skip to EOF or new line then return the next token
                    while ((currentChar = Read()) >= 0 && currentChar != '\r'
                            && currentChar != '\n')
                    {
                        // Intentionally empty
                    }
                    peekChar = currentChar;
                    return NextToken(out lineNumber, out starColumn, out endColumn);
                }
                else if (currentType != TOKEN_COMMENT)
                {
                    // Was just a slash by itself
                    peekChar = currentChar;
                    return (ttype = '/');
                }
            }
            // Check for comment character
            if (currentType == TOKEN_COMMENT)
            {
                // Skip to EOF or new line then return the next token
                while ((currentChar = Read()) >= 0 && currentChar != '\r'
                        && currentChar != '\n')
                {
                    // Intentionally empty
                }
                peekChar = currentChar;
                return NextToken(out lineNumber, out starColumn, out endColumn);
            }

            peekChar = Read();
            return (ttype = currentChar);
        }

        /**
         * Specifies that the character {@code ch} shall be treated as an ordinary
         * character by this tokenizer. That is, it has no special meaning as a
         * comment character, word component, white space, string delimiter or
         * number.
         * 
         * @param ch
         *            the character to be considered an ordinary character.
         */
        public void OrdinaryChar(int ch)
        {
            if (0 <= ch && ch < tokenTypes.Length)
            {
                tokenTypes[ch] = 0;
            }
        }

        /**
         * Specifies that the characters in the range from {@code low} to {@code hi}
         * shall be treated as an ordinary character by this tokenizer. That is,
         * they have no special meaning as a comment character, word component,
         * white space, string delimiter or number.
         * 
         * @param low
         *            the first character in the range of ordinary characters.
         * @param hi
         *            the last character in the range of ordinary characters.
         */
        public void OrdinaryChars(int low, int hi)
        {
            if (low < 0)
            {
                low = 0;
            }
            if (hi > tokenTypes.Length)
            {
                hi = tokenTypes.Length - 1;
            }
            for (int i = low; i <= hi; i++)
            {
                tokenTypes[i] = 0;
            }
        }

        /**
         * Specifies that this tokenizer shall parse numbers.
         */
        public void ParseNumbers()
        {
            for (int i = '0'; i <= '9'; i++)
            {
                tokenTypes[i] |= TOKEN_DIGIT;
            }
            tokenTypes['.'] |= TOKEN_DIGIT;
            tokenTypes['-'] |= TOKEN_DIGIT;
        }

        /**
         * Indicates that the current token should be pushed back and returned again
         * the next time {@code nextToken()} is called.
         */
        public void PushBack()
        {
            pushBackToken = true;
        }

        /**
         * Specifies that the character {@code ch} shall be treated as a quote
         * character.
         * 
         * @param ch
         *            the character to be considered a quote character.
         */
        public void QuoteChar(int ch)
        {
            if (0 <= ch && ch < tokenTypes.Length)
            {
                tokenTypes[ch] = TOKEN_QUOTE;
            }
        }

        private int Read()
        {
            if (pos < Buffer.Length)
            {
                ColumnNumber++;
                return Buffer[pos++];
            }
            return -1;
        }

        public struct Context
        {
            public int Pos;
            public int LineNumber;
            public int ColumnNumber;
            public Context(int pos, int lineNumber, int columnNumber)
            {
                this.Pos = pos;
                this.LineNumber = lineNumber;
                this.ColumnNumber = columnNumber;
            }
        }

        /// <summary>
        /// Get the current scanning context
        /// </summary>
        /// <returns>The scanning context</returns>
        public Context GetCurrentContext()
        {
            return new Context(pos, LineNumber, ColumnNumber);
        }

        /// <summary>
        /// Restore a previously get scanning context.
        /// </summary>
        /// <param name="ctx"></param>
        public void SetContext(Context ctx)
        {
            pos = ctx.Pos;
            LineNumber = ctx.LineNumber;
            ColumnNumber = ctx.ColumnNumber;
        }

        /**
         * Specifies that all characters shall be treated as ordinary characters.
         */
        public void ResetSyntax()
        {
            for (int i = 0; i < 256; i++)
            {
                tokenTypes[i] = 0;
            }
        }

        /**
         * Specifies whether "slash-slash" (C++-style) comments shall be recognized.
         * This kind of comment ends at the end of the line.
         * 
         * @param flag
         *            {@code true} if {@code //} should be recognized as the start
         *            of a comment, {@code false} otherwise.
         */
        public void SlashSlashComments(bool flag)
        {
            slashSlashComments = flag;
        }

        /**
         * Specifies whether "slash-star" (C-style) comments shall be recognized.
         * Slash-star comments cannot be nested and end when a star-slash
         * combination is found.
         * 
         * @param flag
         *            {@code true} if {@code /*} should be recognized as the start
         *            of a comment, {@code false} otherwise.
         */
        public void SlashStarComments(bool flag)
        {
            slashStarComments = flag;
        }

        /**
         * Returns the state of this tokenizer in a readable format.
         * 
         * @return the current state of this tokenizer.
         */
        public override String ToString()
        {
            // Values determined through experimentation
            StringBuilder result = new StringBuilder();
            result.Append("Token["); //$NON-NLS-1$
            switch (ttype)
            {
                case TT_EOF:
                    result.Append("EOF"); //$NON-NLS-1$
                    break;
                case TT_EOL:
                    result.Append("EOL"); //$NON-NLS-1$
                    break;
                case TT_NUMBER:
                    result.Append("n="); //$NON-NLS-1$
                    result.Append(nval);
                    break;
                case TT_WORD:
                    result.Append(sval);
                    break;
                default:
                    if (ttype == TT_UNKNOWN || tokenTypes[ttype] == TOKEN_QUOTE)
                    {
                        result.Append(sval);
                    }
                    else
                    {
                        result.Append('\'');
                        result.Append((char)ttype);
                        result.Append('\'');
                    }
                    break;
            }
            result.Append("], line "); //$NON-NLS-1$
            result.Append(LineNumber);
            return result.ToString();
        }

        /**
         * Specifies that the characters in the range from {@code low} to {@code hi}
         * shall be treated as whitespace characters by this tokenizer.
         * 
         * @param low
         *            the first character in the range of whitespace characters.
         * @param hi
         *            the last character in the range of whitespace characters.
         */
        public void WhitespaceChars(int low, int hi)
        {
            if (low < 0)
            {
                low = 0;
            }
            if (hi > tokenTypes.Length)
            {
                hi = tokenTypes.Length - 1;
            }
            for (int i = low; i <= hi; i++)
            {
                tokenTypes[i] = TOKEN_WHITE;
            }
        }

        /**
         * Specifies that the characters in the range from {@code low} to {@code hi}
         * shall be treated as word characters by this tokenizer. A word consists of
         * a word character followed by zero or more word or number characters.
         * 
         * @param low
         *            the first character in the range of word characters.
         * @param hi
         *            the last character in the range of word characters.
         */
        public void WordChars(int low, int hi)
        {
            if (low < 0)
            {
                low = 0;
            }
            if (hi > tokenTypes.Length)
            {
                hi = tokenTypes.Length - 1;
            }
            for (int i = low; i <= hi; i++)
            {
                tokenTypes[i] |= TOKEN_WORD;
            }
        }
    }
}

