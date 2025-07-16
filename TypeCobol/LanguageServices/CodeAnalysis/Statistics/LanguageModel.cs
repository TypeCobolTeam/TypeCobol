using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.LanguageServices.CodeAnalysis.Statistics
{
    /// <summary>
    /// Probabilistic model of the Cobol language, trained on a set of sample programs.
    /// Computes the most probable TokenType for the next word, given the TokenTypes of two previous words. 
    /// </summary>
    public class LanguageModel
    {
        private static readonly Lazy<LanguageModel> _defaultModelForProgram =
            new(() => LoadModel("TypeCobol.LanguageServices.CodeAnalysis.Statistics.LanguageModel.Program.txt"), LazyThreadSafetyMode.ExecutionAndPublication);

        /// <summary>
        /// Default language model for a Cobol program (trained on a set of traditional banking applications)
        /// </summary>
        public static LanguageModel DefaultModelForProgram => _defaultModelForProgram.Value;

        private static readonly Lazy<LanguageModel> _defaultModelForCopy =
            new(() => LoadModel("TypeCobol.LanguageServices.CodeAnalysis.Statistics.LanguageModel.Copy.txt"), LazyThreadSafetyMode.ExecutionAndPublication);

        /// <summary>
        /// Default language model for a Cobol copy (trained on a set of traditional banking applications)
        /// </summary>
        public static LanguageModel DefaultModelForCopy => _defaultModelForCopy.Value;

        private static LanguageModel LoadModel(string modelResourceName)
        {
            Stream embeddedFileStream = Assembly.GetExecutingAssembly().GetManifestResourceStream(modelResourceName);
            if (embeddedFileStream == null) return null;

            using StreamReader textStreamReader = new StreamReader(embeddedFileStream);
            return new LanguageModel(textStreamReader);
        }

        /// <summary>
        /// Learned parameters for the language model
        /// </summary>
        private IDictionary<TokenType, WordProbabilitiesAfterElementStartingWord> WordProbabilitiesAfterElementStartingWord;

        /// <summary>
        /// Load a language model in memory from a file on disk
        /// </summary>
        public LanguageModel(StreamReader textStreamReader)
        {
            WordProbabilitiesAfterElementStartingWord = new Dictionary<TokenType, WordProbabilitiesAfterElementStartingWord>();

            string line = null;
            while ((line = textStreamReader.ReadLine()) != null)
            {
                // load file here
            }
        }

        /// <summary>
        /// Constructor used internally by LanguageModelGenerator
        /// </summary>
        internal LanguageModel(IDictionary<TokenType, WordProbabilitiesAfterElementStartingWord> wordProbabilitiesAfterElementStartingWord)
        {
            WordProbabilitiesAfterElementStartingWord = wordProbabilitiesAfterElementStartingWord;
        }

        // Current position in the Tokens flow
        private TokenType lastElementStartingWord = TokenType.InvalidToken;
        private TokenType lastKeywordToken = TokenType.InvalidToken;
        private TokenType lastWord = TokenType.InvalidToken;
                
        /// <summary>
        /// Call the method each time you start using the model on a new program
        /// </summary>
        public void OnBeginProgram()
        {
            lastElementStartingWord = TokenType.InvalidToken;
            lastKeywordToken = TokenType.InvalidToken;
            lastWord = TokenType.InvalidToken;
        }
        
        /// <summary>
        /// Call this method each time you encounter a new Token while iterating
        /// on the program lines.
        /// It is not necessary to start iterating at the beginning of the program, 
        /// but you must start iterating before the last element starting
        /// keyword preceding the current token.
        /// </summary>
        public void OnToken(Token token)
        {
            TokenType tokenType = token.TokenType;
            TokenFamily tokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            if (IsSignificantWord(tokenType, tokenFamily))
            {
                if (IsKeywordToken(tokenType, tokenFamily))
                {
                    lastKeywordToken = lastWord;
                }            
                if (IsElementStartingWord(tokenType, tokenFamily, lastWord))
                {
                    lastElementStartingWord = tokenType;
                }
                lastWord = tokenType;
            }
        }

        internal static bool IsSignificantWord(TokenType tokenType, TokenFamily tokenFamily)
        {
            return tokenFamily != TokenFamily.Whitespace && tokenFamily != TokenFamily.Comments && tokenType != TokenType.COMPILER_DIRECTIVE &&
                   tokenType != TokenType.EJECT && tokenType != TokenType.SKIP1 && tokenType != TokenType.SKIP2 && tokenType != TokenType.SKIP3;
        }
        
        internal static bool IsKeywordToken(TokenType tokenType, TokenFamily tokenFamily)
        {
            return tokenFamily >= TokenFamily.CompilerDirectiveStartingKeyword &&
                   tokenFamily != TokenFamily.SpecialRegisterKeyword &&
                   tokenFamily != TokenFamily.FigurativeConstantKeyword &&
                   tokenFamily != TokenFamily.TypeCobolOperators;
        }

        internal static bool IsSymbolOrLiteral(TokenType tokenType, TokenFamily tokenFamily)
        {
            return tokenType == TokenType.UserDefinedWord || tokenType == TokenType.PartialCobolWord ||
                   tokenFamily == TokenFamily.NumericLiteral || tokenFamily == TokenFamily.AlphanumericLiteral;
        }

        internal static bool IsElementStartingWord(TokenType tokenType, TokenFamily tokenFamily, TokenType lastWord)
        {
            return tokenFamily == TokenFamily.CompilerDirectiveStartingKeyword || tokenFamily == TokenFamily.CodeElementStartingKeyword ||
                   tokenFamily == TokenFamily.SqlKeywords ||
                   tokenType == TokenType.LevelNumber || tokenType == TokenType.SectionParagraphName;
        }

        internal static string WordKeyToString(int currentWordKey)
        {
            string result = String.Empty;
            if(currentWordKey >= 1024)
            {
                TokenType lastWordBeforeSymbolOrLiteral = (TokenType)(currentWordKey >> 10);
                result = Enum.GetName(typeof(TokenType), lastWordBeforeSymbolOrLiteral) + ">";
                currentWordKey = currentWordKey & 1023;
            }
            TokenType lastWord = (TokenType)currentWordKey;
            result += Enum.GetName(typeof(TokenType), lastWord);
            return result;
        }

        /// <summary>
        /// Returns the most probable TokenTypes for the next Token 
        /// given the current position in the tokens flow
        /// </summary>
        public IList<WordProbability> NextWordsProbabilities()
        {
            WordProbabilitiesAfterElementStartingWord wordProbabilities = null;
            if (WordProbabilitiesAfterElementStartingWord.TryGetValue(lastElementStartingWord, out wordProbabilities))
            {
                return wordProbabilities.NextWordsProbability(lastWord, lastKeywordToken);
            }
            else
            {
                return null;
            }
        }
    }
}
