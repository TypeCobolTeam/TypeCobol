using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.LanguageServices.CodeAnalysis.Statistics
{
    // -- Internal classes for LanguageModel --

    /// <summary>
    /// Probabilities of all words following and CodeElement starting keyword
    /// </summary>
    class WordProbabilitiesAfterElementStartingWord
    {
        public TokenType ElementStartingWordType;

        public long TotalCount;
        public IDictionary<int, NextWordProbabilities> WordProbabilities;

        // Sorting order in the model file only
        internal long ElementCount = 0;
        internal long ElementIndexes = 0;

        public WordProbabilitiesAfterElementStartingWord(TokenType elementStartingWordType)
        {
            ElementStartingWordType = elementStartingWordType;
            WordProbabilities = new Dictionary<int, NextWordProbabilities>();
        }

        internal void OnElementStartingWord(int elementStartingWordIndexInProgram)
        {
            ElementCount++;
            ElementIndexes += elementStartingWordIndexInProgram;
        }

        public void OnWords(TokenType firstWord, TokenType secondWord, SymbolInformation secondWordSymbolInfo, TokenType lastWordBeforeSymbolOrLiteral, int secondWordIndexInElement)
        {
            TotalCount++;
            int firstWordKey = ComputeLastWordsKey(firstWord, lastWordBeforeSymbolOrLiteral);
            NextWordProbabilities nextWordProbabilities = null;
            if (!WordProbabilities.TryGetValue(firstWordKey, out nextWordProbabilities))
            {
                nextWordProbabilities = new NextWordProbabilities(firstWordKey);
                WordProbabilities.Add(firstWordKey, nextWordProbabilities);
            }
            nextWordProbabilities.OnNextWord(secondWord, secondWordSymbolInfo, secondWordIndexInElement);
        }

        private static int ComputeLastWordsKey(TokenType lastWord, TokenType lastWordBeforeSymbolOrLiteral)
        {
            int lastWordKey = 0;
            if (LanguageModel.IsSymbolOrLiteral(lastWord, TokenUtils.GetTokenFamilyFromTokenType(lastWord)) || lastWord == TokenType.PeriodSeparator)
            {
                lastWordKey = (int)lastWordBeforeSymbolOrLiteral << 10;
            }
            lastWordKey += (int)lastWord;
            return lastWordKey;
        }

        public void ComputeWordProbabilities()
        {
            foreach (var nextWordProbabilities in WordProbabilities.Values)
            {
                nextWordProbabilities.ComputeNextWords();
            }
        }

        public IList<WordProbability> NextWordsProbability(TokenType lastWord, TokenType lastWordBeforeSymbolOrLiteral)
        {
            int lastWordKey = ComputeLastWordsKey(lastWord, lastWordBeforeSymbolOrLiteral);
            NextWordProbabilities wordProbabilities = null;
            if (WordProbabilities.TryGetValue(lastWordKey, out wordProbabilities))
            {
                return wordProbabilities.NextWords;
            }
            else
            {
                return null;
            }
        }
    }

    /// <summary>
    /// Probabilities of the TokenTypes for the next word
    /// </summary>
    class NextWordProbabilities
    {
        public int CurrentWordKey;

        public long TotalCount;
        private IDictionary<TokenType, long> NextWordCounts;
        private IDictionary<TokenType, IList<SymbolType>> NextWordSymbolTypes;

        public IList<WordProbability> NextWords;

        // Sorting order in the model file only
        internal long CurrentWordIndexes = 0;

        public NextWordProbabilities(int currentWordKey)
        {
            CurrentWordKey = currentWordKey;
            NextWordCounts = new Dictionary<TokenType, long>();
        }

        public void OnNextWord(TokenType nextWordType, SymbolInformation nextWordSymbolInfo, int secondWordIndexInElement)
        {
            TotalCount++;
            CurrentWordIndexes += secondWordIndexInElement - 1;
            if (NextWordCounts.ContainsKey(nextWordType))
            {
                NextWordCounts[nextWordType] = NextWordCounts[nextWordType] + 1;
            }
            else
            {
                NextWordCounts.Add(nextWordType, 1);
            }
            if (nextWordSymbolInfo != null)
            {
                if (NextWordSymbolTypes == null)
                {
                    NextWordSymbolTypes = new Dictionary<TokenType, IList<SymbolType>>();
                }
                IList<SymbolType> candidateSymbolTypes = null;
                if (!NextWordSymbolTypes.TryGetValue(nextWordType, out candidateSymbolTypes))
                {
                    candidateSymbolTypes = new List<SymbolType>();
                    NextWordSymbolTypes.Add(nextWordType, candidateSymbolTypes);
                }

                var ambiguous = nextWordSymbolInfo as AmbiguousSymbolReference;
                if (ambiguous != null && ambiguous.CandidateTypes != null) {
                    foreach(var symbolType in ambiguous.CandidateTypes) {
                        if (!candidateSymbolTypes.Contains(symbolType))
                            candidateSymbolTypes.Add(symbolType);
                    }
                } else {
                    if (!candidateSymbolTypes.Contains(nextWordSymbolInfo.Type))
                        candidateSymbolTypes.Add(nextWordSymbolInfo.Type);
                }
            }
        }

        public void ComputeNextWords()
        {
            NextWords = new List<WordProbability>();
            foreach (var tokenType in NextWordCounts.Keys.OrderByDescending(tokenType => NextWordCounts[tokenType]))
            {
                long nextWordCount = NextWordCounts[tokenType];
                double probability = nextWordCount / (double)TotalCount;
                if (probability >= 0.02)
                {
                    SymbolType[] symbolTypes = null;
                    if (NextWordSymbolTypes != null)
                    {
                        if (NextWordSymbolTypes.ContainsKey(tokenType))
                        {
                            symbolTypes = NextWordSymbolTypes[tokenType].ToArray();
                        }
                    }
                    NextWords.Add(new WordProbability(tokenType, symbolTypes, probability));
                }
                else
                {
                    break;
                }
            }
            NextWordCounts = null;
        }
    }

    /// <summary>
    /// Probability of one single TokenType
    /// </summary>
    public class WordProbability
    {
        public TokenType WordType;
        public SymbolType[] SymbolTypes;
        public double Probability;

        public WordProbability(TokenType wordType, SymbolType[] symbolTypes, double probability)
        {
            WordType = wordType;
            SymbolTypes = symbolTypes;
            Probability = probability;
        }
    }
}
