using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Symbols;
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
        public IDictionary<TokenType, NextWordProbabilities> WordProbabilities;

        public WordProbabilitiesAfterElementStartingWord(TokenType elementStartingWordType)
        {
            ElementStartingWordType = elementStartingWordType;
            WordProbabilities = new Dictionary<TokenType, NextWordProbabilities>();
        }

        public void OnWords(TokenType firstWord, TokenType secondWord, SymbolInformation secondWordSymbolInfo)
        {
            TotalCount++;
            NextWordProbabilities nextWordProbabilities = null;
            if (!WordProbabilities.TryGetValue(firstWord, out nextWordProbabilities))
            {
                nextWordProbabilities = new NextWordProbabilities(firstWord);
                WordProbabilities.Add(firstWord, nextWordProbabilities);
            }
            nextWordProbabilities.OnNextWord(secondWord, secondWordSymbolInfo);
        }

        public void ComputeWordProbabilities()
        {
            foreach (var nextWordProbabilities in WordProbabilities.Values)
            {
                nextWordProbabilities.ComputeNextWords();
            }
        }

        public IList<WordProbability> NextWordsProbability(TokenType lastWordType)
        {
            NextWordProbabilities wordProbabilities = null;
            if (WordProbabilities.TryGetValue(lastWordType, out wordProbabilities))
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
        public TokenType CurrentWordType;

        public long TotalCount;
        private IDictionary<TokenType, long> NextWordCounts;
        private IDictionary<TokenType, IList<SymbolType>> NextWordSymbolTypes;

        public IList<WordProbability> NextWords;

        public NextWordProbabilities(TokenType currentTokenType)
        {
            CurrentWordType = currentTokenType;
            NextWordCounts = new Dictionary<TokenType, long>();
        }

        public void OnNextWord(TokenType nextWordType, SymbolInformation nextWordSymbolInfo)
        {
            TotalCount++;
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
                if (nextWordSymbolInfo.CandidateSymbolTypes != null)
                {
                    foreach (var symbolType in nextWordSymbolInfo.CandidateSymbolTypes)
                    {
                        if (!candidateSymbolTypes.Contains(symbolType))
                        {
                            candidateSymbolTypes.Add(symbolType);
                        }
                    }
                }
                else
                {
                    if (!candidateSymbolTypes.Contains(nextWordSymbolInfo.Type))
                    {
                        candidateSymbolTypes.Add(nextWordSymbolInfo.Type);
                    }
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
                if (probability >= 0.05)
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
