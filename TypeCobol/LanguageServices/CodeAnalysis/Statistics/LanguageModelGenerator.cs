using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Symbols;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.LanguageServices.CodeAnalysis.Statistics
{
    /// <summary>
    /// Probabilistic model of the Cobol language, trained on a set of sample programs.
    /// Computes the most probable TokenType for the next word, given the TokenTypes of two previous words. 
    /// </summary>
    class LanguageModelGenerator
    {
        private long TotalCount;
        public IDictionary<TokenType, WordProbabilitiesAfterElementStartingWord> WordProbabilitiesAfterElementStartingWord = new Dictionary<TokenType, WordProbabilitiesAfterElementStartingWord>();

        private TokenType lastElementStartingWord = TokenType.InvalidToken;
        private TokenType lastWord = TokenType.InvalidToken;

        private IDictionary<Token, SymbolInformation> symbolInformationForTokens;

        public void OnBeginProgram()
        {
            lastElementStartingWord = TokenType.InvalidToken;
            lastWord = TokenType.InvalidToken;
            symbolInformationForTokens = new Dictionary<Token, SymbolInformation>();
        }

        public void AddSymbolInformationForTokens(IDictionary<Token, SymbolInformation> additionalSymbolInformationForTokens)
        {
            foreach (var pair in additionalSymbolInformationForTokens)
            {
                symbolInformationForTokens[pair.Key] = pair.Value;
            }
        }

        public void OnToken(Token token)
        {
            TokenType tokenType = token.TokenType;
            TokenFamily tokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            if (tokenFamily != TokenFamily.Whitespace && tokenFamily != TokenFamily.Comments && tokenType != TokenType.CompilerDirective &&
                tokenType != TokenType.EJECT && tokenType != TokenType.SKIP1 && tokenType != TokenType.SKIP2 && tokenType != TokenType.SKIP3)
            {
                RegisterToken(token);
                lastWord = tokenType;
                if (tokenFamily == TokenFamily.CompilerDirectiveStartingKeyword || tokenFamily == TokenFamily.CodeElementStartingKeyword || tokenFamily == TokenFamily.StatementStartingKeyword || tokenFamily == TokenFamily.StatementEndingKeyword)
                {
                    lastElementStartingWord = tokenType;
                }
            }
        }

        private void RegisterToken(Token token)
        {
            TotalCount++;
            WordProbabilitiesAfterElementStartingWord wordProbabilities = null;
            if (!WordProbabilitiesAfterElementStartingWord.TryGetValue(lastElementStartingWord, out wordProbabilities))
            {
                wordProbabilities = new WordProbabilitiesAfterElementStartingWord(lastElementStartingWord);
                WordProbabilitiesAfterElementStartingWord.Add(lastElementStartingWord, wordProbabilities);
            }

            SymbolInformation symbolInfo = null;
            symbolInformationForTokens.TryGetValue(token, out symbolInfo);
            wordProbabilities.OnWords(lastWord, token.TokenType, symbolInfo);
        }

        public LanguageModel ComputeProbabilities()
        {
            foreach (var wordProbabilities in WordProbabilitiesAfterElementStartingWord.Values)
            {
                wordProbabilities.ComputeWordProbabilities();
            }
            return new LanguageModel(WordProbabilitiesAfterElementStartingWord);
        }

        public void WriteModelFile(StreamWriter modelFile, TextWriter console)
        {
            double top1Prediction = 0;
            double top3Prediction = 0;
            double top5Prediction = 0;
            foreach (var wordProbabilities in WordProbabilitiesAfterElementStartingWord.Values.OrderByDescending(wordProbabilities => wordProbabilities.TotalCount))
            {
                double elementStartingWordProbability = wordProbabilities.TotalCount / (double)TotalCount;
                modelFile.WriteLine(Enum.GetName(typeof(TokenType), wordProbabilities.ElementStartingWordType) + "\t" + Math.Round(elementStartingWordProbability * 100, 2));

                double top1PredictionAfterFirstWord = 0;
                double top3PredictionAfterFirstWord = 0;
                double top5PredictionAfterFirstWord = 0;
                foreach (var nextWordProbabilities in wordProbabilities.WordProbabilities.Values.OrderByDescending(nextWordProbabilities => nextWordProbabilities.TotalCount))
                {
                    double firstWordProbability = nextWordProbabilities.TotalCount / (double)wordProbabilities.TotalCount;
                    modelFile.WriteLine("\t" + Enum.GetName(typeof(TokenType), nextWordProbabilities.CurrentWordType) + "\t" + Math.Round(firstWordProbability * 100, 2));

                    double top1PredictionForSecondWord = 0;
                    double top3PredictionForSecondWord = 0;
                    double top5PredictionForSecondWord = 0;
                    int topIndex = 0;
                    foreach (var wordProbability in nextWordProbabilities.NextWords)
                    {
                        topIndex++;
                        double secondWordProbability = wordProbability.Probability;
                        if (topIndex <= 1)
                        {
                            top1PredictionForSecondWord += secondWordProbability;
                        }
                        if (topIndex <= 3)
                        {
                            top3PredictionForSecondWord += secondWordProbability;
                        }
                        if (topIndex <= 5)
                        {
                            top5PredictionForSecondWord += secondWordProbability;
                        }

                        string symbolTypes = String.Empty;
                        if (wordProbability.SymbolTypes != null)
                        {
                            symbolTypes = "(";
                            bool isFirst = true;
                            foreach (var symbolType in wordProbability.SymbolTypes)
                            {
                                if (isFirst)
                                {
                                    isFirst = false;
                                }
                                else
                                {
                                    symbolTypes += ",";
                                }
                                symbolTypes += Enum.GetName(typeof(SymbolType), symbolType);
                            }
                            symbolTypes += ")";
                        }
                        modelFile.WriteLine("\t\t" + Enum.GetName(typeof(TokenType), wordProbability.WordType) + symbolTypes + "\t" + Math.Round(secondWordProbability * 100, 2));
                    }
                    top1PredictionAfterFirstWord += firstWordProbability * top1PredictionForSecondWord;
                    top3PredictionAfterFirstWord += firstWordProbability * top3PredictionForSecondWord;
                    top5PredictionAfterFirstWord += firstWordProbability * top5PredictionForSecondWord;
                }
                top1Prediction += elementStartingWordProbability * top1PredictionAfterFirstWord;
                top3Prediction += elementStartingWordProbability * top3PredictionAfterFirstWord;
                top5Prediction += elementStartingWordProbability * top5PredictionAfterFirstWord;
            }

            console.WriteLine("Done => Model performance :");
            console.WriteLine("Next word is top 1 suggestion  " + Math.Round(top1Prediction * 100, 2) + "% of the time");
            console.WriteLine("Next word in top 3 suggestions " + Math.Round(top3Prediction * 100, 2) + "% of the time");
            console.WriteLine("Next word in top 5 suggestions " + Math.Round(top5Prediction * 100, 2) + "% of the time");
        }
    }
}
