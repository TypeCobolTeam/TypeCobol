using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Scanner
{
    public partial class MultilineScanState
    {
        public class SpecialNamesContext : IEquatable<SpecialNamesContext>
        {
            /// <summary>
            /// True if we are inside the symbolicCharactersClause > symbolicCharacterDefinition+
            /// </summary>
            internal bool InsideSymbolicCharacterDefinitions { get; set; }

            /// <summary>
            /// Symbolic character names previously defined in the source file
            /// NB : value will be null until at least one symbolic character is defined
            /// => only use method AddSymbolicCharacter to safely add an element to this list
            /// </summary>
            public IList<string> SymbolicCharacters { get; private set; }

            /// <summary>
            /// Register a new symbolic character name found in the source file
            /// </summary>
            internal void AddSymbolicCharacter(string tokenText)
            {
                if (SymbolicCharacters == null)
                {
                    SymbolicCharacters = new List<string>();
                }
                SymbolicCharacters.Add(tokenText);
            }

            private IDictionary<char, PictureValidator.CurrencyDescriptor> _customCurrencyDescriptors;
            private bool _nextLiteralIsCurrencySign;
            private bool _nextLiteralIsCurrencySymbol;
            private Token _lastCurrencySignToken;
            private Token _lastCurrencySymbolToken;

            /// <summary>
            /// True if we are inside currencySignClause
            /// </summary>
            internal bool InsideCurrencySignDefinitions => _nextLiteralIsCurrencySign || _nextLiteralIsCurrencySymbol;

            /// <summary>
            /// Call to signal the beginning of a currencySignClause
            /// </summary>
            internal void BeginCurrencySignClause()
            {
                _nextLiteralIsCurrencySign = true;
                _nextLiteralIsCurrencySymbol = false;
                //Flush tokens of the previous clause (if any)
                CreateCurrencyDescriptor();
            }

            /// <summary>
            /// Call to signal the presence of a WITH PICTURE SYMBOL fragment
            /// </summary>
            internal void WithPictureSymbol()
            {
                _nextLiteralIsCurrencySign = false;
                _nextLiteralIsCurrencySymbol = true;
            }

            /// <summary>
            /// Allows to register currency signs and symbols
            /// </summary>
            /// <param name="alphanumericLiteralToken">Current alphanumeric literal token</param>
            /// <remarks>Expects a non-null, AlphanumericLiteralToken|HexadecimalAlphanumericLiteral|NullTerminatedAlphanumericLiteral TokenType</remarks>
            internal void OnAlphanumericLiteralToken(Token alphanumericLiteralToken)
            {
                if (_nextLiteralIsCurrencySign)
                {
                    _lastCurrencySignToken = alphanumericLiteralToken;
                }
                else if (_nextLiteralIsCurrencySymbol)
                {
                    _lastCurrencySymbolToken = alphanumericLiteralToken;
                    //End of clause, flush
                    CreateCurrencyDescriptor();
                }
            }

            /// <summary>
            /// Call to signal that all currencySignClause have been scanned
            /// </summary>
            internal void EndAllCurrencySignClauses()
            {
                _nextLiteralIsCurrencySign = false;
                _nextLiteralIsCurrencySymbol = false;
                //Flush last tokens if any
                CreateCurrencyDescriptor();
            }

            /// <summary>
            /// All valid currency descriptors for this scan state. Not-null, never empty.
            /// </summary>
            public IEnumerable<PictureValidator.CurrencyDescriptor> CurrencyDescriptors
            {
                get
                {
                    if (_customCurrencyDescriptors != null)
                    {
                        foreach (var customCurrencyDescriptor in _customCurrencyDescriptors.Values)
                        {
                            yield return customCurrencyDescriptor;
                        }
                    }
                    else
                    {
                        yield return PictureValidator.CurrencyDescriptor.Default;
                    }
                }
            }

            private void CreateCurrencyDescriptor()
            {
                //Assign token roles
                Token symbolToken, signToken;
                if (_lastCurrencySignToken == null)
                {
                    //If we have a symbol but no sign, then we'll get a syntax error in ANTLR, this is an invalid CURRENCY SIGN clause.
                    //If both are null, we have nothing to do. So either case, reset tokens and return.
                    _lastCurrencySymbolToken = null;
                    return;
                }

                if (_lastCurrencySymbolToken == null)
                {
                    //Sign is also its own associated symbol
                    symbolToken = _lastCurrencySignToken;
                    signToken = _lastCurrencySignToken;
                    _lastCurrencySignToken = null;
                }
                else
                {
                    //Use both custom sign and symbol
                    symbolToken = _lastCurrencySymbolToken;
                    signToken = _lastCurrencySignToken;
                    _lastCurrencySymbolToken = null;
                    _lastCurrencySignToken = null;
                }

                /*
                 * Validate symbol token.
                 * NOTE: although specs describe restrictions on sign, IBM compiler does not check them...
                 */
                string sign = Text(signToken);
                string symbolText = Text(symbolToken);
                if (!PictureValidator.CurrencyDescriptor.ValidateSymbol(symbolText, out string error))
                {
                    AddError(symbolToken, error);
                    return;
                }
                char symbol = symbolText[0];

                //Add new descriptor
                var descriptor = new PictureValidator.CurrencyDescriptor(symbol, sign);
                if (_customCurrencyDescriptors == null)
                {
                    _customCurrencyDescriptors = new Dictionary<char, PictureValidator.CurrencyDescriptor>() { { symbol, descriptor } };
                }
                else if (_customCurrencyDescriptors.ContainsKey(symbol))
                {
                    AddError(symbolToken, $"duplicate currency symbol '{symbol}'.");
                }
                else
                {
                    _customCurrencyDescriptors.Add(symbol, descriptor);
                }

                //Helper local functions
                string Text(Token alphanumericLiteralToken) => ((AlphanumericLiteralTokenValue) alphanumericLiteralToken.LiteralValue).Text;
                void AddError(Token token, string message) => ((TokensLine) token.TokensLine).AddDiagnostic(MessageCode.SyntaxErrorInParser, token, message);
            }

            /// <summary>
            /// True as soon as the keyword DECIMAL-POINT has been encountered
            /// </summary>
            public bool DecimalPointIsComma { get; internal set; }

            internal SpecialNamesContext(bool decimalPointIsComma)
                : this(false, null, false, false, null, null, null, decimalPointIsComma)
            {

            }

            private SpecialNamesContext(bool insideSymbolicCharacterDefinitions,
                IList<string> symbolicCharacters,
                bool nextLiteralIsCurrencySign,
                bool nextLiteralIsCurrencySymbol,
                Token lastCurrencySignToken,
                Token lastCurrencySymbolToken,
                IDictionary<char, PictureValidator.CurrencyDescriptor> customCurrencyDescriptors,
                bool decimalPointIsComma)
            {
                InsideSymbolicCharacterDefinitions = insideSymbolicCharacterDefinitions;
                SymbolicCharacters = symbolicCharacters;
                _customCurrencyDescriptors = customCurrencyDescriptors;
                _nextLiteralIsCurrencySign = nextLiteralIsCurrencySign;
                _nextLiteralIsCurrencySymbol = nextLiteralIsCurrencySymbol;
                _lastCurrencySignToken = lastCurrencySignToken;
                _lastCurrencySymbolToken = lastCurrencySymbolToken;
                DecimalPointIsComma = decimalPointIsComma;
            }

            public SpecialNamesContext Clone()
            {
                var symbolicCharacters = SymbolicCharacters != null ? new List<string>(SymbolicCharacters) : null;
                var customCurrencyDescriptors = _customCurrencyDescriptors != null ? new Dictionary<char, PictureValidator.CurrencyDescriptor>(_customCurrencyDescriptors) : null;
                return new SpecialNamesContext(InsideSymbolicCharacterDefinitions,
                    symbolicCharacters,
                    _nextLiteralIsCurrencySign,
                    _nextLiteralIsCurrencySymbol,
                    _lastCurrencySignToken,
                    _lastCurrencySymbolToken,
                    customCurrencyDescriptors,
                    DecimalPointIsComma);
            }

            public bool Equals(SpecialNamesContext other)
            {
                if (ReferenceEquals(this, other)) return true;
                if (ReferenceEquals(null, other)) return false;

                return InsideSymbolicCharacterDefinitions == other.InsideSymbolicCharacterDefinitions
                       && SymbolicCharacters?.Count == other.SymbolicCharacters?.Count
                       && _nextLiteralIsCurrencySign == other._nextLiteralIsCurrencySign
                       && _nextLiteralIsCurrencySymbol == other._nextLiteralIsCurrencySymbol
                       && _customCurrencyDescriptors?.Count == other._customCurrencyDescriptors?.Count
                       && DecimalPointIsComma == other.DecimalPointIsComma;
            }

            public override bool Equals(object obj) => Equals(obj as SpecialNamesContext);

            //Consistent with Equals method but completely unsafe as it uses mutable properties !
            public override int GetHashCode()
            {
                unchecked
                {
                    int hash = 17;
                    hash = hash * 23 + InsideSymbolicCharacterDefinitions.GetHashCode();
                    hash = hash * 23 + SymbolicCharacters?.Count ?? 0;
                    hash = hash * 23 + _nextLiteralIsCurrencySign.GetHashCode();
                    hash = hash * 23 + _nextLiteralIsCurrencySymbol.GetHashCode();
                    hash = hash * 23 + _customCurrencyDescriptors?.Count ?? 0;
                    hash = hash * 23 + DecimalPointIsComma.GetHashCode();
                    return hash;
                }
            }
        }
    }
}
