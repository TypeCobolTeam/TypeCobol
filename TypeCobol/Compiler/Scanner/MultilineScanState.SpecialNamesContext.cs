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

            internal bool BeforeCurrencySignToken { get; set; }

            internal bool BeforeCurrencySymbolToken { get; set; }

            internal Token LastCurrencySignToken { get; set; }

            internal Token LastCurrencySymbolToken { get; set; }

            public IList<PictureValidator.CurrencyDescriptor> CurrencyDescriptors { get; private set; }

            internal void CreateCurrencyDescriptor()
            {
                //Assign token roles
                Token symbolToken, signToken;
                if (LastCurrencySignToken == null)
                {
                    //If we have a symbol but no sign, then we'll get a syntax error in ANTLR, this is an invalid CURRENCY SIGN clause.
                    //If both are null, we have nothing to do. So either case, reset tokens and return.
                    LastCurrencySymbolToken = null;
                    return;
                }

                if (LastCurrencySymbolToken == null)
                {
                    //Sign is also its own associated symbol
                    symbolToken = LastCurrencySignToken;
                    signToken = LastCurrencySignToken;
                    LastCurrencySignToken = null;
                }
                else
                {
                    //Use both custom sign and symbol
                    symbolToken = LastCurrencySymbolToken;
                    signToken = LastCurrencySignToken;
                    LastCurrencySymbolToken = null;
                    LastCurrencySignToken = null;
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
                if (CurrencyDescriptors == null)
                {
                    CurrencyDescriptors = new List<PictureValidator.CurrencyDescriptor>();
                }
                CurrencyDescriptors.Add(new PictureValidator.CurrencyDescriptor(symbol, sign));

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
                bool beforeCurrencySignToken,
                bool beforeCurrencySymbolToken,
                Token lastCurrencySignToken,
                Token lastCurrencySymbolToken,
                IList<PictureValidator.CurrencyDescriptor> currencyDescriptors,
                bool decimalPointIsComma)
            {
                InsideSymbolicCharacterDefinitions = insideSymbolicCharacterDefinitions;
                SymbolicCharacters = symbolicCharacters;
                BeforeCurrencySignToken = beforeCurrencySignToken;
                BeforeCurrencySymbolToken = beforeCurrencySymbolToken;
                LastCurrencySignToken = lastCurrencySignToken;
                LastCurrencySymbolToken = lastCurrencySymbolToken;
                CurrencyDescriptors = currencyDescriptors;
                DecimalPointIsComma = decimalPointIsComma;
            }

            public SpecialNamesContext Clone()
            {
                var symbolicCharacters = SymbolicCharacters != null ? new List<string>(SymbolicCharacters) : null;
                var currencyDescriptors = CurrencyDescriptors != null ? new List<PictureValidator.CurrencyDescriptor>(CurrencyDescriptors) : null;
                return new SpecialNamesContext(InsideSymbolicCharacterDefinitions,
                    symbolicCharacters,
                    BeforeCurrencySignToken,
                    BeforeCurrencySymbolToken,
                    LastCurrencySignToken,
                    LastCurrencySymbolToken,
                    currencyDescriptors,
                    DecimalPointIsComma);
            }

            public bool Equals(SpecialNamesContext other)
            {
                if (ReferenceEquals(this, other)) return true;
                if (ReferenceEquals(null, other)) return false;

                return InsideSymbolicCharacterDefinitions == other.InsideSymbolicCharacterDefinitions
                       && SymbolicCharacters?.Count == other.SymbolicCharacters?.Count
                       && BeforeCurrencySignToken == other.BeforeCurrencySignToken
                       && BeforeCurrencySymbolToken == other.BeforeCurrencySymbolToken
                       //Compare tokens ?
                       && CurrencyDescriptors?.Count == other.CurrencyDescriptors?.Count
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
                    hash = hash * 23 + BeforeCurrencySignToken.GetHashCode();
                    hash = hash * 23 + BeforeCurrencySymbolToken.GetHashCode();
                    //Include tokens ?
                    hash = hash * 23 + CurrencyDescriptors?.Count ?? 0;
                    hash = hash * 23 + DecimalPointIsComma.GetHashCode();
                    return hash;
                }
            }
        }
    }
}
