using System.Collections.Generic;

namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        private partial class Automata
        {
            private readonly PictureValidator _validator;

            public int Digits { get; private set; }
            public int RealDigits { get; private set; }
            public bool IsSigned { get; private set; }
            public int Scale { get; private set; }
            public int Size { get; private set; }

            public Automata(PictureValidator validator)
            {
                _validator = validator;
            }

            public bool Run(Character[] sequence, PictureCategory category, List<string> validationMessages)
            {
                ComputeFloatingInsertionRange(sequence, category, out int firstFloatingIndex, out int lastFloatingIndex);

                //Initialization
                var sequenceEnumerator = new SequenceEnumerator(sequence, firstFloatingIndex, lastFloatingIndex);
                State state = State.Initial;
                int vCount = 0;
                bool zSeen = false;
                bool starSeen = false;
                bool csSignSizeAdded = false;

                while (sequenceEnumerator.MoveNext())
                {
                    Character c = sequenceEnumerator.Current;
                    System.Diagnostics.Debug.Assert(c != null);

                    //Check transition from current state to next
                    if (!state.CheckTransition(c.SpecialChar, sequenceEnumerator, out State nextState))
                    {
                        validationMessages.Add(string.Format(INVALID_SYMBOL_POSITION, _validator.SC2String(c.SpecialChar)));
                        return false;
                    }

                    //Check current symbol
                    if (!ValidateSymbol(c))
                    {
                        return false;
                    }

                    //Go to next state and check reflexive transition
                    state = nextState;
                    if (c.Count > 1 && !state.CheckReflexiveTransition())
                    {
                        validationMessages.Add(string.Format(INVALID_SYMBOL_POSITION, _validator.SC2String(c.SpecialChar)));
                        return false;
                    }
                }

                return true;

                //Perform validation on a given symbol and update Digits/Scale/Size
                bool ValidateSymbol(Character c)
                {
                    switch (c.SpecialChar)
                    {
                        case SC.PLUS:
                        case SC.MINUS:
                            Digits += c.Count - 1;
                            IsSigned = true;
                            break;
                        case SC.Z:
                        case SC.STAR:
                            if (c.SpecialChar == SC.Z)
                                zSeen = true;
                            else
                                starSeen = true;
                            if (zSeen && starSeen)
                            {
                                validationMessages.Add(Z_STAR_MUTUALLY_EXCLUSIVE);
                                return false;
                            }
                            if (sequenceEnumerator.IsLast)
                            {
                                // Check that sequence contains only DOT, COMMA or Z or * or CS
                                foreach (var s in sequence)
                                {
                                    if (s.SpecialChar != SC.B &&
                                        s.SpecialChar != SC.ZERO &&
                                        s.SpecialChar != SC.SLASH &&
                                        s.SpecialChar != SC.COMMA &&
                                        s.SpecialChar != SC.DOT &&
                                        s.SpecialChar != SC.CS &&
                                        s.SpecialChar != SC.Z &&
                                        s.SpecialChar != SC.STAR &&
                                        s.SpecialChar != SC.PLUS &&
                                        s.SpecialChar != SC.MINUS)
                                        return false;
                                }
                            }
                            Digits += c.Count;
                            if (vCount > 0)
                            {
                                Scale += c.Count;
                            }
                            break;
                        case SC.CR:
                        case SC.DB:
                            IsSigned = true;
                            break;
                        case SC.CS:
                            Digits += c.Count - 1;
                            break;
                        case SC.NINE:
                            Digits += c.Count;
                            RealDigits += c.Count;
                            if (vCount > 0)
                            {
                                //We have seen the decimal point --> digits are in the decimal part
                                Scale += c.Count;
                            }
                            break;
                        case SC.S:
                            if (!sequenceEnumerator.IsFirst)
                            {
                                validationMessages.Add(SYMBOL_S_MUST_BE_THE_FIRST);
                                return false;
                            }
                            IsSigned = true;
                            break;
                        case SC.DOT:
                        case SC.V:
                            vCount += c.Count;
                            if (vCount > 1)
                            {
                                validationMessages.Add(MULTIPLE_V);
                                return false;
                            }
                            break;
                        case SC.P:
                            if (!ValidateP())
                                return false;
                            Digits += c.Count;
                            Scale += vCount > 0 ? c.Count : -c.Count;
                            break;
                    }

                    //Update total size
                    switch (c.SpecialChar)
                    {
                        case SC.S:
                            Size += _validator.IsSeparateSign ? 1 : 0;
                            break;
                        case SC.V:
                        case SC.P:
                            break;
                        case SC.DB:
                        case SC.CR:
                            Size += c.Count * 2;
                            break;
                        case SC.CS:
                            System.Diagnostics.Debug.Assert(_validator._currencyDescriptor != null);
                            if (!csSignSizeAdded)
                            {
                                //First CS adds the sign length
                                Size += _validator._currencyDescriptor.Sign.Length;
                                csSignSizeAdded = true;
                                Size += c.Count - 1; //Each subsequent CS counts for 1 character
                            }
                            else
                            {
                                Size += c.Count; //Each subsequent CS counts for 1 character
                            }
                            break;
                        case SC.N:
                        case SC.G:
                            Size += c.Count * 2;
                            break;
                        default:
                            Size += c.Count;
                            break;
                    }

                    return true;
                }

                /*
                 * Validate the position of the P character.
                 * The Symbol P specifies a scaling position and implies an assumed decimal point
                 * (to the left of the Ps if the Ps are leftmost PICTURE characters,
                 * to the right of the Ps if the Ps are rightmost PICTURE characters).
                 *
                 * If we say that the character ^ means the beginning of the PICTURE sequence and $ means the end of the PICTURE sequence,
                 * only the following situations are valid for P:
                 *   ^P | ^VP | ^SP | ^SVP | P$ | PV$
                 */
                bool ValidateP()
                {
                    if (sequenceEnumerator.IsFirst || sequenceEnumerator.IsLast)
                    {
                        vCount += sequenceEnumerator.Index == 0 ? 1 : 0; //Assume decimal point symbol V at the beginning
                        return true;//^P | P$;
                    }
                    if (sequenceEnumerator.Index == 1 && (sequence[0].SpecialChar == SC.S || sequence[0].SpecialChar == SC.V))
                    {
                        vCount += 1;//Assume decimal point symbol V at the beginning
                        return true;//^SP | ^VP
                    }
                    if (sequenceEnumerator.Index == 2 && sequence[0].SpecialChar == SC.S && sequence[1].SpecialChar == SC.V)
                    {
                        vCount += 1;//Assume decimal point symbol V at the beginning
                        return true;//^SVP
                    }
                    if (sequenceEnumerator.Index == sequence.Length - 2 && sequence[sequence.Length - 1].SpecialChar == SC.V)
                    {
                        return true;//PV$
                    }

                    //validation failed
                    validationMessages.Add(WRONG_P_POSITION);
                    return false;
                }
            }

            /// <summary>
            /// Compute indexes of the Floating Insertion String. That is to the left most and the right most of the characters
            /// CS, + or -.
            /// </summary>
            /// <param name="sequence">The on which to perform the computation</param>
            /// <param name="category">Data category computed for the sequence</param>
            /// <param name="firstFloatingIndex">[out] First Floating Index</param>
            /// <param name="lastFloatingIndex">[out] Last Floating Index</param>
            private void ComputeFloatingInsertionRange(Character[] sequence, PictureCategory category, out int firstFloatingIndex, out int lastFloatingIndex)
            {
                firstFloatingIndex = lastFloatingIndex = -1;
                if (category != PictureCategory.NumericEdited) return;//Floating insertion is valid only for NumericEdited items

                int lastNonSimpleIndex = -1;
                SC floatChar = (SC)(-1); //The float character that corresponds to the first index either CS, + or -.
                int i;
                for (i = 0; i < sequence.Length; i++)
                {
                    Character c = sequence[i];
                    if (firstFloatingIndex == -1 && (c.SpecialChar == SC.PLUS || c.SpecialChar == SC.MINUS || c.SpecialChar == SC.CS))
                    {
                        if (lastNonSimpleIndex >= 0 && sequence[lastNonSimpleIndex].SpecialChar == c.SpecialChar)
                        {
                            firstFloatingIndex = lastNonSimpleIndex;
                            floatChar = c.SpecialChar;
                            continue;
                        }
                        if (c.Count > 1)
                        {
                            firstFloatingIndex = i;
                            floatChar = c.SpecialChar;
                            continue;
                        }
                    }
                    if (firstFloatingIndex == -1 && !IsSimpleInsertionCharacter(c.SpecialChar))
                    {
                        lastNonSimpleIndex = i;
                    }
                    else if (firstFloatingIndex >= 0 && !(IsSimpleInsertionCharacter(c.SpecialChar) || c.SpecialChar == floatChar))
                    {
                        lastFloatingIndex = i - 1;
                        break;
                    }
                }

                if (i >= sequence.Length && firstFloatingIndex >= 0)
                {
                    //We have reached the end of the sequence with a first index and no lastIndex ==>
                    //Set the last index to the last character of the sequence
                    lastFloatingIndex = sequence.Length - 1;
                    return;
                }

                if (!(i < sequence.Length && (sequence[i].SpecialChar == SC.DOT || sequence[i].SpecialChar == SC.V)))
                {
                    //The last index does not precede the DecimalPoint separator position
                    return;
                }

                //If the last index precedes the DecimalPoint position so all characters including the decimal point
                //that are not simple characters or the floating character must be part of the right most index
                for (++i; i < sequence.Length; i++)
                {
                    Character c = sequence[i];
                    if (!(IsSimpleInsertionCharacter(c.SpecialChar) || c.SpecialChar == floatChar))
                        return;
                }
                lastFloatingIndex = i - 1;

                bool IsSimpleInsertionCharacter(SC c) => c == SC.B || c == SC.ZERO || c == SC.SLASH || c == SC.COMMA;
            }
        }
    }
}
