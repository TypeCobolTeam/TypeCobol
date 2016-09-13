﻿using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    // The two types of literal values below can not be detected before the parsing step

    public class FigurativeConstantValue : LiteralValue
    {
        public FigurativeConstantValue(TokenType tokenType) : base(LiteralValueType.FigurativeConstant)
        {
            FigurativeConstant = tokenType;
        }

        public bool All { get; set; }

        public TokenType FigurativeConstant { get; private set; }
    }

    public class SymbolicCharacterValue : LiteralValue
    {
        public SymbolicCharacterValue(Token symbolicCharacter) : base(LiteralValueType.SymbolicCharacter)
        {
            SymbolicCharacter = new SymbolicCharacter(symbolicCharacter);
        }

        public bool All { get; set; }

        public SymbolicCharacter SymbolicCharacter { get; private set; }
    }
}
