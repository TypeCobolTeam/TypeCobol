package typecobol.client;

public enum TokenFamily {

	//          0 : Error
	Invalid(0),
	//   1 ->   4 : Whitespace
	// p46: The separator comma and separator semicolon can
	// be used anywhere the separator space is used.
	Whitespace(1),
	//   5 ->   6 : Comments
	Comments(5),
	// 7 ->  11 : Separators - Syntax
	// -1 (EndOfFile)
	SyntaxSeparator(7),
    //  12 ->  16 : Special character word - Arithmetic operators
    ArithmeticOperator(12),
    //  17 ->  21 : Special character word - Relational operators
    RelationalOperator(17),
    //  22 ->  27 : Literals - Alphanumeric
    AlphanumericLiteral(22),
    //  28 ->  30 : Literals - Numeric 
    NumericLiteral(28),
    //  31 ->  33 : Literals - Syntax tokens
    SyntaxLiteral(31),
    //  34 ->  37 : Symbols
    Symbol(34),
    //  38 ->  56 : Keywords - Compiler directive starting tokens
    CompilerDirectiveStartingKeyword(38),
    //  57 ->  79 : Keywords - Code element starting tokens
    CodeElementStartingKeyword(57),
    //  80 ->  120: Keywords - Statement starting tokens
    StatementStartingKeyword(80),
    //  121 -> 141 : Keywords - Statement ending tokens
    StatementEndingKeyword(121),
    // 142 -> 172 : Keywords - Special registers
    SpecialRegisterKeyword(142),
    // 173 -> 186 : Keywords - Figurative constants
    FigurativeConstantKeyword(173),
    // 187 -> 188 : Keywords - Special object identifiers
    SpecialObjetIdentifierKeyword(187),
    // 189 -> 450 : Keywords - Syntax tokens  
    SyntaxKeyword(189),
    // 451 -> 453 : Compiler directives
    CompilerDirective(451),
    // 454 -> 454 : Internal token groups - used by the preprocessor only
    InternalTokenGroup(454);



	private final int code;
	private TokenFamily(final int code) { this.code = code; }
	@Override
	public String toString() { return String.valueOf(code); }

	public static TokenFamily asEnum(final int code) {
		for(final TokenFamily family: TokenFamily.values()) {
			if (family.code == code) return family;
		}
		throw new IllegalArgumentException("Invalid code for enum: "+code);
	}

}
