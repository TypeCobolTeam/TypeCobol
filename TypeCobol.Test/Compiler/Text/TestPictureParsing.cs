using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Test {

static class TestPictureParsing {

	public static void Check_PictureToType() {
		TestConversion("",              DataType.Unknown);
		TestConversion("X(10)/XX",      DataType.AlphanumericEdited);
		TestConversion("X(5)BX(7)",     DataType.AlphanumericEdited);
		TestConversion("X(5)X(7)",      DataType.Alphanumeric);
		TestConversion("99,B999,B000",  DataType.NumericEdited);
		TestConversion("99,999",        DataType.Numeric);
		TestConversion("GGBBGG",        DataType.DBCS);
		TestConversion("+999,99E+99",   DataType.FloatingPoint);
	}

	private static bool TestConversion(string picture, DataType expected) {
		DataType result = DataType.Create(picture);
		if (result != expected) throw new System.Exception("\""+picture+"\">"+result+" vs expected: "+expected);
		return result == expected;
	}

}

}