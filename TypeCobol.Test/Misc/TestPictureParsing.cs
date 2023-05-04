using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Test.Misc {

    [TestClass]
    public class TestPictureParsing {

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time","fast")]
        public void Check_PictureToType() {
            TestConversion("",              DataType.Unknown);
            TestConversion("WWW",           DataType.Unknown);
            TestConversion("AAA",           DataType.Alphabetic);
            TestConversion("AAaaaAaaaAA",   DataType.Alphabetic);
            TestConversion("0X",            DataType.AlphanumericEdited);
            TestConversion("X(10)/XX",      DataType.AlphanumericEdited);
            TestConversion("X(5)BX(7)",     DataType.AlphanumericEdited);
            TestConversion("X(5)X(10)",     DataType.Alphanumeric);
            TestConversion("XXXXXXXXX",     DataType.Alphanumeric);
            TestConversion("xxxxxxxxx",     DataType.Alphanumeric);
            TestConversion("****.**",       DataType.NumericEdited);
            TestConversion("ZZZZ.ZZ",       DataType.NumericEdited);
            TestConversion("99,B999,B000",  DataType.NumericEdited);
            TestConversion("99,999",        DataType.Numeric);
            TestConversion("GGBBGG",        DataType.DBCS);
            TestConversion("+999,99E+99",   DataType.FloatingPoint);
            TestConversion("-9v9(9)E-99",   DataType.FloatingPoint);

            TestConversion("$,99", DataType.Numeric);
            TestConversion("$,99", DataType.NumericEdited, new char[] {'$'});
            TestConversion("€,99", DataType.NumericEdited, new char[] {'€'});
            TestConversion("$,99", DataType.NumericEdited, new char[] {'$','€'});
            TestConversion("SVU",  DataType.NumericEdited, new char[] {'U'});
            TestConversion("SV",   DataType.Numeric);
            // the following syntaxes are recognized, although wrong
            // this method tries to detect intent, it's not its job to detect syntax errors
            TestConversion("UX",   DataType.Alphanumeric,  new char[] {'U'});
            TestConversion("UN",   DataType.DBCS,          new char[] {'U'});
            TestConversion("UA",   DataType.Alphabetic,    new char[] {'U'});
            TestConversion("$,50", DataType.NumericEdited);
            TestConversion("$,50", DataType.NumericEdited, new char[] {'$'});
        }

        private static bool TestConversion(string picture, DataType expected, char[] currencies = null) {
            DataType result;
            if (currencies == null) result = DataType.Create(picture);
            else result = DataType.Create(picture, currencies);
            if (result != expected) throw new System.Exception("\""+picture+"\">"+result+" vs expected: "+expected);
            return result == expected;
        }
    }

}