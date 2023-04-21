using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Types;

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
			TestConversion("****,**",       DataType.NumericEdited, false);
			TestConversion("ZZZZ.ZZ",       DataType.NumericEdited);
			TestConversion("99,B999,B000",  DataType.NumericEdited);
			TestConversion("99,999",        DataType.NumericEdited);
			TestConversion("GGBBGG",        DataType.DBCS);
			TestConversion("+999.99E+99",   DataType.FloatingPoint);
			TestConversion("+999,99E+99",   DataType.FloatingPoint, true);
			TestConversion("-9v9(9)E-99",   DataType.FloatingPoint);

			TestConversion("999",  DataType.Numeric);
			TestConversion("$,99", DataType.NumericEdited);
			TestConversion("$,99", DataType.NumericEdited, '$');
			TestConversion("€,99", DataType.NumericEdited, '€');
			TestConversion("$,99", DataType.NumericEdited, '$', '€');
			
			
			TestConversion("U9V", DataType.NumericEdited, 'U');
			TestConversion("S9V", DataType.Numeric);

			// the following syntaxes are recognized, although wrong
			// this method tries to detect intent, it's not its job to detect syntax errors
			TestConversion("SVU",  DataType.Unknown, 'U');
			TestConversion("UX",   DataType.Unknown, 'U');
			TestConversion("UN",   DataType.Unknown, 'U');
			TestConversion("UA",   DataType.Unknown, 'U');
			TestConversion("$,50", DataType.Unknown);
			TestConversion("$,50", DataType.Unknown, '$');
		}

        private static void TestConversion(string picture, DataType expected, params char[] currencies)
        {
            TestConversion(picture, expected, false, currencies);
        }

        private static void TestConversion(string picture, DataType expected, bool decimalPointIsComma, params char[] currencies)
        {
            Dictionary<char, PictureValidator.CurrencyDescriptor> currencyDescriptor = null;
            if (currencies?.Length > 0)
            {
                currencyDescriptor = new Dictionary<char, PictureValidator.CurrencyDescriptor>();
                foreach (var currency in currencies)
                {
                    currencyDescriptor.Add(currency, new PictureValidator.CurrencyDescriptor(currency, currency.ToString()));
                }
            }

            PictureValidator pictureValidator = new PictureValidator(picture, false, decimalPointIsComma, currencyDescriptor);
            var result = pictureValidator.Validate(out var validationMessages);

            var dataType = DataType.Create(result);
            if (dataType != expected) throw new System.Exception("\""+picture+"\">"+result.Category+" vs expected: "+expected);
        }
	}

}