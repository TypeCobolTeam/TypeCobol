using System;
using System.Text;
using TypeCobol.Compiler.File;

namespace TypeCobol.Test.Parser.FileFormat
{
    static class TestIBMCodePages
    {
        public static void Check_GetDotNetEncoding()
        {
            Encoding encoding1 = IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147);
            if (encoding1.EncodingName != "IBM EBCDIC (France-Euro)")
            {
                throw new Exception(".Net encoding \"IBM EBCDIC (France-Euro)\" was expected");
            }

            Encoding encoding2 = IBMCodePages.GetDotNetEncodingFromIBMCCSID(1200);
            if (encoding2.EncodingName != "Unicode")
            {
                throw new Exception(".Net encoding \"Unicode\" was expected");
            }
        }

        public static void Check_IsEBCDICCodePage()
        {
            Encoding encoding1 = IBMCodePages.GetDotNetEncodingFromIBMCCSID(1148);
            bool isEBCDIC1 = IBMCodePages.IsEBCDICCodePage(encoding1);
            if(!isEBCDIC1)
            {
                throw new Exception("Encoding 1148 should be detected as EBCDIC");
            }

            Encoding encoding2 = IBMCodePages.GetDotNetEncodingFromIBMCCSID(1252);
            bool isEBCDIC2 = IBMCodePages.IsEBCDICCodePage(encoding2);
            if (isEBCDIC2)
            {
                throw new Exception("Encoding 1252 should not be detected as EBCDIC");
            }
        }

        public static void Check_DBCSCodePageNotSupported()
        {
            bool exceptionOK = false;
            try
            {
                Encoding encoding = IBMCodePages.GetDotNetEncodingFromIBMCCSID(930);
            }
            catch (Exception e)
            {
                if (e is NotSupportedException) exceptionOK = true;
            }
            if (!exceptionOK)
            {
                throw new Exception("IBMCodePages should throw exception to ensure that no EBCDIC DBCS character set is used");
            }
        }
    }
}
