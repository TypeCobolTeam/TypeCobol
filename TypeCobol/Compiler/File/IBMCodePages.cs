using System.Text;

namespace TypeCobol.Compiler.File
{
    /// <summary>
    /// Check properties of IBM code pages and get the equivalent .Net Encodings
    /// </summary>
    public static class IBMCodePages
    {
        /// <summary>
        /// Gets the .Net Encoding equivalent to an IBM Coded Character Set ID
        /// (or throws a NotSupportedException)
        /// </summary>
        public static Encoding GetDotNetEncodingFromIBMCCSID(int ibmCCSID)
        {
            if(IsDBCSCodePage(ibmCCSID))
            {
                throw new NotSupportedException(String.Format("The compiler does not support DBCS character encoding in EBCDIC source files (with IBM CCSID {0})", ibmCCSID));
            }
            else if (!DotNetCodePageFromIBMCCSID.ContainsKey(ibmCCSID))
            {
                throw new NotSupportedException(String.Format("Could not find a .Net encoding equivalent to IBM CCSID {0}", ibmCCSID));
            }
            else
            {
                return Encoding.GetEncoding(DotNetCodePageFromIBMCCSID[ibmCCSID].MS_CodePage);
            }
        }

        /// <summary>
        /// Returns true if the .Net Encoding is equivalent to an IBM EBCDIC code page
        /// </summary>
        public static bool IsEBCDICCodePage(Encoding encoding)
        {
            CodePageEquivalence codePageEquivalence;
            if (IBMCCSIDFromDotNetCodePage.TryGetValue(encoding.CodePage, out codePageEquivalence))
            {
                return codePageEquivalence.IBM_EBCDIC;
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// Returns true if the IBM Coded Character Set ID allows the use of DBCS characters
        /// </summary>
        public static bool IsDBCSCodePage(int ibmCCSID)
        {
            // Programming guide / Compiler options / CODEPAGE
            // DBCS code pages:
            // Compile your COBOL program using the CODEPAGE option with the ccsid set to one of the EBCDIC multibyte character set (MBCS) CCSIDs shown in the table below if the program contains any of the following items: 
            // - User-defined words formed with DBCS characters 
            // - DBCS (USAGE DISPLAY-1) data items 
            // - DBCS literals 
            // All of the CCSIDs in the table below identify mixed code pages that refer to a combination of SBCS and DBCS coded character sets. 
            // These are also the CCSIDs that are supported for mixed data by DB2.
            // Table 1. EBCDIC multibyte coded character set identifiers 
            // National language                  | MBCS CCSID | SBCS CCSID component | DBCS CCSID component 
            // Japanese (Katakana-Kanji)            930          290                    300 
            // Japanese (Katakana-Kanji with euro)  1390         8482                   16684 
            // Japanese (Katakana-Kanji)            5026         290                    4396 
            // Japanese (Latin-Kanji)               939          1027                   300 
            // Japanese (Latin-Kanji with euro)     1399         5123                   16684 
            // Japanese (Latin-Kanji)               5035         1027                   4396 
            // Korean                               933          833                    834 
            // Korean                               1364         13121                  4930 
            // Simplified Chinese                   935          836                    837 
            // Simplified Chinese                   1388         13124                  4933 
            // Traditional Chinese                  937          28709                  835 
            // => MBCS CCSIDS : 930,933,935,937,939,1364,1388,1390,1399,5026,5035

            if (ibmCCSID == 930 || ibmCCSID == 933 || ibmCCSID == 935 || ibmCCSID == 937 || ibmCCSID == 939 ||
               ibmCCSID == 1364 || ibmCCSID == 1388 || ibmCCSID == 1390 || ibmCCSID == 1399 ||
               ibmCCSID == 5026 || ibmCCSID == 5035)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        // --- Internal mapping table between IBM code pages and Microsoft .Net Encodings ---

        private static IDictionary<int, CodePageEquivalence> DotNetCodePageFromIBMCCSID = new Dictionary<int, CodePageEquivalence>();
        private static IDictionary<int, CodePageEquivalence> IBMCCSIDFromDotNetCodePage = new Dictionary<int, CodePageEquivalence>();

        static IBMCodePages()
        {
            DotNetCodePageFromIBMCCSID.Add(37, new CodePageEquivalence() { IBM_CCSID = 37, IBM_Name = "COM EUROPE EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 37, MS_DotNetName = "IBM037 ", MS_AdditionalInfo = "IBM EBCDIC (EU-Canada) " });
            DotNetCodePageFromIBMCCSID.Add(273, new CodePageEquivalence() { IBM_CCSID = 273, IBM_Name = "AUS/GERM EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20273, MS_DotNetName = "IBM273 ", MS_AdditionalInfo = "IBM EBCDIC (Allemagne) " });
            DotNetCodePageFromIBMCCSID.Add(277, new CodePageEquivalence() { IBM_CCSID = 277, IBM_Name = "DEN/NORWAY EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20277, MS_DotNetName = "IBM277 ", MS_AdditionalInfo = "IBM EBCDIC (Danemark-Norvège) " });
            DotNetCodePageFromIBMCCSID.Add(278, new CodePageEquivalence() { IBM_CCSID = 278, IBM_Name = "FIN/SWEDEN EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20278, MS_DotNetName = "IBM278 ", MS_AdditionalInfo = "IBM EBCDIC (Finlande-Suède) " });
            DotNetCodePageFromIBMCCSID.Add(280, new CodePageEquivalence() { IBM_CCSID = 280, IBM_Name = "ITALIAN EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20280, MS_DotNetName = "IBM280 ", MS_AdditionalInfo = "IBM EBCDIC (Italie) " });
            DotNetCodePageFromIBMCCSID.Add(284, new CodePageEquivalence() { IBM_CCSID = 284, IBM_Name = "SPANISH EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20284, MS_DotNetName = "IBM284 ", MS_AdditionalInfo = "IBM EBCDIC (Espagne) " });
            DotNetCodePageFromIBMCCSID.Add(285, new CodePageEquivalence() { IBM_CCSID = 285, IBM_Name = "UK EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20285, MS_DotNetName = "IBM285 ", MS_AdditionalInfo = "IBM EBCDIC (RU) " });
            DotNetCodePageFromIBMCCSID.Add(290, new CodePageEquivalence() { IBM_CCSID = 290, IBM_Name = "JAPANESE EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20290, MS_DotNetName = "IBM290 ", MS_AdditionalInfo = "IBM EBCDIC (Japonais katakana) " });
            DotNetCodePageFromIBMCCSID.Add(297, new CodePageEquivalence() { IBM_CCSID = 297, IBM_Name = "FRENCH EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20297, MS_DotNetName = "IBM297 ", MS_AdditionalInfo = "IBM EBCDIC (France) " });
            DotNetCodePageFromIBMCCSID.Add(420, new CodePageEquivalence() { IBM_CCSID = 420, IBM_Name = "ARABIC EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20420, MS_DotNetName = "IBM420 ", MS_AdditionalInfo = "IBM EBCDIC (Arabe) " });
            DotNetCodePageFromIBMCCSID.Add(423, new CodePageEquivalence() { IBM_CCSID = 423, IBM_Name = "GREEK EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20423, MS_DotNetName = "IBM423 ", MS_AdditionalInfo = "IBM EBCDIC (Grec) " });
            DotNetCodePageFromIBMCCSID.Add(424, new CodePageEquivalence() { IBM_CCSID = 424, IBM_Name = "HEBREW EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20424, MS_DotNetName = "IBM424 ", MS_AdditionalInfo = "IBM EBCDIC (Hébreu) " });
            DotNetCodePageFromIBMCCSID.Add(437, new CodePageEquivalence() { IBM_CCSID = 437, IBM_Name = "USA PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 437, MS_DotNetName = "IBM437 ", MS_AdditionalInfo = "OEM États-Unis " });
            DotNetCodePageFromIBMCCSID.Add(500, new CodePageEquivalence() { IBM_CCSID = 500, IBM_Name = "INTL EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 500, MS_DotNetName = "IBM500 ", MS_AdditionalInfo = "IBM EBCDIC (International) " });
            DotNetCodePageFromIBMCCSID.Add(720, new CodePageEquivalence() { IBM_CCSID = 720, IBM_Name = "MSDOS ARABIC", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 720, MS_DotNetName = "DOS-720 ", MS_AdditionalInfo = "Arabe (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(737, new CodePageEquivalence() { IBM_CCSID = 737, IBM_Name = "MSDOS GREEK", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 737, MS_DotNetName = "ibm737 ", MS_AdditionalInfo = "Grec (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(775, new CodePageEquivalence() { IBM_CCSID = 775, IBM_Name = "MSDOS BALTIC", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 775, MS_DotNetName = "ibm775 ", MS_AdditionalInfo = "Baltique (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(813, new CodePageEquivalence() { IBM_CCSID = 813, IBM_Name = "ISO 8859-7 Greek/Latin", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28597, MS_DotNetName = "iso-8859-7 ", MS_AdditionalInfo = "Grec (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(819, new CodePageEquivalence() { IBM_CCSID = 819, IBM_Name = "ISO 8859-1 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28591, MS_DotNetName = "iso-8859-1 ", MS_AdditionalInfo = "Europe de l'Ouest (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(833, new CodePageEquivalence() { IBM_CCSID = 833, IBM_Name = "KOREAN EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20833, MS_DotNetName = "x-EBCDIC-KoreanExtended ", MS_AdditionalInfo = "IBM EBCDIC (Coréen étendu) " });
            DotNetCodePageFromIBMCCSID.Add(838, new CodePageEquivalence() { IBM_CCSID = 838, IBM_Name = "THAILAND EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20838, MS_DotNetName = "IBM-Thaï ", MS_AdditionalInfo = "IBM EBCDIC (Thaï) " });
            DotNetCodePageFromIBMCCSID.Add(850, new CodePageEquivalence() { IBM_CCSID = 850, IBM_Name = "LATIN-1 PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 850, MS_DotNetName = "ibm850 ", MS_AdditionalInfo = "Europe de l'Ouest (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(852, new CodePageEquivalence() { IBM_CCSID = 852, IBM_Name = "LATIN-2 PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 852, MS_DotNetName = "ibm852 ", MS_AdditionalInfo = "Europe centrale (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(855, new CodePageEquivalence() { IBM_CCSID = 855, IBM_Name = "CYRILLIC PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 855, MS_DotNetName = "IBM855 ", MS_AdditionalInfo = "OEM Cyrillique " });
            DotNetCodePageFromIBMCCSID.Add(857, new CodePageEquivalence() { IBM_CCSID = 857, IBM_Name = "TURKISH PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 857, MS_DotNetName = "ibm857 ", MS_AdditionalInfo = "Turc (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(858, new CodePageEquivalence() { IBM_CCSID = 858, IBM_Name = "LATIN-1E PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 858, MS_DotNetName = "IBM00858 ", MS_AdditionalInfo = "OEM Multilingue Latin I " });
            DotNetCodePageFromIBMCCSID.Add(860, new CodePageEquivalence() { IBM_CCSID = 860, IBM_Name = "PORTUGESE PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 860, MS_DotNetName = "IBM860 ", MS_AdditionalInfo = "Portugais (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(861, new CodePageEquivalence() { IBM_CCSID = 861, IBM_Name = "ICELAND PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 861, MS_DotNetName = "ibm861 ", MS_AdditionalInfo = "Islandais (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(862, new CodePageEquivalence() { IBM_CCSID = 862, IBM_Name = "HEBREW PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 862, MS_DotNetName = "DOS-862 ", MS_AdditionalInfo = "Hébreu (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(863, new CodePageEquivalence() { IBM_CCSID = 863, IBM_Name = "CANADA PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 863, MS_DotNetName = "IBM863 ", MS_AdditionalInfo = "Français canadien (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(864, new CodePageEquivalence() { IBM_CCSID = 864, IBM_Name = "ARABIC PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 864, MS_DotNetName = "IBM864 ", MS_AdditionalInfo = "Arabe (864) " });
            DotNetCodePageFromIBMCCSID.Add(865, new CodePageEquivalence() { IBM_CCSID = 865, IBM_Name = "DEN/NORWAY PC-DAT", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 865, MS_DotNetName = "IBM865 ", MS_AdditionalInfo = "Nordique (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(866, new CodePageEquivalence() { IBM_CCSID = 866, IBM_Name = "CYRILLIC PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 866, MS_DotNetName = "cp866 ", MS_AdditionalInfo = "Cyrillique (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(869, new CodePageEquivalence() { IBM_CCSID = 869, IBM_Name = "GREEK PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 869, MS_DotNetName = "ibm869 ", MS_AdditionalInfo = "Grec moderne (DOS) " });
            DotNetCodePageFromIBMCCSID.Add(870, new CodePageEquivalence() { IBM_CCSID = 870, IBM_Name = "LATIN-2 EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 870, MS_DotNetName = "IBM870 ", MS_AdditionalInfo = "IBM EBCDIC (Multilingue Latin-2) " });
            DotNetCodePageFromIBMCCSID.Add(871, new CodePageEquivalence() { IBM_CCSID = 871, IBM_Name = "ICELAND EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20871, MS_DotNetName = "IBM871 ", MS_AdditionalInfo = "IBM EBCDIC (islandais) " });
            DotNetCodePageFromIBMCCSID.Add(874, new CodePageEquivalence() { IBM_CCSID = 874, IBM_Name = "THAI PC-DATA", IBM_EncodingScheme = "IBM-PC Data, SBCS", IBM_EBCDIC = false, MS_CodePage = 874, MS_DotNetName = "windows-874 ", MS_AdditionalInfo = "Thaï (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(875, new CodePageEquivalence() { IBM_CCSID = 875, IBM_Name = "GREEK EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 875, MS_DotNetName = "cp875 ", MS_AdditionalInfo = "IBM EBCDIC (Grec moderne) " });
            DotNetCodePageFromIBMCCSID.Add(878, new CodePageEquivalence() { IBM_CCSID = 878, IBM_Name = "KOI8-R CYRILLIC", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 20866, MS_DotNetName = "koi8-r ", MS_AdditionalInfo = "Cyrillique (KOI8-R) " });
            DotNetCodePageFromIBMCCSID.Add(880, new CodePageEquivalence() { IBM_CCSID = 880, IBM_Name = "CYRILLIC EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20880, MS_DotNetName = "IBM880 ", MS_AdditionalInfo = "IBM EBCDIC (Russe cyrillique) " });
            DotNetCodePageFromIBMCCSID.Add(905, new CodePageEquivalence() { IBM_CCSID = 905, IBM_Name = "TURKEY EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20905, MS_DotNetName = "IBM905 ", MS_AdditionalInfo = "IBM EBCDIC (Turc) " });
            DotNetCodePageFromIBMCCSID.Add(912, new CodePageEquivalence() { IBM_CCSID = 912, IBM_Name = "ISO 8859-2 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28592, MS_DotNetName = "iso-8859-2 ", MS_AdditionalInfo = "Europe centrale (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(913, new CodePageEquivalence() { IBM_CCSID = 913, IBM_Name = "ISO 8859-3 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28593, MS_DotNetName = "iso-8859-3 ", MS_AdditionalInfo = "Latin 3 (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(914, new CodePageEquivalence() { IBM_CCSID = 914, IBM_Name = "ISO 8859-4 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28594, MS_DotNetName = "iso-8859-4 ", MS_AdditionalInfo = "Baltique (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(915, new CodePageEquivalence() { IBM_CCSID = 915, IBM_Name = "ISO 8859-5 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28595, MS_DotNetName = "iso-8859-5 ", MS_AdditionalInfo = "Cyrillique (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(916, new CodePageEquivalence() { IBM_CCSID = 916, IBM_Name = "ISO 8859-8 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28598, MS_DotNetName = "iso-8859-8 ", MS_AdditionalInfo = "Hébreu (ISO-Visual) " });
            DotNetCodePageFromIBMCCSID.Add(920, new CodePageEquivalence() { IBM_CCSID = 920, IBM_Name = "ISO 8859-9 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28599, MS_DotNetName = "iso-8859-9 ", MS_AdditionalInfo = "Turc (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(921, new CodePageEquivalence() { IBM_CCSID = 921, IBM_Name = "ISO 8859-13", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28603, MS_DotNetName = "iso-8859-13 ", MS_AdditionalInfo = "Estonien (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(923, new CodePageEquivalence() { IBM_CCSID = 923, IBM_Name = "ISO 8859-15 ASCII", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28605, MS_DotNetName = "iso-8859-15 ", MS_AdditionalInfo = "Latin 9 (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(924, new CodePageEquivalence() { IBM_CCSID = 924, IBM_Name = "Latin 9 EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 20924, MS_DotNetName = "IBM00924 ", MS_AdditionalInfo = "IBM Latin-1 " });
            DotNetCodePageFromIBMCCSID.Add(932, new CodePageEquivalence() { IBM_CCSID = 932, IBM_Name = "JAPAN MIX PC-DATA", IBM_EncodingScheme = "IBM-PC Data, Mixed single-byte and double-byte", IBM_EBCDIC = false, MS_CodePage = 932, MS_DotNetName = "shift_jis ", MS_AdditionalInfo = "Japonais (Shift-JIS) " });
            DotNetCodePageFromIBMCCSID.Add(936, new CodePageEquivalence() { IBM_CCSID = 936, IBM_Name = "S-CHINESE PC-DATA", IBM_EncodingScheme = "IBM-PC Data, Mixed single-byte and double-byte", IBM_EBCDIC = false, MS_CodePage = 936, MS_DotNetName = "gb2312 ", MS_AdditionalInfo = "Chinois simplifié (GB2312) " });
            DotNetCodePageFromIBMCCSID.Add(949, new CodePageEquivalence() { IBM_CCSID = 949, IBM_Name = "KOREA KS PC-DATA", IBM_EncodingScheme = "IBM-PC Data, Mixed single-byte and double-byte", IBM_EBCDIC = false, MS_CodePage = 949, MS_DotNetName = "ks_c_5601-1987 ", MS_AdditionalInfo = "Coréen " });
            DotNetCodePageFromIBMCCSID.Add(950, new CodePageEquivalence() { IBM_CCSID = 950, IBM_Name = "T-CH MIX PC-DATA", IBM_EncodingScheme = "IBM-PC Data, Mixed single-byte and double-byte", IBM_EBCDIC = false, MS_CodePage = 950, MS_DotNetName = "big5 ", MS_AdditionalInfo = "Chinois traditionnel (Big5) " });
            DotNetCodePageFromIBMCCSID.Add(1025, new CodePageEquivalence() { IBM_CCSID = 1025, IBM_Name = "CYRILLIC EBCDIC", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 21025, MS_DotNetName = "cp1025 ", MS_AdditionalInfo = "IBM EBCDIC (Serbe cyrillique-bulgare) " });
            DotNetCodePageFromIBMCCSID.Add(1026, new CodePageEquivalence() { IBM_CCSID = 1026, IBM_Name = "TURKEY LATIN-5 EB", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1026, MS_DotNetName = "IBM1026 ", MS_AdditionalInfo = "IBM EBCDIC (Turc Latin-5) " });
            DotNetCodePageFromIBMCCSID.Add(1047, new CodePageEquivalence() { IBM_CCSID = 1047, IBM_Name = "LATIN OPEN SYS EB", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1047, MS_DotNetName = "IBM01047 ", MS_AdditionalInfo = "IBM Latin-1 " });
            DotNetCodePageFromIBMCCSID.Add(1089, new CodePageEquivalence() { IBM_CCSID = 1089, IBM_Name = "ARABIC ISO 8859-6", IBM_EncodingScheme = "ISO 8, SBCS", IBM_EBCDIC = false, MS_CodePage = 28596, MS_DotNetName = "iso-8859-6 ", MS_AdditionalInfo = "Arabe (ISO) " });
            DotNetCodePageFromIBMCCSID.Add(1140, new CodePageEquivalence() { IBM_CCSID = 1140, IBM_Name = "COM EUROPE ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1140, MS_DotNetName = "IBM01140 ", MS_AdditionalInfo = "IBM EBCDIC (EU-Canada-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1141, new CodePageEquivalence() { IBM_CCSID = 1141, IBM_Name = "AUS/GERM ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1141, MS_DotNetName = "IBM01141 ", MS_AdditionalInfo = "IBM EBCDIC (Allemagne-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1142, new CodePageEquivalence() { IBM_CCSID = 1142, IBM_Name = "DEN/NORWAY ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1142, MS_DotNetName = "IBM01142 ", MS_AdditionalInfo = "IBM EBCDIC (Danemark-Norvège-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1143, new CodePageEquivalence() { IBM_CCSID = 1143, IBM_Name = "FIN/SWEDEN ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1143, MS_DotNetName = "IBM01143 ", MS_AdditionalInfo = "IBM EBCDIC (Finlande-Suède-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1144, new CodePageEquivalence() { IBM_CCSID = 1144, IBM_Name = "ITALIAN ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1144, MS_DotNetName = "IBM01144 ", MS_AdditionalInfo = "IBM EBCDIC (Italie-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1145, new CodePageEquivalence() { IBM_CCSID = 1145, IBM_Name = "SPANISH ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1145, MS_DotNetName = "IBM01145 ", MS_AdditionalInfo = "IBM EBCDIC (Espagne-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1146, new CodePageEquivalence() { IBM_CCSID = 1146, IBM_Name = "UK ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1146, MS_DotNetName = "IBM01146 ", MS_AdditionalInfo = "IBM EBCDIC (RU-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1147, new CodePageEquivalence() { IBM_CCSID = 1147, IBM_Name = "FRENCH ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1147, MS_DotNetName = "IBM01147 ", MS_AdditionalInfo = "IBM EBCDIC (France-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1148, new CodePageEquivalence() { IBM_CCSID = 1148, IBM_Name = "INTL ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1148, MS_DotNetName = "IBM01148 ", MS_AdditionalInfo = "IBM EBCDIC (International-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1149, new CodePageEquivalence() { IBM_CCSID = 1149, IBM_Name = "ICELAND ECECP", IBM_EncodingScheme = "EBCDIC, SBCS", IBM_EBCDIC = true, MS_CodePage = 1149, MS_DotNetName = "IBM01149 ", MS_AdditionalInfo = "IBM EBCDIC (Islandais-Euro) " });
            DotNetCodePageFromIBMCCSID.Add(1168, new CodePageEquivalence() { IBM_CCSID = 1168, IBM_Name = "KOI8-U", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 21866, MS_DotNetName = "koi8-u ", MS_AdditionalInfo = "Cyrillique (KOI8-U) " });
            DotNetCodePageFromIBMCCSID.Add(1200, new CodePageEquivalence() { IBM_CCSID = 1200, IBM_Name = "UTF-16 BE with IBM PUA", IBM_EncodingScheme = "Unicode, UCS-2, including UTF-16 to allow for support of surrogates, Big Endian order", IBM_EBCDIC = false, MS_CodePage = 1200, MS_DotNetName = "utf-16 ", MS_AdditionalInfo = "Unicode " });
            DotNetCodePageFromIBMCCSID.Add(1201, new CodePageEquivalence() { IBM_CCSID = 1201, IBM_Name = "UTF-16 BE", IBM_EncodingScheme = "Unicode, UCS-2, including UTF-16 to allow for support of surrogates, Big Endian order", IBM_EBCDIC = false, MS_CodePage = 1201, MS_DotNetName = "unicodeFFFE ", MS_AdditionalInfo = "Unicode (avec primauté des octets de poids fort (big-endian)) " });
            DotNetCodePageFromIBMCCSID.Add(1209, new CodePageEquivalence() { IBM_CCSID = 1209, IBM_Name = "UTF-8", IBM_EncodingScheme = "UTF-8, UCS-2 transform", IBM_EBCDIC = false, MS_CodePage = 65001, MS_DotNetName = "utf-8 ", MS_AdditionalInfo = "Unicode (UTF-8) " });
            DotNetCodePageFromIBMCCSID.Add(1232, new CodePageEquivalence() { IBM_CCSID = 1232, IBM_Name = "UTF-32 BE with IBM PUA", IBM_EncodingScheme = "Unicode UTF-32, Big Endian order", IBM_EBCDIC = false, MS_CodePage = 12000, MS_DotNetName = "utf-32 ", MS_AdditionalInfo = "Unicode (UTF-32) " });
            DotNetCodePageFromIBMCCSID.Add(1233, new CodePageEquivalence() { IBM_CCSID = 1233, IBM_Name = "UTF-32 BE", IBM_EncodingScheme = "Unicode UTF-32, Big Endian order", IBM_EBCDIC = false, MS_CodePage = 12001, MS_DotNetName = "utf-32BE ", MS_AdditionalInfo = "Unicode (UTF-32 avec primauté des octets de poids fort (big-endian)) " });
            DotNetCodePageFromIBMCCSID.Add(1250, new CodePageEquivalence() { IBM_CCSID = 1250, IBM_Name = "MS-WIN LATIN-2", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1250, MS_DotNetName = "windows-1250 ", MS_AdditionalInfo = "Europe centrale (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1251, new CodePageEquivalence() { IBM_CCSID = 1251, IBM_Name = "MS-WIN CYRILLIC", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1251, MS_DotNetName = "windows-1251 ", MS_AdditionalInfo = "Cyrillique (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1252, new CodePageEquivalence() { IBM_CCSID = 1252, IBM_Name = "MS-WIN LATIN-1", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1252, MS_DotNetName = "Windows -1252 ", MS_AdditionalInfo = "Europe de l'Ouest (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1253, new CodePageEquivalence() { IBM_CCSID = 1253, IBM_Name = "MS-WIN GREEK", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1253, MS_DotNetName = "windows-1253 ", MS_AdditionalInfo = "Grec (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1254, new CodePageEquivalence() { IBM_CCSID = 1254, IBM_Name = "MS-WIN TURKEY", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1254, MS_DotNetName = "windows-1254 ", MS_AdditionalInfo = "Turc (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1255, new CodePageEquivalence() { IBM_CCSID = 1255, IBM_Name = "MS-WIN HEBREW", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1255, MS_DotNetName = "windows-1255 ", MS_AdditionalInfo = "Hébreu (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1256, new CodePageEquivalence() { IBM_CCSID = 1256, IBM_Name = "MS-WIN ARABIC", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1256, MS_DotNetName = "windows-1256 ", MS_AdditionalInfo = "Arabe (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1257, new CodePageEquivalence() { IBM_CCSID = 1257, IBM_Name = "MS-WIN BALTIC", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1257, MS_DotNetName = "windows-1257 ", MS_AdditionalInfo = "Baltique (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1258, new CodePageEquivalence() { IBM_CCSID = 1258, IBM_Name = "MS-WIN VIETNAM", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 1258, MS_DotNetName = "windows-1258 ", MS_AdditionalInfo = "Vietnamien (Windows) " });
            DotNetCodePageFromIBMCCSID.Add(1275, new CodePageEquivalence() { IBM_CCSID = 1275, IBM_Name = "APPLE LATIN-1", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10000, MS_DotNetName = "macintosh ", MS_AdditionalInfo = "Europe de l'Ouest (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1280, new CodePageEquivalence() { IBM_CCSID = 1280, IBM_Name = "APPLE GREEK", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10006, MS_DotNetName = "x-mac-greek ", MS_AdditionalInfo = "Grec (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1281, new CodePageEquivalence() { IBM_CCSID = 1281, IBM_Name = "APPLE TURKEY", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10081, MS_DotNetName = "x-mac-turkish ", MS_AdditionalInfo = "Turc (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1282, new CodePageEquivalence() { IBM_CCSID = 1282, IBM_Name = "APPLE LATIN2", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10029, MS_DotNetName = "x-mac-ce ", MS_AdditionalInfo = "Europe centrale (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1283, new CodePageEquivalence() { IBM_CCSID = 1283, IBM_Name = "APPLE CYRILLIC", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10007, MS_DotNetName = "x-mac-cyrillic ", MS_AdditionalInfo = "Cyrillique (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1284, new CodePageEquivalence() { IBM_CCSID = 1284, IBM_Name = "APPLE CROATIAN", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10082, MS_DotNetName = "x-mac-croatian ", MS_AdditionalInfo = "Croate (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1285, new CodePageEquivalence() { IBM_CCSID = 1285, IBM_Name = "APPLE ROMANIAN", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10010, MS_DotNetName = "x-mac-romanian ", MS_AdditionalInfo = "Roumain (Mac) " });
            DotNetCodePageFromIBMCCSID.Add(1286, new CodePageEquivalence() { IBM_CCSID = 1286, IBM_Name = "APPLE ICELANDIC", IBM_EncodingScheme = "ISO 8 (ASCII code), SBCS, Graphics in C1", IBM_EBCDIC = false, MS_CodePage = 10079, MS_DotNetName = "x-mac-icelandic ", MS_AdditionalInfo = "Islandais (Mac) " });

            foreach (CodePageEquivalence codePageEquivalence in DotNetCodePageFromIBMCCSID.Values)
            {
                IBMCCSIDFromDotNetCodePage.Add(codePageEquivalence.MS_CodePage, codePageEquivalence);
            }
        }

        private class CodePageEquivalence
        {
            public int IBM_CCSID;
            public string IBM_Name;
            public string IBM_EncodingScheme;
            public bool IBM_EBCDIC;
            public int MS_CodePage;
            public string MS_DotNetName;
            public string MS_AdditionalInfo;
        }
    }
}
