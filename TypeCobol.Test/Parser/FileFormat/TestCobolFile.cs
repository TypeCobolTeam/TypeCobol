using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Parser.Scanner;

namespace TypeCobol.Test.Parser.FileFormat
{
    static class TestCobolFile
    {
        private static readonly Encoding _EncodingForAlphanumericLiterals =
#if EUROINFO_RULES
            IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147);
#else
            IBMCodePages.GetDotNetEncodingFromIBMCCSID(1140);
#endif
        public static readonly string SampleFolder = "Parser" + Path.DirectorySeparatorChar + "FileFormat" + Path.DirectorySeparatorChar + "Samples";
        public static void Check_EBCDICCobolFile()
        {
            DocumentFormat docFormat = DocumentFormat.ZOsReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(SampleFolder),
                false, new string[] { ".txt" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("EbcdicRefFormat", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                ReadOnlyTextDocument textDocument = new ReadOnlyTextDocument("EbcdicRefFormat.TXT", _EncodingForAlphanumericLiterals, docFormat.ColumnsLayout, false, cobolFile.ReadChars());
                // Send all text lines in one batch to the test observer
                textDocument.TextChanged += textSourceListener.OnTextChanged;
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
               tce1.NewLineMap.SequenceNumberText != "000010" || tce1.NewLineMap.IndicatorChar != ' ' ||
               tce1.NewLineMap.SourceText != "CBL DATA(31)                                                      " || tce1.NewLineMap.CommentText != "        ")
            {
                throw new Exception("Error reading line 1 of the EBCDIC text source");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[9], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 9 || tce2.Type != TextChangeType.LineInserted ||
               tce2.NewLineMap.SequenceNumberText != "000200" || tce2.NewLineMap.IndicatorChar != '*' ||
               tce2.NewLineMap.SourceText != "REMARKS. COPY=(YACMERM YACMCTL).                                 " || tce2.NewLineMap.CommentText != "00600000")
            {
                throw new Exception("Error reading line 10 of the EBCDIC text source");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[31], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 31 || tce3.Type != TextChangeType.LineInserted ||
               tce3.NewLineMap.SequenceNumberText != "000420" || tce3.NewLineMap.IndicatorChar != ' ' ||
               tce3.NewLineMap.SourceText != "    05  FILLER     PIC XXX    VALUE ' ; '.                       " || tce3.NewLineMap.CommentText != "00860000")
            {
                throw new Exception("Error reading line 32 of the EBCDIC text source");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[223], docFormat.ColumnsLayout);
            if (tce4.LineIndex != 223 || tce4.Type != TextChangeType.LineInserted ||
               tce4.NewLineMap.SequenceNumberText != "002340" || tce4.NewLineMap.IndicatorChar != '*' ||
               tce4.NewLineMap.SourceText != "    CALL 'ILBOABN0' USING LCP-ABND-CODE.                         " || tce4.NewLineMap.CommentText != "02740000")
            {
                throw new Exception("Error reading line 224 of the EBCDIC text source");
            }
        }

        public static void Check_EBCDICCobolFileWithUnsupportedChar()
        {
            DocumentFormat docFormat = DocumentFormat.ZOsReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(SampleFolder),
                false, new string[] { ".txt" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);
            
            bool exceptionWasThrownWithCorrectMessage = false;

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("EbcdicRefFormatWithBadChars", out cobolFile))
            {
                try
                {
                    // Load the CobolFile in a TextDocument;
                    ReadOnlyTextDocument textDocument = new ReadOnlyTextDocument("EbcdicRefFormatWithBadChars.TXT", _EncodingForAlphanumericLiterals, docFormat.ColumnsLayout, false, cobolFile.ReadChars());
                }
                catch(Exception e)
                {
                    if (e.Message == "The character code 13 in source encoding IBM EBCDIC (France-Euro) found at position 3072 can not be safely converted to the internal Unicode representation : please replace it with the alphanumeric hexadecimal literal X'0D' in the source text")
                    {
                        exceptionWasThrownWithCorrectMessage = true;
                    }
                }
            }

            if(!exceptionWasThrownWithCorrectMessage)
            {
                throw new Exception("Unsupported chars in fixed length EBCDIC source file were not correctly filtered");
            }
        }

        public static void Check_ASCIICobolFile_ReferenceFormat()
        {
            DocumentFormat docFormat = DocumentFormat.RDZReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(SampleFolder),
                false, new string[] { ".cpy" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("AsciiRefFormat", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                ReadOnlyTextDocument textDocument = new ReadOnlyTextDocument("MSVCOUT.cpy", _EncodingForAlphanumericLiterals, docFormat.ColumnsLayout, true, cobolFile.ReadChars());
                // Send all text lines in one batch to the test observer
                textDocument.TextChanged += textSourceListener.OnTextChanged;
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
                tce1.NewLineMap.SequenceNumberText != "000100" || tce1.NewLineMap.IndicatorChar != '*' ||
                tce1.NewLineMap.SourceText != "----------------------------------------------------------------*" || tce1.NewLineMap.CommentText != "00010000")
            {
                throw new Exception("Error reading line 1 of the ASCII text source (reference format)");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[114], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 114 || tce2.Type != TextChangeType.LineInserted ||
                tce2.NewLineMap.SequenceNumberText != "001780" || tce2.NewLineMap.IndicatorChar != ' ' ||
                tce2.NewLineMap.SourceText != "            20  FILLER                          PIC X(020).      " || tce2.NewLineMap.CommentText != "01550001")
            {
                throw new Exception("Error reading line 115 of the ASCII text source (reference format)");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[132], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 132 || tce3.Type != TextChangeType.LineInserted ||
                tce3.NewLineMap.SequenceNumberText != "002030" || tce3.NewLineMap.IndicatorChar != ' ' ||
                tce3.NewLineMap.SourceText != "          15  :MSVCOUT:-DataElm  OCCURS  10  TIMES               " || tce3.NewLineMap.CommentText != "01800001")
            {
                throw new Exception("Error reading line 132 of the ASCII text source (reference format)");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.Last<TextChange>(), docFormat.ColumnsLayout);
            if (tce4.LineIndex != 147 || tce4.Type != TextChangeType.LineInserted ||
                tce4.NewLineMap.SequenceNumberText != "002240" || tce4.NewLineMap.IndicatorChar != ' ' ||
                tce4.NewLineMap.SourceText != "    05                             PIC X(08) VALUE '/MSVCOUT'.   " || tce4.NewLineMap.CommentText != "02010001")
            {
                throw new Exception("Error reading line 224 of the ASCII text source (reference format)");
            }
        }

        public static void Check_ASCIICobolFile_LinuxReferenceFormat()
        {
            DocumentFormat docFormat = DocumentFormat.RDZReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(SampleFolder),
                false, null,
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("AsciiLinuxFormat.14", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                ReadOnlyTextDocument textDocument = new ReadOnlyTextDocument("AsciiLinuxFormat.14", _EncodingForAlphanumericLiterals, docFormat.ColumnsLayout, false, cobolFile.ReadChars());
                // Send all text lines in one batch to the test observer
                textDocument.TextChanged += textSourceListener.OnTextChanged;
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
                tce1.NewLineMap.SequenceNumberText != null || tce1.NewLineMap.IndicatorChar != ' ' ||
                tce1.NewLineMap.SourceText != string.Empty || tce1.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 1 of the ASCII Linux text source (reference format)");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[5], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 5 || tce2.Type != TextChangeType.LineInserted ||
                tce2.NewLineMap.SequenceNumberText != "      " || tce2.NewLineMap.IndicatorChar != ' ' ||
                tce2.NewLineMap.SourceText != "COPY \"copy.inc\"" || tce2.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 6 of the ASCII Linux text source (reference format)");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[7], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 7 || tce3.Type != TextChangeType.LineInserted ||
                tce3.NewLineMap.SequenceNumberText != "      " || tce3.NewLineMap.IndicatorChar != ' ' ||
                tce3.NewLineMap.SourceText != "               LEADING ==NORM== BY ==SECOND==." || tce3.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 8 of the ASCII Linux text source (reference format)");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[13], docFormat.ColumnsLayout);
            if (tce4.LineIndex != 13 || tce4.Type != TextChangeType.LineInserted ||
                tce4.NewLineMap.SequenceNumberText != "      " || tce4.NewLineMap.IndicatorChar != ' ' ||
                tce4.NewLineMap.SourceText != "    STOP RUN." || tce4.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 14 of the ASCII Linux text source (reference format)");
            }
        }

        public static void Check_ASCIICobolFile_FreeTextFormat()
        {
            DocumentFormat docFormat = DocumentFormat.FreeTextFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(SampleFolder),
                false, new string[] { ".cpy" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("AsciiFreeFormat", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                ReadOnlyTextDocument textDocument = new ReadOnlyTextDocument("AsciiFreeFormat.cpy", _EncodingForAlphanumericLiterals, docFormat.ColumnsLayout, true, cobolFile.ReadChars());
                // Send all text lines in one batch to the test observer
                textDocument.TextChanged += textSourceListener.OnTextChanged;
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
                tce1.NewLineMap.SequenceNumberText != null || tce1.NewLineMap.IndicatorChar != '/' ||
                tce1.NewLineMap.SourceText != "----------------------------------------------------------------" || tce1.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 1 of the ASCII text source (free text format)");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[4], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 4 || tce2.Type != TextChangeType.LineInserted ||
                tce2.NewLineMap.SequenceNumberText != null || tce2.NewLineMap.IndicatorChar != '*' ||
                tce2.NewLineMap.SourceText != " Comportant TAGs (ou BALISEs) standards/normalisés apposées via  commentaires standards à respecter                             " || tce2.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 5 of the ASCII text source (free text format)");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[7], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 7 || tce3.Type != TextChangeType.LineInserted ||
                tce3.NewLineMap.SequenceNumberText != null || tce3.NewLineMap.IndicatorChar != ' ' ||
                tce3.NewLineMap.SourceText != "      10                          PIC X(008) VALUE " || tce3.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 8 of the ASCII text source (free text format)");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[8], docFormat.ColumnsLayout);
            if (tce4.LineIndex != 8 || tce4.Type != TextChangeType.LineInserted ||
                tce4.NewLineMap.SequenceNumberText != null || tce4.NewLineMap.IndicatorChar != ' ' ||
                tce4.NewLineMap.SourceText != " 'MSVCINP '.   " || tce4.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 9 of the ASCII text source (free text format)");
            }

            TextChangeMap tce5 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[13], docFormat.ColumnsLayout);
            if (tce5.LineIndex != 13 || tce5.Type != TextChangeType.LineInserted ||
                tce5.NewLineMap.SequenceNumberText != null || tce5.NewLineMap.IndicatorChar != 'D' ||
                tce5.NewLineMap.SourceText != "       15  :MSVCINP:-AppSessnId           PIC X(064).           " || tce5.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 14 of the ASCII text source (free text format)");
            }

            TextChangeMap tce6 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[18], docFormat.ColumnsLayout);
            if (tce6.LineIndex != 18 || tce6.Type != TextChangeType.LineInserted ||
                tce6.NewLineMap.SequenceNumberText != null || tce6.NewLineMap.IndicatorChar != ' ' ||
                tce6.NewLineMap.SourceText != "    05  FILLER                             PIC X(499).           " || tce6.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 19 of the ASCII text source (free text format)");
            }

            TextChangeMap tce7 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.Last<TextChange>(), docFormat.ColumnsLayout);
            if (tce7.LineIndex != 27 || tce7.Type != TextChangeType.LineInserted ||
                tce7.NewLineMap.SequenceNumberText != null || tce7.NewLineMap.IndicatorChar != 'd' ||
                tce7.NewLineMap.SourceText != "   05                            PIC X(008) VALUE '/MSVCINP'.   " || tce7.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 28 of the ASCII text source (free text format)");
            }
        }

        public static void Check_UTF8File()
        {
            DocumentFormat docFormat = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(SampleFolder),
                false, null,
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            string filename = "UTF8Format.txt";
            CobolFile cobolFile;
            if (fileProvider.TryGetFile(filename, out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                ReadOnlyTextDocument textDocument = new ReadOnlyTextDocument(filename, _EncodingForAlphanumericLiterals, docFormat.ColumnsLayout, false, cobolFile.ReadChars());
                // Send all text lines in one batch to the test observer
                textDocument.TextChanged += textSourceListener.OnTextChanged;
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce;
            tce = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            CheckLine(filename, tce, 0, "english: hello, world");
            tce = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[1], docFormat.ColumnsLayout);
            bool okay = false;
            try { CheckLine(filename, tce, 1, "français: salut, tout le monde"); }
            catch (Exception) { okay = true; }
            if (!okay) throw new Exception("Exception should have been thrown!");
            CheckLine(filename, tce, 1, "arabic: مرحبا بالعالم");
            tce = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[2], docFormat.ColumnsLayout);
            CheckLine(filename, tce, 2, "japanese: こんにちは世界");
        }

        private static void CheckLine(string filename, TextChangeMap tce, int index, string expectedText, TextChangeType expectedTextChangeType = TextChangeType.LineInserted, string expectedSequenceNumber = null, string expectedComment = null)
        {
            bool hasError = false;
            string error = filename + "," + index + ": ";
            hasError = hasError | (tce.LineIndex != index);
            if (hasError)
            {
                error += "[line index: " + tce.LineIndex + ", expected: " + index + "]";
            }
            hasError = hasError | (tce.Type != expectedTextChangeType);
            if (hasError)
            {
                error += "[text change type: " + tce.Type + ", expected: " + expectedTextChangeType + "]";
            }
            hasError = hasError | (tce.NewLineMap.SequenceNumberText != expectedSequenceNumber);
            if (hasError)
            {
                error += "[sequence number area: " + tce.NewLineMap.SequenceNumberText + ", expected: " + expectedSequenceNumber + "]";
            }
            hasError = hasError | (tce.NewLineMap.CommentText != expectedComment);
            if (hasError)
            {
                error += "[comment area: " + tce.NewLineMap.CommentText + ", expected: " + expectedComment + "]";
            }
            hasError = hasError | (tce.NewLineMap.SourceText != expectedText);
            if (hasError)
            {
                error += "[text: " + tce.NewLineMap.SourceText + ", expected: " + expectedText + "]";
            }
            if (hasError)
            {
                throw new Exception(error);
            }
        }
    }

    class DummyTextSourceListener
    {
        public TextChangedEvent LastTextChangedEvent;

        public void OnTextChanged(object sender, TextChangedEvent textChangedEvent)
        {
            LastTextChangedEvent = textChangedEvent;
        }
    }
}
