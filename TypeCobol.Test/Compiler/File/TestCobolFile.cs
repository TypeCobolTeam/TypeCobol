using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Scanner;

namespace TypeCobol.Test.Compiler.File
{
    static class TestCobolFile
    {
        public static void Check_EBCDICCobolFile()
        {
            DocumentFormat docFormat = DocumentFormat.ZOsReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(@"Compiler\File\Samples"), 
                false, new string[] { ".txt" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("ACMERMON", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                TextDocument textDocument = new TextDocument("ACMERMON.TXT", cobolFile.ReadChars(), docFormat.ColumnsLayout);
                // Send all text lines in one batch to the test observer
                textDocument.TextChangedEventsSource.Subscribe(textSourceListener);
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if(tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
               tce1.NewLineMap.SequenceNumberText != "000010" || tce1.NewLineMap.IndicatorChar != ' ' ||
               tce1.NewLineMap.SourceText != "CBL DATA(31)                                                      " || tce1.NewLineMap.CommentText != "        ") 
            {
                throw new Exception("Error reading line 1 of the EBCDIC text source");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[19], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 19 || tce2.Type != TextChangeType.LineInserted ||
               tce2.NewLineMap.SequenceNumberText != "000200" || tce2.NewLineMap.IndicatorChar != '*' ||
               tce2.NewLineMap.SourceText != "REMARKS. COPY=(YACMERM YACMCTL).                                 " || tce2.NewLineMap.CommentText != "00600000")
            {
                throw new Exception("Error reading line 20 of the EBCDIC text source");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[41], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 41 || tce3.Type != TextChangeType.LineInserted ||
               tce3.NewLineMap.SequenceNumberText != "000420" || tce3.NewLineMap.IndicatorChar != ' ' ||
               tce3.NewLineMap.SourceText != "    05  FILLER     PIC XXX    VALUE ' ; '.                       " || tce3.NewLineMap.CommentText != "00860000")
            {
                throw new Exception("Error reading line 42 of the EBCDIC text source");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.Last<TextChange>(), docFormat.ColumnsLayout);
            if (tce4.LineIndex != 233 || tce4.Type != TextChangeType.LineInserted ||
               tce4.NewLineMap.SequenceNumberText != "002340" || tce4.NewLineMap.IndicatorChar != '*' ||
               tce4.NewLineMap.SourceText != "    CALL 'ILBOABN0' USING LCP-ABND-CODE.                         " || tce4.NewLineMap.CommentText != "02740000")
            {
                throw new Exception("Error reading line 234 of the EBCDIC text source");
            }
        }

        public static void Check_ASCIICobolFile_ReferenceFormat()
        {            
            DocumentFormat docFormat = DocumentFormat.RDZReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(@"Compiler\File\Samples"),
                false, new string[] { ".cpy" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();
            
            CobolFile cobolFile;
            if (fileProvider.TryGetFile("MSVCOUT", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                TextDocument textDocument = new TextDocument("MSVCOUT.cpy", cobolFile.ReadChars(), docFormat.ColumnsLayout);
                // Send all text lines in one batch to the test observer
                textDocument.TextChangedEventsSource.Subscribe(textSourceListener);
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
                tce1.NewLineMap.SequenceNumberText != "000010" || tce1.NewLineMap.IndicatorChar != '*' ||
                tce1.NewLineMap.SourceText != "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-  " || tce1.NewLineMap.CommentText != "00001001")
            {
                throw new Exception("Error reading line 1 of the ASCII text source (reference format)");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[114], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 114 || tce2.Type != TextChangeType.LineInserted ||
                tce2.NewLineMap.SequenceNumberText != "001150" || tce2.NewLineMap.IndicatorChar != '*' ||
                tce2.NewLineMap.SourceText != "&     - Nom du programme en erreur                              *" || tce2.NewLineMap.CommentText != "00919501")
            {
                throw new Exception("Error reading line 115 of the ASCII text source (reference format)");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[202], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 202 || tce3.Type != TextChangeType.LineInserted ||
                tce3.NewLineMap.SequenceNumberText != "002030" || tce3.NewLineMap.IndicatorChar != ' ' ||
                tce3.NewLineMap.SourceText != "          15  :MSVCOUT:-DataElm  OCCURS  10  TIMES               " || tce3.NewLineMap.CommentText != "01800001")
            {
                throw new Exception("Error reading line 203 of the ASCII text source (reference format)");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.Last<TextChange>(), docFormat.ColumnsLayout);
            if (tce4.LineIndex != 223 || tce4.Type != TextChangeType.LineInserted ||
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
                PlatformUtils.GetPathForProjectFile(@"Compiler\File\Samples"),
                false, null,
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("copyb.14", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                TextDocument textDocument = new TextDocument("copyb.14", cobolFile.ReadChars(), docFormat.ColumnsLayout);
                // Send all text lines in one batch to the test observer
                textDocument.TextChangedEventsSource.Subscribe(textSourceListener);
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
                tce1.NewLineMap.SequenceNumberText != null || tce1.NewLineMap.IndicatorChar != ' ' ||
                tce1.NewLineMap.SourceText != null || tce1.NewLineMap.CommentText != null)
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

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.Last<TextChange>(), docFormat.ColumnsLayout);
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
                PlatformUtils.GetPathForProjectFile(@"Compiler\File\Samples"),
                false, new string[] { ".cpy" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            DummyTextSourceListener textSourceListener = new DummyTextSourceListener();

            CobolFile cobolFile;
            if (fileProvider.TryGetFile("MSVCINP free format", out cobolFile))
            {
                // Load the CobolFile in a TextDocument
                TextDocument textDocument = new TextDocument("MSVCINP free format.cpy", cobolFile.ReadChars(), docFormat.ColumnsLayout);
                // Send all text lines in one batch to the test observer
                textDocument.TextChangedEventsSource.Subscribe(textSourceListener);
                textDocument.StartSendingChangeEvents();
            }

            TextChangeMap tce1 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.First<TextChange>(), docFormat.ColumnsLayout);
            if (tce1.LineIndex != 0 || tce1.Type != TextChangeType.LineInserted ||
                tce1.NewLineMap.SequenceNumberText != null || tce1.NewLineMap.IndicatorChar != '/' ||
                tce1.NewLineMap.SourceText != "----------------------------------------------------------------" || tce1.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 1 of the ASCII text source (free text format)");
            }

            TextChangeMap tce2 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[9], docFormat.ColumnsLayout);
            if (tce2.LineIndex != 9 || tce2.Type != TextChangeType.LineInserted ||
                tce2.NewLineMap.SequenceNumberText != null || tce2.NewLineMap.IndicatorChar != '*' ||
                tce2.NewLineMap.SourceText != " Comportant TAGs (ou BALISEs) standards/normalisés apposées via  commentaires standards à respecter                             " || tce2.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 10 of the ASCII text source (free text format)");
            }

            TextChangeMap tce3 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[16], docFormat.ColumnsLayout);
            if (tce3.LineIndex != 16 || tce3.Type != TextChangeType.LineInserted ||
                tce3.NewLineMap.SequenceNumberText != null || tce3.NewLineMap.IndicatorChar != ' ' ||
                tce3.NewLineMap.SourceText != "      10                          PIC X(008) VALUE " || tce3.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 17 of the ASCII text source (free text format)");
            }

            TextChangeMap tce4 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[17], docFormat.ColumnsLayout);
            if (tce4.LineIndex != 17 || tce4.Type != TextChangeType.LineInserted ||
                tce4.NewLineMap.SequenceNumberText != null || tce4.NewLineMap.IndicatorChar != ' ' ||
                tce4.NewLineMap.SourceText != " 'MSVCINP '.   " || tce4.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 18 of the ASCII text source (free text format)");
            }

            TextChangeMap tce5 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[22], docFormat.ColumnsLayout);
            if (tce5.LineIndex != 22 || tce5.Type != TextChangeType.LineInserted ||
                tce5.NewLineMap.SequenceNumberText != null || tce5.NewLineMap.IndicatorChar != 'D' ||
                tce5.NewLineMap.SourceText != "       15  :MSVCINP:-AppSessnId           PIC X(064).           " || tce5.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 23 of the ASCII text source (free text format)");
            }

            TextChangeMap tce6 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges[27], docFormat.ColumnsLayout);
            if (tce6.LineIndex != 27 || tce6.Type != TextChangeType.LineInserted ||
                tce6.NewLineMap.SequenceNumberText != null || tce6.NewLineMap.IndicatorChar != ' ' ||
                tce6.NewLineMap.SourceText != "    05  FILLER                             PIC X(499).           " || tce6.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 28 of the ASCII text source (free text format)");
            }

            TextChangeMap tce7 = new TextChangeMap(textSourceListener.LastTextChangedEvent.TextChanges.Last<TextChange>(), docFormat.ColumnsLayout);
            if (tce7.LineIndex != 36 || tce7.Type != TextChangeType.LineInserted ||
                tce7.NewLineMap.SequenceNumberText != null || tce7.NewLineMap.IndicatorChar != 'd' ||
                tce7.NewLineMap.SourceText != "   05                            PIC X(008) VALUE '/MSVCINP'.   " || tce7.NewLineMap.CommentText != null)
            {
                throw new Exception("Error reading line 37 of the ASCII text source (free text format)");
            }
        }
    }

    class DummyTextSourceListener : IObserver<TextChangedEvent>
    {
        public TextChangedEvent LastTextChangedEvent;

        public void OnCompleted()
        {
            // Do nothing
        }

        public void OnError(Exception error)
        {
            // Propagate errors
            throw error;
        }

        public void OnNext(TextChangedEvent textChangedEvent)
        {
            LastTextChangedEvent = textChangedEvent;
        }
    }
}
