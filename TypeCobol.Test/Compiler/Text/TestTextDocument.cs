using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Text
{
    static class TestTextDocument
    {
        public static void Check_DocumentFormatExceptions()
        {
            bool exceptionOK = false;
            try
            {
                DocumentFormat docFormat = new DocumentFormat(Encoding.Default, EndOfLineDelimiter.FixedLengthLines, 0, ColumnsLayout.FreeTextFormat);
            }
            catch (Exception e)
            {
                if (e is ArgumentException) exceptionOK = true;
            }
            if (!exceptionOK)
            {
                throw new Exception("Free text format with fixed length lines should not be allowed");
            }

            exceptionOK = false;
            try
            {
                DocumentFormat docFormat = new DocumentFormat(Encoding.Default, EndOfLineDelimiter.FixedLengthLines, 71, ColumnsLayout.CobolReferenceFormat);
            }
            catch (Exception e)
            {
                if (e is ArgumentException) exceptionOK = true;
            }
            if (!exceptionOK)
            {
                throw new Exception("Cobol reference text format with fixed length lines < 72 should not be allowed");
            }

            // But Cobol reference text format with fixed length lines == 72 should be allowed
            DocumentFormat docFormat2 = new DocumentFormat(Encoding.Default, EndOfLineDelimiter.FixedLengthLines, 72, ColumnsLayout.CobolReferenceFormat);
        }

        public static void Check_EmptyDocument()
        {
            TextDocument textDocument = new TextDocument("empty", String.Empty, ColumnsLayout.CobolReferenceFormat);

            Exception resultException = null;
            try
            {
                textDocument.CharAt(-1);
            }
            catch(Exception e)
            {
                resultException = e;
            }
            if(resultException == null || !(resultException is InvalidOperationException))
            {
                throw new Exception("Any attempt to get char at -1 invalid offset should result in an InvalidOperationException");
            }
            resultException = null;
            try
            {
                textDocument.CharAt(0);
            }
            catch (Exception e)
            {
                resultException = e;
            }
            if (resultException == null || !(resultException is InvalidOperationException))
            {
                throw new Exception("Any attempt to get char at 0 invalid offset should result in an InvalidOperationException");
            }

            if(textDocument.Chars.Count() != 0)
            {
                throw new Exception("An empty document should have 0 chars");
            }

            if(textDocument.FileName != "empty")
            {
                throw new Exception("File name should be \"empty\"");
            }

            resultException = null;
            try
            {
                textDocument.GetLineByIndex(-1);
            }
            catch (Exception e)
            {
                resultException = e;
            }
            if (resultException == null || !(resultException is InvalidOperationException))
            {
                throw new Exception("Any attempt to get line at -1 invalid index should result in an InvalidOperationException");
            }
            resultException = null;
            try
            {
                textDocument.GetLineByIndex(0);
            }
            catch (Exception e)
            {
                resultException = e;
            }
            if (resultException == null || !(resultException is InvalidOperationException))
            {
                throw new Exception("Any attempt to get line at 0 invalid index should result in an InvalidOperationException");
            }

            resultException = null;
            int indexOfCharInLine;
            try
            {
                textDocument.GetLineByOffset(-1, out indexOfCharInLine);
            }
            catch (Exception e)
            {
                resultException = e;
            }
            if (resultException == null || !(resultException is InvalidOperationException))
            {
                throw new Exception("Any attempt to get line at -1 invalid index should result in an InvalidOperationException");
            }
            resultException = null;
            try
            {
                textDocument.GetLineByOffset(0, out indexOfCharInLine);
            }
            catch (Exception e)
            {
                resultException = e;
            }
            if (resultException == null || !(resultException is InvalidOperationException))
            {
                throw new Exception("Any attempt to get line at 0 invalid index should result in an InvalidOperationException");
            }

            if (textDocument.Length != 0)
            {
                throw new Exception("An empty document should have length 0");
            }

            if (textDocument.LineCount != 0)
            {
                throw new Exception("An empty document should have 0 line count");
            }

            if (textDocument.Lines.Count() != 0)
            {
                throw new Exception("An empty document should have 0 lines");
            }
        }

        public static void Check_ReferenceFormatDocument()
        {
            DocumentFormat docFormat = DocumentFormat.RDZReferenceFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(@"Compiler\Text\Samples"),
                false, new string[] { ".cpy" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            CobolFile cobolFile;
            if (!fileProvider.TryGetFile("MSVCOUT", out cobolFile))
            {
                throw new Exception("File MSVCOUT.cpy not found");
            }
                
            // Load the CobolFile in a TextDocument
            TextDocument textDocument = new TextDocument("MSVCOUT.cpy", cobolFile.ReadChars(), docFormat.ColumnsLayout);
            
            if(textDocument.CharAt(0) != '0')
            {
                throw new Exception("Character at position 0 should be 0");
            }
            if (textDocument.CharAt(88) != 'M')
            {
                throw new Exception("Character at position 88 should be M");
            }
            if (textDocument.CharAt(17899) != '/')
            {
                throw new Exception("Character at position 17899 should be /");
            }
            if (textDocument.CharAt(17919) != '1')
            {
                throw new Exception("Character at position 17191 should be 1");
            }

            if(textDocument.Chars.Skip(88).First() != 'M')
            {
                throw new Exception("Character enumerator after 88 iterations should return M");
            }

            ITextLine line = textDocument.GetLineByIndex(0);
            if(line.Length != 80 || line.LineIndex != 0 || line.StartOffset != 0 ||
                line.Text != "000010*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-  00001001")
            {
                throw new Exception("Incorrect line at index 0");
            }
            line = textDocument.GetLineByIndex(1);
            if (line.Length != 80 || line.LineIndex != 1 || line.StartOffset != 80 ||
                line.Text != "000020*-Maintenance frame - Created on 14 Oct 2013 at 17:27:08          00002001")
            {
                throw new Exception("Incorrect line at index 1");
            }
            line = textDocument.GetLineByIndex(223);
            if (line.Length != 80 || line.LineIndex != 223 || line.StartOffset != 17840 ||
                line.Text != "002240     05                             PIC X(08) VALUE '/MSVCOUT'.   02010001")
            {
                throw new Exception("Incorrect line at index 223");
            }

            int indexOfCharInLine = -1;
            line = textDocument.GetLineByOffset(12, out indexOfCharInLine);
            if (line.LineIndex != 0 || indexOfCharInLine != 12)
            {
                throw new Exception("Incorrect line at offset 12");
            }
            line = textDocument.GetLineByOffset(159, out indexOfCharInLine);
            if (line.LineIndex != 1 || indexOfCharInLine != 79)
            {
                throw new Exception("Incorrect line at offset 79");
            }
            line = textDocument.GetLineByOffset(17899, out indexOfCharInLine);
            if (line.LineIndex != 223 || indexOfCharInLine != 59)
            {
                throw new Exception("Incorrect line at offset 17899");
            }

            if (textDocument.Length != 17920)
            {
                throw new Exception("Document should have length 17920");
            }

            if (textDocument.LineCount != 224)
            {
                throw new Exception("Document should have 224 line count");
            }

            if (textDocument.Lines.Count() != 224)
            {
                throw new Exception("Document should have 224 lines");
            }
        }

        public static void Check_FreeFormatDocument()
        {
            DocumentFormat docFormat = DocumentFormat.FreeTextFormat;

            SourceFileProvider fileProvider = new SourceFileProvider();
            fileProvider.AddLocalDirectoryLibrary(
                PlatformUtils.GetPathForProjectFile(@"Compiler\Text\Samples"),
                false, new string[] { ".cpy" },
                docFormat.Encoding, docFormat.EndOfLineDelimiter, docFormat.FixedLineLength);

            CobolFile cobolFile;
            if (!fileProvider.TryGetFile("MSVCINP free format", out cobolFile))
            {
                throw new Exception("File MSVCINP free format.cpy not found");
            }

            // Load the CobolFile in a TextDocument
            TextDocument textDocument = new TextDocument("MSVCINP free format.cpy", cobolFile.ReadChars(), docFormat.ColumnsLayout);

            if (textDocument.CharAt(0) != '/')
            {
                throw new Exception("Character at position 0 should be 0");
            }
            if (textDocument.CharAt(90) != 'a')
            {
                throw new Exception("Character at position 90 should be a");
            }
            if (textDocument.CharAt(2510) != '/')
            {
                throw new Exception("Character at position 2510 should be /");
            }
            if (textDocument.CharAt(2522) != ' ')
            {
                throw new Exception("Character at position 2522 should be space");
            }

            if (textDocument.Chars.Skip(90).First() != 'a')
            {
                throw new Exception("Character enumerator after 90 iterations should return a");
            }

            ITextLine line = textDocument.GetLineByIndex(0);
            if (line.Length != 65 || line.LineIndex != 0 || line.StartOffset != 0 ||
                line.Text != "/----------------------------------------------------------------")
            {
                throw new Exception("Incorrect line at index 0");
            }
            line = textDocument.GetLineByIndex(1);
            if (line.Length != 96 || line.LineIndex != 1 || line.StartOffset != 65 ||
                line.Text != "* MSVCINP               partie ALLER FIXE des MESSAGES échangés avec tout SERVICE APPLICATIF C14")
            {
                throw new Exception("Incorrect line at index 1");
            }
            line = textDocument.GetLineByIndex(36);
            if (line.Length != 66 || line.LineIndex != 36 || line.StartOffset != 2457 ||
                line.Text != "d    05                            PIC X(008) VALUE '/MSVCINP'.   ")
            {
                throw new Exception("Incorrect line at index 36");
            }

            int indexOfCharInLine = -1;
            line = textDocument.GetLineByOffset(12, out indexOfCharInLine);
            if (line.LineIndex != 0 || indexOfCharInLine != 12)
            {
                throw new Exception("Incorrect line at offset 12");
            }
            line = textDocument.GetLineByOffset(881, out indexOfCharInLine);
            if (line.LineIndex != 12 || indexOfCharInLine != 5)
            {
                throw new Exception("Incorrect line at offset 981");
            }
            line = textDocument.GetLineByOffset(2509, out indexOfCharInLine);
            if (line.LineIndex != 36 || indexOfCharInLine != 52)
            {
                throw new Exception("Incorrect line at offset 2509");
            }

            if (textDocument.Length != 2523)
            {
                throw new Exception("Document should have length 2523");
            }

            if (textDocument.LineCount != 37)
            {
                throw new Exception("Document should have 37 line count");
            }

            if (textDocument.Lines.Count() != 37)
            {
                throw new Exception("Document should have 37 lines");
            }
        }
    }
}
