using System;
using System.Text;
using JetBrains.Annotations;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Class used to group all the properties needed to load a CompilationDocument from a file
    /// </summary>
    public class DocumentFormat
    {
        public DocumentFormat(Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength, ColumnsLayout columnsLayout)
        {
            Encoding = encoding;
            EndOfLineDelimiter = endOfLineDelimiter;
            if (endOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
            {
                if (columnsLayout == ColumnsLayout.FreeTextFormat)
                {
                    throw new ArgumentException("With free text format, fixed length lines are not allowed");
                }
                else if (columnsLayout == ColumnsLayout.CobolReferenceFormat && fixedLineLength < 72)
                {
                    throw new ArgumentException("With Cobol reference format, fixed length lines must be at least 72 characters long");
                }
                FixedLineLength = fixedLineLength;
            }
            ColumnsLayout = columnsLayout;
        }

        /// <summary>
        /// Encoding used to read or write the Cobol file
        /// </summary>
        public Encoding Encoding { get; }

        /// <summary>
        /// Tells the compiler how to split a character stream in consecutive lines
        /// </summary>
        public EndOfLineDelimiter EndOfLineDelimiter { get; }

        /// <summary>
        /// Used only in case the text is formatted with fixed length line
        /// </summary>
        public int FixedLineLength { get; }

        /// <summary>
        /// Tells the compiler how to interpret the column position of each character in the source text
        /// </summary>
        public ColumnsLayout ColumnsLayout { get; }

        // Most often used document formats
        public static DocumentFormat ZOsReferenceFormat = new DocumentFormat(IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat);
        public static DocumentFormat RDZReferenceFormat = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat);
        public static DocumentFormat FreeTextFormat = new DocumentFormat(Encoding.GetEncoding(1252), EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
        public static DocumentFormat FreeUTF8Format = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);

        /// <summary>
        /// Used to retrieve TextArea type in Cobol reference format from token position
        ///
        /// </summary>
        public static TextAreaType GetTextAreaTypeInCobolReferenceFormat([NotNull]Token token)
        {
            if (token.Column < 7) return TextAreaType.SequenceNumber;
            else if (token.Column == 7) return TextAreaType.Indicator;
            else if (token.Column > 7 && token.Column < 12) return TextAreaType.AreaA;
            else if (token.Column > 11 && token.Column < 73) return TextAreaType.AreaB;
            else return TextAreaType.Comment;
        }
    }


}
