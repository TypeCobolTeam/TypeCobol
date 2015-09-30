using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.File
{    
    /// <summary>
    /// Load or save a Cobol text document to a persistent file : local filesystem file, remote zOs PDS member, source control repository item
    /// </summary>
    public abstract class CobolFile : IDisposable
    {
        internal CobolFile(string name, Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength)
        {
            Name = name;
            Encoding = encoding;
            EndOfLineDelimiter = endOfLineDelimiter;
            if (endOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
            {
                FixedLineLength = fixedLineLength;
            }

            // Some characters from the source encoding may not support roundtrip Unicode decoding & encoding
            InitUnsupportedSourceCharCodes();
        }

        // -- File properties --

        /// <summary>
        /// Gets the simple text name of the Cobol file, as specified in a Cobol program (without path and extension)
        /// </summary>
        public string Name { get; protected set; }

        /// <summary>
        /// Absolute path, including file name and file extension, which can be used to retrieve the file in the underlying storage system 
        /// </summary>
        public string FullPath { get; protected set; }

        /// <summary>
        /// Gets the creation time of the Cobol file 
        /// </summary>
        public abstract DateTime CreationTime { get; }

        /// <summary>
        /// Gets the time when the Cobol file was last written to
        /// </summary>
        public abstract DateTime LastWriteTime { get; }

        /// <summary>
        /// Determines if the Cobol file is read only
        /// </summary>
        public abstract bool IsReadOnly { get; }

        /// <summary>
        /// Gets the size, in bytes, of the Cobol file
        /// </summary>
        public abstract long Length { get; }

        /// -- File format properties --

        /// <summary>
        /// Encoding used to read or write the Cobol file
        /// </summary>
        public Encoding Encoding { get; protected set; }

        /// <summary>
        /// Tells the compiler how to normalize the end of line characters
        /// </summary>
        public EndOfLineDelimiter EndOfLineDelimiter { get; protected set; }

        /// <summary>
        /// Used only in case the text is formatted with fixed length lines
        /// </summary>
        public int FixedLineLength { get; protected set; }

        /// <summary>
        /// This class will convert the text from the source file to Unicode with explicit CR LF line endings.
        /// The user may want to edit it in Unicode from and then save it back in its original encoding.
        /// For this scenario to work reliably, me must ensure two things at load time :
        /// - all chars read in the source file encoding must be convertible back and forth to and from Unicode
        /// - if the source file does not use line delimiter chars (fixed length lines), none of its chars should 
        ///   convert to CR or LF Unicode chars
        /// If any one of two conditions above is not true, the file can not be safely handled, and the compiler
        /// should refuse to load it by throwing a fatal exception.
        /// The solution is to replace the faulty characters in the source file with hexadecimal alphanumeric 
        /// literals and then try to reload the file.
        /// </summary>
        protected void InitUnsupportedSourceCharCodes()
        {
            // If the source file already encodes each char with several bytes, we should have no problem
            if (Encoding.IsSingleByte)
            {
                // Generate all possible source char codes
                byte[] sourceCharCodes = new byte[256];
                for (int charCode = 0; charCode <= 255; charCode++)
                {
                    sourceCharCodes[charCode] = (byte)charCode;
                }

                // Decode each one of them to a Unicode char, using the current Encoding
                char[] unicodeChars = Encoding.GetChars(sourceCharCodes);

                // Encode then back to the source Encoding
                byte[] roundtripCharCodes = Encoding.GetBytes(unicodeChars);

                // Check if the roundtrip conversion was successful
                unsupportedSourceCharCodes = new bool[256];
                bool encodingHasAtLeastOneUnsupportedChar = false;
                for(int charCode = 0; charCode <= 255; charCode++)
                {
                    if (sourceCharCodes[charCode] == roundtripCharCodes[charCode] &&
                       (EndOfLineDelimiter == EndOfLineDelimiter.CrLfCharacters || (unicodeChars[charCode] != '\r' && unicodeChars[charCode] != '\n')))
                    {
                        unsupportedSourceCharCodes[charCode] = false;
                    }
                    else
                    {
                        encodingHasAtLeastOneUnsupportedChar = true;
                        unsupportedSourceCharCodes[charCode] = true;
                    }
                }
                if(!encodingHasAtLeastOneUnsupportedChar)
                {
                    unsupportedSourceCharCodes = null;
                }
            }
        }
        private bool[] unsupportedSourceCharCodes;


        // -- File operations --

        /// <summary>
        /// Overload this method to get a stream to read the binary contents of the file
        /// </summary>
        protected abstract Stream OpenInputStream();

        /// <summary>
        /// Overload this method to get a stream to write a new binary content to the file
        /// </summary>
        /// <returns></returns>
        protected abstract Stream OpenOutputStream();
        
        /// <summary>
        /// Reads the characters of the source file as Unicode characters.
        /// Inserts additional Cr/Lf characters at end of line for fixed length lines.
        /// </summary>    
        public IEnumerable<char> ReadChars()
        {
            IList<char> chars = new List<char>();
            using (Stream inputStream = OpenInputStream())
            {
                Stream safeInputStream = (unsupportedSourceCharCodes == null) ? inputStream : new FilteringStream(inputStream, Encoding, unsupportedSourceCharCodes);
                using (StreamReader inputStreamReader = new StreamReader(safeInputStream, Encoding))
                {
                    if (EndOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
                    {
                        int lineLength = FixedLineLength;
                        char[] lineBuffer = new char[lineLength];
                        int charsCount = 0;
                        while ((charsCount = inputStreamReader.Read(lineBuffer, 0, lineLength)) > 0)
                        {
                            for (int i = 0; i < charsCount; i++)
                            {
                                chars.Add(lineBuffer[i]);
                            }
                            if (charsCount == lineLength)
                            {
                                chars.Add('\r');
                                chars.Add('\n');
                            }
                        }
                    }
                    else if (EndOfLineDelimiter == EndOfLineDelimiter.CrLfCharacters)
                    {
                        int chr = -1;
                        while ((chr = inputStreamReader.Read()) >= 0)
                        {
                            chars.Add((char)chr);
                        }
                    }
                }
            }
            return chars;
        }

        /// <summary>
        /// Writes the Unicode characters of a text document to a binary source file.
        /// Removes Cr/Lf characters at end of line for fixed length lines.
        /// </summary>    
        public void WriteChars(IEnumerable<char> textChars)
        {
            using (Stream outputStream = OpenOutputStream())
            {
                Stream safeOutputStream = (unsupportedSourceCharCodes == null) ? outputStream : new FilteringStream(outputStream, Encoding, unsupportedSourceCharCodes);
                using (StreamWriter outputStreamWriter = new StreamWriter(safeOutputStream, Encoding))
                {
                    if (EndOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
                    { 
                        int charsCountInCurrentLine = 0;
                        bool previousCharWasEndOfLine = false;
                        foreach(char chr in textChars)
                        {
                            if(chr == '\r' || chr == '\n')
                            {
                                if(!previousCharWasEndOfLine && charsCountInCurrentLine < FixedLineLength)
                                {
                                    for(int i=charsCountInCurrentLine ; i < FixedLineLength ; i++)
                                    {
                                        outputStreamWriter.Write(' ');
                                    }
                                }
                                previousCharWasEndOfLine = true;
                                charsCountInCurrentLine = 0;
                            }
                            else
                            {
                                outputStreamWriter.Write(chr);
                                charsCountInCurrentLine++;
                                previousCharWasEndOfLine = false;
                            }
                        }
                    }
                    else if (EndOfLineDelimiter == EndOfLineDelimiter.CrLfCharacters)
                    {
                        foreach(char chr in textChars)
                        {
                            outputStreamWriter.Write(chr);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Observers can subscribe to be notified of any external change applied to the Cobol file 
        /// </summary>
        public abstract IObservable<CobolFileChangedEvent> CobolFileChangedEventsSource { get; }

        /// <summary>
        /// Starts monitoring the external changes applied to the Cobol file (service stopped by default)
        /// </summary>
        public abstract void StartMonitoringExternalChanges();

        /// <summary>
        /// Stops monitoring the external changes applied to the Cobol file
        /// </summary>
        public abstract void StopMonitoringExternalChanges();

        /// <summary>
        /// Release all resources acquired to implement and monitor this Cobol file
        /// </summary>
        public abstract void Dispose();

        /// <summary>
        /// Utility class used to filter source char codes if necessary 
        /// </summary>
        class FilteringStream : Stream
        {
            private Stream baseStream;
            private Encoding sourceEncoding;
            private bool[] unsupportedSourceCharCodes;
            
            public FilteringStream(Stream baseStream, Encoding sourceEncoding, bool[] unsupportedSourceCharCodes)
            {
                this.baseStream = baseStream;
                this.sourceEncoding = sourceEncoding;
                this.unsupportedSourceCharCodes = unsupportedSourceCharCodes;
            }
            
            private void CheckUnsupportedCharCodes(byte[] buffer, int offset, int count)
            {
                for(int index = offset; index < offset + count; index++)
                {
                    if(unsupportedSourceCharCodes[buffer[index]])
                    {
                        throw new NotSupportedException(String.Format("The character code {0} in source encoding {1} found at position {2} can not be safely converted to the internal Unicode representation : please replace it with the alphanumeric hexadecimal literal X'{0:X2}' in the source text", buffer[index], sourceEncoding.EncodingName, Position));
                    }
                }
            }

            public override bool CanRead
            {
                get
                {
                    return baseStream.CanRead;
                }
            }

            public override bool CanSeek
            {
                get
                {
                    return baseStream.CanSeek;
                }
            }

            public override bool CanWrite
            {
                get
                {
                    return baseStream.CanWrite;
                }
            }

            public override long Length
            {
                get
                {
                    return baseStream.Length;
                }
            }

            public override long Position
            {
                get
                {
                    return baseStream.Position;
                }

                set
                {
                    baseStream.Position = value;
                }
            }

            public override void Flush()
            {
                baseStream.Flush();
            }

            public override int Read(byte[] buffer, int offset, int count)
            {
                CheckUnsupportedCharCodes(buffer, offset, count);
                return baseStream.Read(buffer, offset, count);
            }

            public override long Seek(long offset, SeekOrigin origin)
            {
                return baseStream.Seek(offset, origin);
            }

            public override void SetLength(long value)
            {
                baseStream.SetLength(value);
            }

            public override void Write(byte[] buffer, int offset, int count)
            {
                CheckUnsupportedCharCodes(buffer, offset, count);
                baseStream.Write(buffer, offset, count);
            }
        }
    }

    /// <summary>
    /// Indicates how a Cobol file reader can detect the end of a line
    /// </summary>
    public enum EndOfLineDelimiter
    {
        /// <summary>
        /// No line delimiters : split the characters stream by counting a fixed number of characters for each line.
        /// </summary>
        FixedLengthLines,
        /// <summary>
        /// Characters "\n","\r", or "\r\n" delimit two consecutive lines.
        /// NB: they can not appear in alphanumeric or national literals.
        /// </summary>
        CrLfCharacters
    }
}
