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
            using (Stream inputStream = OpenInputStream())
            {
                using (StreamReader inputStreamReader = new StreamReader(inputStream, Encoding))
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
                                yield return lineBuffer[i];
                            }
                            if (charsCount == lineLength)
                            {
                                yield return '\r';
                                yield return '\n';
                            }
                        }
                    }
                    else if (EndOfLineDelimiter == EndOfLineDelimiter.CrLfCharacters)
                    {
                        int chr = -1;
                        while ((chr = inputStreamReader.Read()) >= 0)
                        {
                            yield return (char)chr;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Writes the Unicode characters of a text document to a binary source file.
        /// Removes Cr/Lf characters at end of line for fixed length lines.
        /// </summary>    
        public void WriteChars(IEnumerable<char> textChars)
        {
            using (Stream outputStream = OpenOutputStream())
            {
                using (StreamWriter outputStreamWriter = new StreamWriter(outputStream, Encoding))
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
    }

    /// <summary>
    /// 
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
