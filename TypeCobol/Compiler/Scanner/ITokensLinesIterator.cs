namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Common interface to iterate over tokens stored in TokensLines
    /// </summary>
    public interface ITokensLinesIterator
    {
        /// <summary>
        /// Name or path of the text document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PRGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        string DocumentPath { get; }

        /// <summary>
        /// Current line index in the main text document 
        /// (PROGRAM or CLASS source file).
        /// If a COPY directive was found on line index 12 in the main program file,
        /// when the iterator returns the first token of the secondary copy file :
        /// DocumentPath = "PGM1/CPY1"
        /// LineIndex = 0 (in file CPY1)
        /// LineIndexInMainDocument = 12 (in file PGM1)
        /// </summary>
        int LineIndexInMainDocument { get; }

        /// <summary>
        /// Current column index 
        /// (in the text document identified by DocumentPath)
        /// </summary>
        int ColumnIndex { get; }

        /// <summary>
        /// Current line index 
        /// (in the text document identified by DocumentPath)
        /// </summary>
        int LineIndex { get; }

        /// <summary>
        /// Current tokens line
        /// (in the text document identified by DocumentPath)
        /// </summary>
        ITokensLine CurrentLine { get;  }

        /// <summary>
        /// Returns the last token of the last line before EOF
        /// </summary>
        ITokensLine LastLine { get; }

        /// <summary>
        /// Get next token or EndOfFile
        /// </summary>
        Token NextToken();

        /// <summary>
        /// Get null (before the first call to NextToken()), current token, or EndOfFile
        /// </summary>
        Token CurrentToken { get; }

        /// <summary>
        /// Get an opaque object representing the current position of the iterator.
        /// Use it with the SeekToPosition method to restore this position later.
        /// </summary>
        object GetCurrentPosition();

        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        void SeekToPosition(object iteratorPosition);

        /// <summary>
        /// Sets the current iterator position to the beginning of the given line in the main document (zero-based index).
        /// After a call to this method, GetNextToken returns the first token of the given line.
        /// </summary>
        /// <param name="line"></param>
        void SeekToLineInMainDocument(int line);

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        void SaveCurrentPositionSnapshot();

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        void ReturnToLastPositionSnapshot();
    }
}
