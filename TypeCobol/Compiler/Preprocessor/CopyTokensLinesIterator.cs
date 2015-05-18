using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Iterator over tokens stored in a ProcessedTokensDocument.
    /// This iterator handles COPY directives : it returns the tokens from the main document
    /// AND all tokens imported from secondary documents.
    /// This iterator does not handle REPLACE directives : it simply returns REPLACE CompilerDirectiveTokens 
    /// which will be handled at another level by a ReplaceTokensLinesIterator.
    /// </summary>
    public class CopyTokensLinesIterator : ITokensLinesIterator
    {
        // Source data
        private ITextDocument textDocument;
        private IReadOnlyList<ProcessedTokensLine> tokensLines;

        // Start conditions
        private int channelFilter;

        // Iterator position
        private struct CopyTokensLinesIteratorPosition
        {
            // Main document position
            public int LineIndex;        
            public int TokenIndexInLine;

            // Imported document position 
            // (null if the iterator is not inside a COPY directive)
            public ITokensLinesIterator ImportedDocumentIterator;
            public object ImportedDocumentIteratorPosition;
        }

        // Current iterator position
        private CopyTokensLinesIteratorPosition currentPosition;

        // Main document line / token
        private ProcessedTokensLine currentLine;
        private Token currentToken;

        // Previous snapshot position
        private object snapshotPosition;

        /// <summary>
        /// Set the initial position of the iterator with startToken.
        /// Filter 
        /// </summary>
        public CopyTokensLinesIterator(ITextDocument textDocument, IReadOnlyList<ProcessedTokensLine> tokensLines, int channelFilter)
        {
            this.textDocument = textDocument;
            this.tokensLines = tokensLines;
            this.channelFilter = channelFilter;

            // Start just before the first token in the document
            Reset();
        }

        /// <summary>
        /// Resets the iterator position : before the first token of the document
        /// </summary>
        public void Reset()
        {
            currentPosition.LineIndex = 0;
            currentPosition.TokenIndexInLine = -1;
            if (tokensLines.Count > 0)
            {
                currentLine = tokensLines[0];
            }
            else
            {
                currentLine = null;
            }
            currentToken = null;
        }

        /// <summary>
        /// Text name of the document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PRGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        public string DocumentPath 
        {
            get 
            { 
                if(currentPosition.ImportedDocumentIterator == null)
                {
                    return textDocument.FileName;
                }
                else
                {
                    return textDocument.FileName + "/" + currentPosition.ImportedDocumentIterator.DocumentPath;
                }
            } 
        }

        /// <summary>
        /// Current line index in the main text document 
        /// (PROGRAM or CLASS source file).
        /// If a COPY directive was found on line index 12 in the main program file,
        /// when the iterator returns the first token of the secondary copy file :
        /// DocumentPath = "PGM1/CPY1"
        /// LineIndex = 0 (in file CPY1)
        /// LineIndexInMainDocument = 12 (in file PGM1)
        /// </summary>
        public int LineIndexInMainDocument 
        { 
            get
            {
                return currentPosition.LineIndex;
            }
        }

        /// <summary>
        /// Current column index
        /// </summary>
        public int ColumnIndex
        {
            get
            {
                if (currentPosition.ImportedDocumentIterator == null)
                {
                    if (currentToken == null)
                    {
                        return 0;
                    }
                    else
                    {
                        return currentToken.StopIndex;
                    }
                }
                else
                {
                    return currentPosition.ImportedDocumentIterator.ColumnIndex;
                }
            }
        }

        /// <summary>
        /// Current line index
        /// </summary>
        public int LineIndex
        {
            get
            {
                if (currentPosition.ImportedDocumentIterator == null)
                {
                    return currentPosition.LineIndex;
                }
                else
                {
                    return currentPosition.ImportedDocumentIterator.LineIndex;
                }
            }
        }

        /// <summary>
        /// Current character offset in the document
        /// </summary>
        public int Offset
        {
            get
            {
                if (currentPosition.ImportedDocumentIterator == null)
                {
                    int currentOffset = 0;
                    if (currentLine != null && currentToken != null)
                    {
                        currentOffset = currentLine.TextLineMap.TextLine.StartOffset + currentToken.StartIndex;
                    }
                    return currentOffset;
                }
                else
                {
                    return currentPosition.ImportedDocumentIterator.LineIndex;
                }   
            }
        }        

        /// <summary>
        /// Get an opaque object representing the current position of the iterator.
        /// Use it with the SeekToPosition method to restore this position later.
        /// </summary>
        public object GetCurrentPosition()
        {
            if (currentPosition.ImportedDocumentIterator != null)
            {
                currentPosition.ImportedDocumentIteratorPosition = currentPosition.ImportedDocumentIterator.GetCurrentPosition();
            }
            return currentPosition;
        }

        
        
        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        public void SeekToPosition(object iteratorPosition)
        {
            // Restore iterators positions
            currentPosition = (CopyTokensLinesIteratorPosition)iteratorPosition;
            if(currentPosition.ImportedDocumentIterator != null)
            {
                currentPosition.ImportedDocumentIterator.SeekToPosition(currentPosition.ImportedDocumentIteratorPosition);
            }

            // Restore current line & current token
            if (currentPosition.LineIndex >= 0)
            {
                currentLine = tokensLines[currentPosition.LineIndex];
            }
            else
            {
                currentLine = null;
            }
            if (currentPosition.TokenIndexInLine >= 0 && currentLine != null)
            {
                currentToken = currentLine.TokensWithCompilerDirectives[currentPosition.TokenIndexInLine];
            }
            else
            {
                currentToken = null;
            }
        }

        /// <summary>
        /// Get next token or EndOfFile
        /// </summary>
        public Token NextToken()
        {
            // If the document is empty or after end of file, immediately return EndOfFile
            if (currentLine == null)
            {
                return Token.END_OF_FILE;
            }

            // If the iterator is positioned in an imported document, return the next imported token
            if(currentPosition.ImportedDocumentIterator != null)
            {
                Token nextImportedToken = currentPosition.ImportedDocumentIterator.NextToken();
                if(nextImportedToken == Token.END_OF_FILE)
                {
                    currentPosition.ImportedDocumentIterator = null;
                    currentPosition.ImportedDocumentIteratorPosition = null;
                }
                else
                {
                    return nextImportedToken;
                }
            }

            // While we can find a next token
            currentToken = null;
            while (currentToken == null)
            {
                // try to find the next token on the same line
                currentPosition.TokenIndexInLine++;
                // but if we reached the end of the current line ...
                while (currentPosition.TokenIndexInLine >= currentLine.TokensWithCompilerDirectives.Count)
                {
                    // .. advance to next line
                    currentPosition.LineIndex++;
                    currentPosition.TokenIndexInLine = 0;
                    if (currentPosition.LineIndex < tokensLines.Count)
                    {
                        currentLine = tokensLines[currentPosition.LineIndex];
                    }
                    // and if we reached the last line of the document ...
                    else
                    {
                        // return EndOfFile
                        currentLine = null;
                        return Token.END_OF_FILE;
                    }
                }
                // Check if the next token found matches the filter criteria or is a COPY compiler directive or is a REPLACE directive
                Token nextTokenCandidate = currentLine.TokensWithCompilerDirectives[currentPosition.TokenIndexInLine];
                if (nextTokenCandidate.Channel == channelFilter || nextTokenCandidate.TokenType == TokenType.CopyImportDirective || nextTokenCandidate.TokenType == TokenType.ReplaceDirective)
                {
                    currentToken = nextTokenCandidate;
                }
            }

            // Check if the next token is a COPY import compiler directive
            if (currentToken.TokenType == TokenType.CopyImportDirective)
            {
                // Get next token in the imported document
                ImportedTokensDocument importedDocument = currentLine.ImportedDocuments[(CopyDirective)((CompilerDirectiveToken)currentToken).CompilerDirective];
                if (importedDocument != null)
                {
                    ITokensLinesIterator importedDocumentIterator = importedDocument.GetTokensIterator();
                    Token nextTokenCandidate = importedDocumentIterator.NextToken();

                    // No suitable next token found in the imported document
                    // -> get next token in the main document
                    if (nextTokenCandidate == Token.END_OF_FILE)
                    {
                        return NextToken();
                    }
                    // Start iterating in the imported document
                    else
                    {
                        currentPosition.ImportedDocumentIterator = importedDocumentIterator;
                        return nextTokenCandidate;
                    }
                }   
                // The reference to the ImportedDocument could not be resolved (error in an earlier phase)
                // -> get next token in the main document (fallback)
                else
                {
                    return NextToken();
                }
            }
            else
            {
                return currentToken;
            }
        }

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        public void SaveCurrentPositionSnapshot()
        {
            snapshotPosition = GetCurrentPosition();
        }

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        public void ReturnToLastPositionSnapshot()
        {
            SeekToPosition(snapshotPosition);
        }        
    }
}
