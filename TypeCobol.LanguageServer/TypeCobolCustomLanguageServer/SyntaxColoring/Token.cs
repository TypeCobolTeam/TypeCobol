﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring
{
    /// <summary>
    /// All Token types 
    /// </summary>
    public enum TokenType
    {
        Unknown = 0,
        FormalComment = 1
    }
    /// <summary>
    /// A Token representation
    /// </summary>
    public class Token
    {
        /// <summary>
        /// The Token Type
        /// </summary>
        public TokenType Type { get; set; }

        /// <summary>
        /// Token range
        /// </summary>
        public VsCodeProtocol.Range Range { get; set; }

        /// <summary>
        /// The Token lexeme, can be null
        /// </summary>
        public String Lexeme { get; set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Token's type</param>
        /// <param name="range">Token's range</param>
        public Token(TokenType type, VsCodeProtocol.Range range) : this(type, range, null)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Token's type</param>
        /// <param name="range">Token's range</param>
        /// <param name="lexeme">Token's lexeme</param>
        public Token(TokenType type, VsCodeProtocol.Range range, String lexeme)
        {
            this.Type = type;
            this.Range = range;
            this.Lexeme = lexeme;
        }
    }
}
