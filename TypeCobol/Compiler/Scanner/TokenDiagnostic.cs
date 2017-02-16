﻿using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Used to register a diagnostic specifically attached to a Token
    /// </summary>
    public class TokenDiagnostic : Diagnostic
    {
        internal TokenDiagnostic(MessageCode messageCode, Token token, int lineNumber, params object[] messageArgs) :
            base(messageCode, token.Column, token.EndColumn, lineNumber, messageArgs)
        {
            Token = token;
        }

        /// <summary>
        /// Token which is the subject of the diagnostics
        /// </summary>
        public Token Token { get; private set; }
    }
}
