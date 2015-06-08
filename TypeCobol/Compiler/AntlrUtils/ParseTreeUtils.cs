using Antlr4.Runtime.Tree;
using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Utility methods to analyse Antlr parse trees
    /// </summary>
    internal static class ParseTreeUtils
    {
        // --- Get tokens in terminal nodes ---

        public static ITerminalNode GetFirstTerminalNode(IParseTree node)
        {
            while (!(node is ITerminalNode))
            {
                if (node.ChildCount == 0)
                {
                    return null;
                }
                else
                {
                    node = node.GetChild(0);
                }
            }
            return (ITerminalNode)node;
        }

        public static Token GetToken(ITerminalNode terminalNode)
        {
            return (Token)terminalNode.Symbol;
        }

        public static Token GetSymbolOrLiteralToken(IParseTree node)
        {
            ITerminalNode terminalNode = GetFirstTerminalNode(node);
            if (terminalNode != null)
            {
                return GetToken(terminalNode);
            }
            else
            {
                return null;
            }
        }

        // --- Get values from tokens in terminal nodes ---

        public static string GetUserDefinedWord(IParseTree node)
        {
            if (node != null)
            {
                ITerminalNode terminalNode = GetFirstTerminalNode(node);
                if (terminalNode != null)
                {
                    Token userDefinedWordToken = GetToken(terminalNode);
                    if (userDefinedWordToken.TokenType == TokenType.UserDefinedWord)
                    {
                        return userDefinedWordToken.Text;
                    }
                }
            }
            return null;
        }

        public static long? GetIntegerLiteral(IParseTree node)
        {
            if (node != null)
            {
                ITerminalNode terminalNode = GetFirstTerminalNode(node);
                if (terminalNode != null)
                {
                    Token integerLiteralToken = GetToken(terminalNode);
                    if (integerLiteralToken.TokenType == TokenType.IntegerLiteral)
                    {
                        return ((IntegerLiteralValue)integerLiteralToken.LiteralValue).Number;
                    }
                }
            }
            return null;
        }

        public static double? GetNumericLiteral(IParseTree node)
        {
            if (node != null)
            {
                ITerminalNode terminalNode = GetFirstTerminalNode(node);
                if (terminalNode != null)
                {
                    Token numericLiteralToken = GetToken(terminalNode);
                    if (numericLiteralToken.TokenType == TokenType.IntegerLiteral)
                    {
                        return ((IntegerLiteralValue)numericLiteralToken.LiteralValue).Number;
                    }
                    else if (numericLiteralToken.TokenType == TokenType.DecimalLiteral)
                    {
                        return ((DecimalLiteralValue)numericLiteralToken.LiteralValue).Number;
                    }
                    else if (numericLiteralToken.TokenType == TokenType.FloatingPointLiteral)
                    {
                        return ((FloatingPointLiteralValue)numericLiteralToken.LiteralValue).Number;
                    }
                }
            }
            return null;
        }

        public static string GetAlphanumericLiteral(IParseTree node)
        {
            if (node != null)
            {
                ITerminalNode terminalNode = GetFirstTerminalNode(node);
                if (terminalNode != null)
                {
                    Token alphaNumericLiteralToken = GetToken(terminalNode);
                    if (alphaNumericLiteralToken.TokenFamily == TokenFamily.AlphanumericLiteral)
                    {
                        return ((AlphanumericLiteralValue)alphaNumericLiteralToken.LiteralValue).Text;
                    }
                }
            }
            return null;
        }

        // --- Specific utility methods for the Compiler Directives parser --

        public static void TryGetUserDefinedWord(ITerminalNode terminalNode, ref string targetProperty)
        {
            if (terminalNode != null && targetProperty == null)
            {
                targetProperty = terminalNode.GetText();
            }
        }

        public static void TryGetAlphanumericLiteralValue(ITerminalNode terminalNode, ref string targetProperty)
        {
            if (terminalNode != null && targetProperty == null)
            {
                targetProperty = ((AlphanumericLiteralValue)((Token)terminalNode.Symbol).LiteralValue).Text;
            }
        }
    }
}
