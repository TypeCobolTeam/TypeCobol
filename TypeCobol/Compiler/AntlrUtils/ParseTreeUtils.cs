using Antlr4.Runtime.Tree;
using System;
using TypeCobol.Compiler.Scanner;
using System.Collections.Generic;
using TypeCobol.Compiler.Parser.Generated;

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

        public static Token GetTokenFromTerminalNode(ITerminalNode terminalNode)
        {
			var ce = terminalNode.Symbol as CodeElements.CodeElement;
			if (ce != null) {
				if (ce.ConsumedTokens.Count < 1) return null;
				return ce.ConsumedTokens[0];
			}
			return (Token)terminalNode.Symbol;
        }

        public static Token GetFirstToken(IParseTree node)
        {
            if (node != null)
            {
                ITerminalNode terminalNode = GetFirstTerminalNode(node);
                if (terminalNode != null)
                {
                    return GetTokenFromTerminalNode(terminalNode);
                }
                else
                {
                    return null;
                }
            }
            else
            {
                return null;
            }
        }

        public static IList<Token> GetTokensList(IParseTree node)
        {
            IList<Token> tokens = new List<Token>();
            if(node is ITerminalNode)
            {
                tokens.Add(GetTokenFromTerminalNode((ITerminalNode)node));
            }
            else
            {
                for(int i=0; i<node.ChildCount; i++)
                {
                    var childNode = node.GetChild(i);
                    if(childNode is ITerminalNode)
                    {
                        tokens.Add(GetTokenFromTerminalNode((ITerminalNode)childNode));
                    }
                }
            }
            return tokens;
        }

        // --- Get values from tokens in terminal nodes ---

        public static string GetUserDefinedWord(IParseTree node)
        {
            if (node != null)
            {
                ITerminalNode terminalNode = GetFirstTerminalNode(node);
                if (terminalNode != null)
                {
                    Token userDefinedWordToken = GetTokenFromTerminalNode(terminalNode);
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
                    Token integerLiteralToken = GetTokenFromTerminalNode(terminalNode);
                    if (integerLiteralToken.TokenType == TokenType.IntegerLiteral)
                    {
                        return ((IntegerLiteralTokenValue)integerLiteralToken.LiteralValue).Number;
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
                    Token numericLiteralToken = GetTokenFromTerminalNode(terminalNode);
                    if (numericLiteralToken.TokenType == TokenType.IntegerLiteral)
                    {
                        return ((IntegerLiteralTokenValue)numericLiteralToken.LiteralValue).Number;
                    }
                    else if (numericLiteralToken.TokenType == TokenType.DecimalLiteral)
                    {
                        return ((DecimalLiteralTokenValue)numericLiteralToken.LiteralValue).Number;
                    }
                    else if (numericLiteralToken.TokenType == TokenType.FloatingPointLiteral)
                    {
                        return ((FloatingPointLiteralTokenValue)numericLiteralToken.LiteralValue).Number;
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
                    Token alphaNumericLiteralToken = GetTokenFromTerminalNode(terminalNode);
                    if (alphaNumericLiteralToken.TokenFamily == TokenFamily.AlphanumericLiteral)
                    {
                        return ((AlphanumericLiteralTokenValue)alphaNumericLiteralToken.LiteralValue).Text;
                    }
                }
            }
            return null;
        }

	}
}
