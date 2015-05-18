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

        public static long GetIntegerLiteralValue(ITerminalNode terminalNode)
        {
            return ((IntegerLiteralValue)((Token)(terminalNode.Symbol)).LiteralValue).Number;
        }

        public static Token GetToken(ITerminalNode terminalNode)
        {
            return (Token)terminalNode.Symbol;
        }

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
    }
}
