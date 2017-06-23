using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Action action to remove a Node from the generated code.
    /// The Removed node will be commented, all its children will be cleared.
    /// If the erase action contains words to be erased from the input, a new GenerateNode is built
    /// and its ouput is the input without the word to be erased:
    /// 
    /// ex: move unsafe  dateFreeFormat to maDate
    /// 
    /// will be generated with the word unsafe to be erased to:
    /// 
    /// *    move unsafe  dateFreeFormat to maDate                              
    /// move         dateFreeFormat to maDate                              
    /// 
    /// </summary>
    public class Erase : EventArgs, Action, IEraseAction
    {
        public string Group { get; private set; }
        internal Node Node;
        private List<string> Words;

        /// <summary>
        /// Get the list of Erased Nodes
        /// </summary>
        public IList<Node> ErasedNodes
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The node to be erased with input word</param>
        /// <param name="word">The word to erase </param>
        public Erase(Node node, string word)
        {
            this.Node = node;
            this.Words = new List<string> { word };
        }

        /// <summary>
        /// The list of erased Tokens if any.
        /// </summary>
        private List<Qualifier.GenerateToken> ErasedTokens
        {
            get;
            set;
        }
        /// <summary>
        /// Determines if in the consumed tokens there are erased words.
        /// </summary>
        private bool HasErasedWord
        {
            get
            {
                if (ErasedTokens == null)
                {
                    ErasedTokens = new List<Qualifier.GenerateToken>();
                    string word = Words[0].ToLower();
                    if (Node.CodeElement.ConsumedTokens != null)
                    {
                        foreach (TypeCobol.Compiler.Scanner.Token token in Node.CodeElement.ConsumedTokens)
                        {
                            if (token.Text.ToLower().Equals(word))
                            {
                                Qualifier.GenerateToken item = new Qualifier.GenerateToken(new Qualifier.TokenCodeElement(token), new string(' ', word.Length), null);
                                ErasedTokens.Add(item);
                            }
                        }
                    }
                }
                return ErasedTokens.Count > 0;
            }
        }

        /// <summary>
        /// Perform the action.
        /// </summary>
        public void Execute()
        {
            if (!HasErasedWord)
                return;
            //Add all Erased Tokens
            foreach (Qualifier.GenerateToken token in ErasedTokens)
            {
                this.Node.Add(token);
            }
            // comment out original "line" (=~ non expanded node)
            this.Node.Comment = true;
        }
    }
}