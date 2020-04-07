using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Tools;

namespace TypeCobol.Codegen.Nodes
{
    using System;
    using System.Collections.Generic;
    using System.Text;
    using Actions;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.CodeModel;
    using TypeCobol.Compiler.Nodes;
    using TypeCobol.Compiler.Text;

    internal class TypedDataNode : DataDescription, Generated
    {
        #region Strategies for FlushConsumedTokens

        /// <summary>
        /// Base class of strategies for FlushConsumedTokens method.
        /// By default, this strategy will output all tokens using their text representation.
        /// </summary>
        private abstract class TokenFlushStrategy
        {
            private static readonly Func<Token, string> _DefaultTokenRender = token => token.Text;

            private readonly Func<Token, string> _tokenRender;

            /// <summary>
            /// Constructor.
            /// </summary>
            /// <param name="tokenRender">Custom token renderer to override default behavior.</param>
            protected TokenFlushStrategy(Func<Token, string> tokenRender = null)
            {
                _tokenRender = tokenRender ?? _DefaultTokenRender;
            }

            /// <summary>
            /// Indicates whether a Token should be written or not.
            /// </summary>
            /// <param name="token">Candidate token.</param>
            /// <returns>True to write the token, False to discard it.</returns>
            public virtual bool ShouldOutput(Token token)
            {
                return true;
            }

            /// <summary>
            /// Returns the text representation of a Token.
            /// </summary>
            /// <param name="token">Token to be written.</param>
            /// <returns>String object representing the token.</returns>
            public virtual string GetTokenText(Token token)
            {
                return _tokenRender(token);
            }
        }

        private class OutputAllTokens : TokenFlushStrategy
        {
            // Use default behavior of abstract class.
        }

        /// <summary>
        /// This strategy extracts the scalar definition of a TYPEDEF if any.
        /// </summary>
        private class ExtractScalarTypedef : TokenFlushStrategy
        {
            private readonly bool _ignoreGlobal;
            private bool _typedefSeen;

            public ExtractScalarTypedef(bool ignoreGlobal)
            {
                _ignoreGlobal = ignoreGlobal;
                _typedefSeen = false;
            }

            public override bool ShouldOutput(Token token)
            {
                // Don't output anything until we reach the TYPEDEF keyword.
                if (!_typedefSeen)
                {
                    _typedefSeen = token.TokenType == TokenType.TYPEDEF;
                    return false;
                }

                // Skip TYPEDEF-related keywords (kind : STRICT/STRONG and visibility : PUBLIC/PRIVATE).
                // Don't output the PeriodSeparator.
                if (token.TokenType == TokenType.STRONG || token.TokenType == TokenType.STRICT || token.TokenType == TokenType.PUBLIC || token.TokenType == TokenType.PRIVATE || token.TokenType == TokenType.PeriodSeparator)
                {
                    return false;
                }

                // Special case for the GLOBAL clause.
                if (_ignoreGlobal && token.TokenType == TokenType.GLOBAL)
                {
                    return false;
                }

                return true;
            }
        }

        /// <summary>
        /// This strategy only outputs the VALUE clause of a declaration.
        /// </summary>
        private class ExtractValue : TokenFlushStrategy
        {
            private bool _valueSeen;
            private bool _stop;

            public ExtractValue()
            {
                _valueSeen = false;
                _stop = false;
            }

            public override bool ShouldOutput(Token token)
            {
                // Don't output anything until we reach the VALUE keyword.
                if (!_valueSeen)
                {
                    _valueSeen = token.TokenType == TokenType.VALUE;
                    return _valueSeen; // VALUE keyword itself is outputted.
                }

                // We already have outputted everything necessary.
                if (_stop)
                {
                    return false;
                }

                // Stop on next keyword following the VALUE clause.
                var tokenFamily = token.TokenFamily;
                if (tokenFamily == TokenFamily.SyntaxKeyword || tokenFamily == TokenFamily.CobolV6Keyword ||
                    tokenFamily == TokenFamily.Cobol2002Keyword || tokenFamily == TokenFamily.TypeCobolKeyword ||
                    token.TokenType == TokenType.PeriodSeparator)
                {
                    _stop = true;
                    return false;
                }

                return true;
            }
        }

        /// <summary>
        /// This strategy outputs everything after the picture clause (including the picture clause itself).
        /// </summary>
        private class ExtractStartingFromPicture : TokenFlushStrategy
        {
            private bool _picSeen;

            public ExtractStartingFromPicture()
            {
                _picSeen = false;
            }

            public override bool ShouldOutput(Token token)
            {
                // Don't output anything until we reach the PICTURE clause.
                if (!_picSeen)
                {
                    _picSeen = token.TokenType == TokenType.PIC || token.TokenType == TokenType.PICTURE;
                    return _picSeen;
                }

                return true;
            }
        }

        /// <summary>
        /// This strategy outputs everything after the level.
        /// </summary>
        private class ExtractAfterLevel : TokenFlushStrategy
        {
            private bool _levelSeen;

            public ExtractAfterLevel(Func<Token, string> tokenRender)
                : base(tokenRender)
            {
                _levelSeen = false;
            }

            public override bool ShouldOutput(Token token)
            {
                // Don't output anything until we reach the Level declaration.
                if (!_levelSeen)
                {
                    _levelSeen = token.TokenType == TokenType.LevelNumber;
                    return false;
                }

                return true;
            }
        }

        /// <summary>
        /// This strategy extracts the type info in a declaration. This can be VALUE clause, usage, GLOBAL clause, etc.
        /// It doesn't output the TYPE clause for typed data/parameter and it also doesn't include the omittable (question mark) for parameter.
        /// </summary>
        private class ExtractTypeInfo : TokenFlushStrategy
        {
            private bool _skipNext;

            public ExtractTypeInfo(Func<Token, string> tokenRender)
                : base(tokenRender)
            {
                _skipNext = true; // First token is always skipped as it is level or name.
            }

            public override bool ShouldOutput(Token token)
            {
                // Don't output data name (which usually follows level) and type name (which follows TYPE keyword).
                // As type name can be qualified, we also skip what follows a :: separator.
                if (token.TokenType == TokenType.LevelNumber || token.TokenType == TokenType.TYPE || token.TokenType == TokenType.QualifiedNameSeparator)
                {
                    _skipNext = true;
                    return false; // The TYPE keyword and :: separator are skipped as well.
                }

                // Previous token analysis showed that we have to skip current token, so return false and reset.
                if (_skipNext)
                {
                    _skipNext = false;
                    return false;
                }

                // Tokens that should not be outputted : QuestionMark in optional parameter declarations and PeriodSeparator at end.
                return token.TokenType != TokenType.PeriodSeparator && token.TokenType != TokenType.QUESTION_MARK;
            }
        }

        #endregion

        private DataDescription Node;
        public TypedDataNode(DataDescription node) : base(node.CodeElement) { this.Node = node; }


        private List<ITextLine> _cache = null;
        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                if (_cache == null)
                {
                    _cache = new List<ITextLine>();
                    if (this.Node.IsPartOfATypeDef) return _cache;

                    var data = this.Node.CodeElement;
                    if (data.LevelNumber != null)
                    {
                        int level = (int) (data.LevelNumber.Value);
                        var customtype = this.Node.TypeDefinition;
                        //collect root procedure
                        List<string> rootProcedures;
                        //Collect from level 01 Pure Cobol85 root variables                    
                        List<Tuple<string, string>> rootVars;
                        GeneratorHelper.ComputeTypedProperPaths(this, data, customtype, out rootProcedures, out rootVars);
                        _cache.AddRange(CreateDataDefinition(this.Node, this.Node.SymbolTable, Layout, rootProcedures, rootVars, customtype, data, level, 0, true, true, customtype));
                        _cache.AddRange(InsertChildren(Layout, rootProcedures, rootVars, customtype, level + 1));
                    }
                }
                return _cache;
            }
        }

        /// <summary>
        /// Writes tokens to a StringBuilder.
        /// </summary>
        /// <param name="layout">Target layout.</param>
        /// <param name="consumedTokens">Source tokens.</param>
        /// <param name="sb">StringBuilder to write to.</param>
        /// <param name="strategy">Object to control which and how tokens are written.</param>
        /// <param name="hasSeenGlobal">Indicates whether a GLOBAL keyword has been written.</param>
        /// <param name="hasSeenPeriod">Indicates whether a Period separator has been written.</param>
        private static void FlushConsumedTokens(ColumnsLayout? layout, IList<Token> consumedTokens, StringBuilder sb, TokenFlushStrategy strategy, out bool hasSeenGlobal, out bool hasSeenPeriod)
        {
            System.Diagnostics.Contracts.Contract.Assert(consumedTokens != null);
            System.Diagnostics.Contracts.Contract.Assert(sb != null);
            System.Diagnostics.Contracts.Contract.Assert(strategy != null);

            hasSeenGlobal = false;
            hasSeenPeriod = false;

            // No need to keep going if there are no tokens...
            if (consumedTokens.Count == 0) return;

            // Skip any formalized comment block at the beginning of the consumed tokens collection.
            int i = -1;
            TokenFamily tokenFamily;
            do
            {
                tokenFamily = consumedTokens[++i].TokenFamily;
            }
            while (tokenFamily == TokenFamily.FormalizedCommentsFamily && i < consumedTokens.Count);

            /*
             * Reference token to handle line breaks, it is initialized with the first consumed token which is not part of a formalized comment.
             * NOTE : if we ran out of tokens while skipping the formalized comment at the beginning, we simply initialize the reference token to null
             * and then the method ends due to the condition in the following while loop.
             */
            Token referenceTokenForCurrentLine = i < consumedTokens.Count ? consumedTokens[i] : null;
            while (i < consumedTokens.Count)
            {
                Token token = consumedTokens[i];

                // Should the token be written or not ? We are also discarding comment tokens here.
                if (token.TokenFamily == TokenFamily.Comments || token.TokenFamily == TokenFamily.MultilinesCommentsFamily || !strategy.ShouldOutput(token))
                {
                    i++;
                    continue;
                }

                // Handle spacing with previous token written.
                if (i != consumedTokens.Count - 1 || token.TokenType != TokenType.PeriodSeparator)
                {
                    if (referenceTokenForCurrentLine.Line == token.Line)
                    {
                        sb.Append(string.Intern(" "));
                    }
                }

                // Handle new line.
                if (referenceTokenForCurrentLine.Line != token.Line)
                {
                    int nPad = token.Column;
                    if (layout.HasValue)
                    {
                        if (layout.Value == ColumnsLayout.CobolReferenceFormat)
                        {
                            nPad = Math.Max(0, token.Column - 8);
                        }
                    }
                    string pad = new string(' ', nPad);
                    sb.Append('\n');
                    sb.Append(pad);
                    referenceTokenForCurrentLine = token;
                }

                // Write token.
                sb.Append(strategy.GetTokenText(token));

                // Memorize special tokens GLOBAL and Period.
                hasSeenGlobal |= token.TokenType == TokenType.GLOBAL;
                hasSeenPeriod |= token.TokenType == TokenType.PeriodSeparator;

                i++;
            }
        }

        /// <summary>
        /// Retrieve the consumed Token as a String
        /// </summary>
        /// <param name="data_def">The DataDefintion to Retrieve the consumed Tokens.</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <returns>The string representing the DataDefinition</returns>
        internal static string ConsumedTokenToString(ColumnsLayout? layout, DataDefinitionEntry data_def, out bool bHasPeriod)
        {
            StringBuilder sb = new StringBuilder();
            FlushConsumedTokens(layout, data_def.ConsumedTokens, sb, new OutputAllTokens(), out _, out bHasPeriod);
            return sb.ToString();
        }

        /// <summary>
        /// Get the first token of the gien type in the List
        /// </summary>
        /// <param name="tokens">The List of tokens</param>
        /// <param name="type">The Token Type to find</param>
        /// <returns>The corresponding token if any, null otherwise</returns>
        internal static Token GetToken(IList<Token> tokens, TokenType type)
        {
            return tokens?.FirstOrDefault(t => t.TokenType == type);
        }

        /// <summary>
        /// Tries to detect a TYPEDEF construction for a scalar type.
        /// </summary>
        /// <param name="layout">Columns layout kind.</param>
        /// <param name="dataDescriptionEntry">The data desc entry of the node</param>
        /// <param name="customtype">The TypeDef definition node</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <param name="bIgnoreGlobal">true if the GLOBAL keyword must be ignored</param>
        /// <returns>The string representing the TYPEDEF type</returns>
        internal static string ExtractAnyCobolScalarTypeDef(ColumnsLayout? layout, DataDescriptionEntry dataDescriptionEntry, TypeDefinition customtype, out bool bHasPeriod, bool bIgnoreGlobal)
        {
            StringBuilder sb = new StringBuilder();
            FlushConsumedTokens(layout, customtype.CodeElement.ConsumedTokens, sb, new ExtractScalarTypedef(bIgnoreGlobal), out _, out bHasPeriod);
            FlushConsumedTokens(layout, dataDescriptionEntry.ConsumedTokens, sb, new ExtractValue(), out _, out bHasPeriod);
            return sb.ToString();
        }

        /// <summary>
        /// Tries to extract all remaining tokens after a Type Name.
        /// </summary>
        /// <param name="customtype">The Type definition node</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <returns>The string representing the Tokens after TYPE Name</returns>
        internal static string ExtractTokensValuesAfterTypeName(ColumnsLayout? layout, DataDescriptionEntry dataDescEntry, out bool bHasPeriod,
            out bool globalSeen,
            Func<Compiler.Scanner.Token, string> tokenFilter = null)
        {
            StringBuilder sb = new StringBuilder();
            FlushConsumedTokens(layout, dataDescEntry.ConsumedTokens, sb, new ExtractTypeInfo(tokenFilter), out globalSeen, out bHasPeriod);
            return sb.ToString();
        }

        /// <summary>
        /// Tries to detect a (PIC|PICTURE) construction for a Data Description Entry.
        /// </summary>
        /// <param name="dataDescEntry">The Data Description Entry Node</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <returns>The string representing the PIC clause </returns>
        internal static string ExtractPicTokensValues(ColumnsLayout? layout, DataDescriptionEntry dataDescEntry, out bool bHasPeriod, out bool globalSeen)
        {
            return ExtractPicTokensValues(layout, dataDescEntry.ConsumedTokens, out bHasPeriod, out globalSeen);
        }

        /// <summary>
        /// Tries to detect a (PIC|PICTURE) construction for a Data Description Entry.
        /// </summary>
        /// <param name="dataDescEntry">The Data Description Entry Node</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <returns>The string representing the PIC clause </returns>
        internal static string ExtractPicTokensValues(ColumnsLayout? layout, IList<Compiler.Scanner.Token> consumedTokens, out bool bHasPeriod, out bool globalSeen)
        {
            var sb = new StringBuilder();
            FlushConsumedTokens(layout, consumedTokens, sb, new ExtractStartingFromPicture(), out globalSeen, out bHasPeriod);
            return sb.ToString();
        }

        /// <summary>
        /// Extract All Tokens after encountering a LevelNumber token
        /// </summary>
        /// <param name="dataDescEntry">The Data Description Entry Node</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <returns>The string representing all tokens after a LevelNumber </returns>
        internal static string ExtractTokensValuesAfterLevel(ColumnsLayout? layout, DataDescriptionEntry dataDescEntry, out bool bHasPeriod, Func<Compiler.Scanner.Token, string> tokenFilter = null)
        {
            StringBuilder sb = new StringBuilder();
            FlushConsumedTokens(layout, dataDescEntry.ConsumedTokens, sb, new ExtractAfterLevel(tokenFilter), out _, out bHasPeriod);
            return sb.ToString();
        }

        /// <summary>
        /// Convert a line to a list of TextLines.
        /// </summary>
        /// <param name="line">The line to convert.</param>
        /// <returns>The list of TextLines</returns>
        internal static List<ITextLine> LineToTextLines(string line)
        {
            List<ITextLine> lines = new List<ITextLine>();
            int lfIndex = line.IndexOf('\n');
            if (lfIndex >= 0)
            {
                char[] sep = { '\n' };
                string[] items = line.Split(sep);
                foreach (var item in items)
                {
                    TextLineSnapshot tl = new TextLineSnapshot(-1, item, null);
                    lines.Add(tl);
                }
            }
            else
            {
                TextLineSnapshot tl = new TextLineSnapshot(-1, line, null);
                lines.Add(tl);
            }
            return lines;
        }

        /// <summary>
        /// Build the Dictionary that associate a token of an index-name-1 to its hash + name.
        /// </summary>
        /// <param name="indexes">The Array of Index Symbol Definition</param>
        /// <param name="ownerDefinition">The Owner of the definition that contains the INDEXED BY clause</param>
        /// <returns>The Dictionary</returns>
        private static Dictionary<Compiler.Scanner.Token, string> BuildIndexMap(Node rootNode, List<string> rootProcedures, List<Tuple<string, string>> rootVariableName, SymbolDefinition[] indexes, TypeCobol.Compiler.Nodes.DataDefinition ownerDefinition)
        {
            Dictionary<Compiler.Scanner.Token, string> map = new Dictionary<Compiler.Scanner.Token, string>(indexes.Length);
            List<string> pathProcedures;
            List<string> pathVariables;
            GeneratorHelper.ComputeProperPaths(ownerDefinition, out pathProcedures, out pathVariables);
            List<string> list_items = new List<string>();

            //Add root procedures
            if (rootProcedures.Count == 0)
            {
                list_items.AddRange(pathProcedures);
            }
            else
            {
                for (int j = rootProcedures.Count - 1; j >= 0; j--)
                {
                    list_items.Add(rootProcedures[j]);
                }
            }

            //Add Root variables
            for (int j = rootVariableName.Count - 1; j >= 0; j--)
            {
                list_items.Add(rootVariableName[j].Item1);
                if (j != 0 && rootVariableName[j].Item2 != null && rootVariableName[j].Item2.Trim().Length > 0)
                    list_items.Add(rootVariableName[j].Item2);
            }

            list_items.AddRange(pathVariables);
            string qn = string.Join(".", list_items.Where(item => item != null));

            // If indexes directly come from their DataDef, use the computed path (qn). If they come from a TypeDef, the path is the rootNode.QualifiedName.
            AddIndexMap(rootNode, rootNode == ownerDefinition ? qn : rootNode.QualifiedName.ToString(), indexes, map);

            return map;
        }

        /// <summary>
        /// Add In the Index Map all indices that match an IndexDefinition children node of a parent node.
        /// </summary>
        /// <param name="parentNode">The parent Node</param>
        /// <param name="qn">Root qualified name of the indes</param>
        /// <param name="indexes">SymbolDefinition[] indexes</param>
        /// <param name="map">Map of token indexes to their hashname</param>
        private static void AddIndexMap(Node parentNode, string qn, SymbolDefinition[] indexes, Dictionary<Compiler.Scanner.Token, string> map)
        {
            foreach (Node child in parentNode.Children)
            {
                if (child is IndexDefinition)
                {
                    IndexDefinition index = child as IndexDefinition;
                    foreach (SymbolDefinition sym in indexes)
                    {
                        if (sym.Name.Equals(index.Name))
                        {
                            string qualified_name = qn + '.' + index.Name;
                            string hash_name = index.IsFlagSet(Flag.IndexUsedWithQualifiedName) ? GeneratorHelper.ComputeIndexHashName(qualified_name, parentNode) : index.Name;
                            map[sym.NameLiteral.Token] = hash_name;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Pre Generation calculation for collection variable path access and index variable map.
        /// </summary>
        /// <param name="table">The Current Symbol Table</param>
        /// <param name="rootProcedures">Root procedures</param>
        /// <param name="rootVariableName">All current root variable</param>
        /// <param name="ownerDefinition">The Owner of the current definition</param>
        /// <param name="data_def">The current definition</param>
        /// <param name="bHasDependingOn">[out] true if the current variable hace depending on variables, false otherwise</param>
        /// <param name="bHasIndexes">[out] true if the current variable definition have indexed variables, fals eotherwise.</param>
        /// <param name="dependingOnAccessPath">[out] depending on variables access path list</param>
        /// <param name="indexesMap">[out] Indexed variable map to tokens</param>
        internal static void PreGenDependingOnAndIndexed(Node rootNode, SymbolTable table, List<string> rootProcedures, List<Tuple<string, string>> rootVariableName, TypeCobol.Compiler.Nodes.DataDefinition ownerDefinition, DataDefinitionEntry data_def,
            out bool bHasDependingOn,
            out bool bHasIndexes,
            out List<string> dependingOnAccessPath,
            out Dictionary<Compiler.Scanner.Token, string> indexesMap
            )
        {
            var data = data_def as DataDescriptionEntry;
            bHasDependingOn = false;
            bHasIndexes = false;
            dependingOnAccessPath = null;
            indexesMap = null;
            if (data?.OccursDependingOn != null)
            {
                if (!data.OccursDependingOn.MainSymbolReference.IsQualifiedReference)
                {
                    dependingOnAccessPath = new List<string>();
                    if (LookupAccessPathForName(table, ownerDefinition, data.OccursDependingOn.MainSymbolReference.Name.ToLower(), dependingOnAccessPath))
                    {   //Remove the Type name
                        dependingOnAccessPath.RemoveAt(0);
                        dependingOnAccessPath.Reverse();
                        dependingOnAccessPath.AddRange(rootVariableName.ConvertAll<string>(vt => vt.Item1));
                        bHasDependingOn = true;
                    }
                }
                else
                {
                    dependingOnAccessPath = new List<string>();
                    QualifiedSymbolReference qualSymRef = (QualifiedSymbolReference)data.OccursDependingOn.MainSymbolReference;
                    string tailName = qualSymRef.Tail.Name;
                    if (LookupAccessPathForName(table, ownerDefinition, tailName.ToLower(), dependingOnAccessPath))
                    {
                        //Remove the type name
                        dependingOnAccessPath.RemoveAt(0);
                        //Remove the variable
                        dependingOnAccessPath.RemoveAt(dependingOnAccessPath.Count - 1);
                        if (dependingOnAccessPath.Count > 0)
                        {
                            dependingOnAccessPath.Reverse();
                            dependingOnAccessPath.AddRange(rootVariableName.ConvertAll<string>(vt => vt.Item1));
                            bHasDependingOn = true;
                        }
                    }
                }
            }
            if (data?.Indexes != null)
            {
                bHasIndexes = true;
                //So Children of the owner definition contains all indexes
                indexesMap = BuildIndexMap(rootNode, rootProcedures, rootVariableName, data.Indexes, ownerDefinition);
            }
        }

        /// <summary>
        /// Post generation calculation of data definition having depending on or indexed variables.
        /// </summary>
        /// <param name="ownerDefinition">The Owner of the current definition</param>
        /// <param name="data_def">The current definition</param>
        /// <param name="bHasDependingOn">true if the current variable hace depending on variables, false otherwise</param>
        /// <param name="bHasIndexes">true if the current variable definition have indexed variables, fals eotherwise.</param>
        /// <param name="dependingOnAccessPath">Depending on variables access path list</param>
        /// <param name="indexesMap">Indexed variable map to tokens</param>
        internal static void PostGenDependingOnAndIndexed(TypeCobol.Compiler.Nodes.DataDefinition ownerDefinition, DataDefinitionEntry data_def,  bool bHasDependingOn, bool bHasIndexes,
            List<string> dependingOnAccessPath,
            Dictionary<Compiler.Scanner.Token, string> indexesMap,
            out Func<Compiler.Scanner.Token, string> depenOnTokenFilter,
            out Func<Compiler.Scanner.Token, string> indexedByTokenFilter
            )
        {
            var data = data_def as DataDescriptionEntry;
            depenOnTokenFilter = null;
            indexedByTokenFilter = null;
            if (bHasIndexes)
            {
                indexedByTokenFilter = (token) =>
                {
                    return indexesMap.ContainsKey(token) ? indexesMap[token] : token.Text;
                };
            }
            if (bHasDependingOn)
            {
                if (data != null && !data.OccursDependingOn.MainSymbolReference.IsQualifiedReference)
                    depenOnTokenFilter = (token) =>
                    {
                        if (bHasIndexes)
                        {
                            if (indexesMap.ContainsKey(token))
                                return indexesMap[token];
                        }
                        if (token == data.OccursDependingOn.MainSymbolReference.NameLiteral.Token)
                        { return string.Join(" OF ", dependingOnAccessPath.ToArray()); }
                        else
                        { return token.Text; }
                    };
                else
                {   //We have an incomplete qualification to the root variable
                    depenOnTokenFilter = (token) =>
                    {
                        if (bHasIndexes)
                        {
                            if (indexesMap.ContainsKey(token))
                                return indexesMap[token];
                        }
                        QualifiedSymbolReference qualSymRef = (QualifiedSymbolReference)data?.OccursDependingOn.MainSymbolReference;
                        if (qualSymRef != null && qualSymRef.IsTypeCobolQualifiedReference)
                        {
                            DataDescription dataDescription = ownerDefinition as DataDescription;
                            if (dataDescription?.QualifiedTokenSubsitutionMap != null && dataDescription.QualifiedTokenSubsitutionMap.ContainsKey(token))
                            {
                                if (token == qualSymRef.Head.NameLiteral.Token)
                                    return dataDescription.QualifiedTokenSubsitutionMap[token] + " OF " + string.Join(" OF ", dependingOnAccessPath.ToArray());
                                else
                                    return dataDescription.QualifiedTokenSubsitutionMap[token];
                            }
                            else
                            { return token.Text; }
                        }
                        else
                        {   //Pure Cobol85 Qualification add left qualification to the root
                            if (qualSymRef != null && token == qualSymRef.Tail.NameLiteral.Token)
                            {
                                return token.Text + " OF " + string.Join(" OF ", dependingOnAccessPath.ToArray());
                            }
                            else
                            {
                                return token.Text;
                            }
                        }
                    };
                }
            }
        }

        /// <summary>
        /// Append in the given StringBuilder the name and any global attribute of the the given DataDefinition object.
        /// </summary>
        /// <param name="buffer">The String Buffer</param>
        /// <param name="dataDef">The Data Definition object</param>
        /// <param name="globalSeen">Global token has been seen already</param>
        internal static void AppendNameAndGlobalDataDef(StringBuilder buffer, DataDefinitionEntry dataDef, bool globalSeen)
        {
            // Write data name if any.
            if (dataDef.Name != null)
            {
                buffer.Append(' ').Append(dataDef.Name);
            }

            if (dataDef is CommonDataDescriptionAndDataRedefines dataDesc)
            {
                // Write FILLER keyword if any. FILLER is mutually exclusive with name so no need to check that data name is null again.
                if (dataDesc.Filler != null)
                {
                    buffer.Append(' ').Append(dataDesc.Filler.Token.Text);
                }

                // Write GLOBAL modifier if not already seen and originally present.
                if (!globalSeen && dataDesc.IsGlobal)
                {
                    buffer.Append(' ').Append(dataDesc.Global.Token.Text);
                }
            }
        }

        /// <summary>
        /// Creates text lines corresponding to the given COPY directive.
        /// The original indentation is preserved.
        /// </summary>
        /// <param name="copy">a CopyDirective instance</param>
        /// <returns>a defered IEnumerable of ITextLine</returns>
        private static IEnumerable<ITextLine> CopyDirectiveToTextLines([NotNull] CopyDirective copy)
        {
            //We recreate the clause copy with its arguments and original formatting.
            StringBuilder textLine = new StringBuilder();
            bool firstLine = true;
            string copyIndent = copy.COPYToken.TokensLine.SourceText.GetIndent();
            foreach (var tokenLine in copy.ConsumedTokens.SelectedTokensOnSeveralLines)
            {
                if (firstLine)
                {
                    textLine.Append(copyIndent);
                    firstLine = false;
                }

                foreach (var token in tokenLine)
                {
                    textLine.Append(token.Text);
                }

                yield return new TextLineSnapshot(-1, textLine.ToString(), null);
                textLine.Clear();
            }
        }

        internal static List<ITextLine> CreateDataDefinition(Node node, SymbolTable table, ColumnsLayout? layout, List<string> rootProcedures, List< Tuple<string,string> > rootVariableName, TypeCobol.Compiler.Nodes.DataDefinition ownerDefinition, DataDefinitionEntry data_def, int level, int indent, bool isCustomType, bool isFirst, TypeDefinition customtype = null)
        {
            var data = data_def as DataDescriptionEntry;
            if (data != null)
            {
                bool bHasPeriod = false;
                var line = GetIndent(level, indent, isFirst);
                line.Append(level.ToString("00"));
                if (!isCustomType)
                {
                    bool bHasDependingOn = false;
                    bool bHasIndexes = false;
                    List<string> dependingOnAccessPath = null;
                    Dictionary<Compiler.Scanner.Token, string> indexesMap = null;

                    PreGenDependingOnAndIndexed(node, table, rootProcedures, rootVariableName, ownerDefinition, data_def, out bHasDependingOn, out bHasIndexes,
                        out dependingOnAccessPath, out indexesMap);

                    bool globalSeen = false;
                    string text = !(bHasDependingOn || bHasIndexes) ? ExtractPicTokensValues(layout, data, out bHasPeriod, out globalSeen) : "";
                    if (text.Length > 0)
                    {
                        AppendNameAndGlobalDataDef(line, data_def, globalSeen);
                        line.Append(text);
                    }
                    else if (!(bHasDependingOn || bHasIndexes) && data.Picture != null && !string.IsNullOrEmpty(data.Picture.ToString()))
                    {
                        AppendNameAndGlobalDataDef(line, data_def, globalSeen);
                        line.Append(" PIC ").Append(data.Picture);
                    }
                    else
                    {//Try to extract after a Level.
                        Func<Compiler.Scanner.Token, string> depenOnTokenFilter = null;
                        Func<Compiler.Scanner.Token, string> indexedByTokenFilter = null;

                        PostGenDependingOnAndIndexed(ownerDefinition, data_def, bHasDependingOn, bHasIndexes, dependingOnAccessPath, indexesMap,
                            out depenOnTokenFilter, out indexedByTokenFilter);
                            
                        text = ExtractTokensValuesAfterLevel(layout, data, out bHasPeriod,
                            bHasDependingOn ? depenOnTokenFilter : indexedByTokenFilter);
                        if (text.Length > 0)
                        {
                            line.Append(text);
                        }
                        else
                        {
                            AppendNameAndGlobalDataDef(line, data_def, globalSeen);
                        }
                    }
                }
                else if (customtype != null)
                {   //This variable will have no subtypes as children at all
                    //So Auto detect a type based on scalar COBOL typedef.            

                    bool bHasDependingOn = false;
                    bool bHasIndexes = false;
                    List<string> dependingOnAccessPath = null;
                    Dictionary<Compiler.Scanner.Token, string> indexesMap = null;

                    PreGenDependingOnAndIndexed(node, table, rootProcedures, rootVariableName, ownerDefinition, data_def, out bHasDependingOn, out bHasIndexes,
                        out dependingOnAccessPath, out indexesMap);

                    string text = !(bHasDependingOn || bHasIndexes) ? ExtractAnyCobolScalarTypeDef(layout, data, customtype, out bHasPeriod, false) : "";

                    if (text.Length != 0)
                    {
                        AppendNameAndGlobalDataDef(line, data_def, false);
                        line.Append(text);
                    }
                    else
                    {
                        Func<Compiler.Scanner.Token, string> depenOnTokenFilter = null;
                        Func<Compiler.Scanner.Token, string> indexedByTokenFilter = null;

                        PostGenDependingOnAndIndexed(ownerDefinition, data_def, bHasDependingOn, bHasIndexes, dependingOnAccessPath, indexesMap,
                            out depenOnTokenFilter, out indexedByTokenFilter);

                        bool globalSeen = false;
                        text = ExtractTokensValuesAfterTypeName(layout, data, out bHasPeriod, out globalSeen,
                            bHasDependingOn ? depenOnTokenFilter : indexedByTokenFilter);

                        AppendNameAndGlobalDataDef(line, data_def, globalSeen);
                        if (text.Length != 0)
                            line.Append(text);
                    }
                }
                else
                {
                    AppendNameAndGlobalDataDef(line, data_def, false);
                }
                if (!bHasPeriod)
                {
                    line.Append('.');
                }
                return LineToTextLines(line.ToString());
            }
            else if (data_def is DataConditionEntry || data_def is DataRenamesEntry || data_def is DataRedefinesEntry)
            {
                bool bHasPeriod = false;
                var line = GetIndent(level, indent, isFirst);
                string text = ConsumedTokenToString(layout, data_def, out bHasPeriod);
                line.Append(text);
                if (!bHasPeriod)
                {
                    line.Append('.');
                }
                return LineToTextLines(line.ToString());
            }
            else
            {//Humm ... It will be a Bug
                System.Diagnostics.Debug.Assert(data_def is DataDescriptionEntry || data_def is DataConditionEntry);
            }
            return null;
        }

        /// <summary>
        /// Compute the access path for a name in a DataDefinition.
        /// </summary>
        /// <param name="dataDef">The Owner DataDefition instance</param>
        /// <param name="name">The name to compute the access path this name must be lowered</param>        
        /// <param name="acc"> The access path accumulator </param>
        /// <returns> if a match has been found, false otherwise</returns>
        private static bool AccessPathForName(SymbolTable table, DataDefinition dataDef, string name, List<string> acc)
        {
            foreach (var child in dataDef.Children)
            {//First lookup in directly accessible
                DataDefinition data = child as DataDefinition;
                if (data != null)
                {
                    if (data.Name != null && data.Name.ToLower().Equals(name))
                    {
                        acc.Add(dataDef.Name);
                        acc.Add(data.Name);
                        return true;
                    }
                    var type = data.TypeDefinition;
                    bool isCustomTypeToo = !(data is TypeDefinition) && (type != null);
                    var dataDefinitionEntry = data.CodeElement;
                    if (isCustomTypeToo && dataDefinitionEntry != null)
                    {                        
                        List<string> sub_acc = new List<string>();
                        bool bFound = AccessPathForName(table, type, name, sub_acc);
                        if (bFound)
                        {   //Remove the type name
                            sub_acc.RemoveAt(0);
                            acc.Add(dataDef.Name);
                            acc.Add(data.Name);
                            acc.AddRange(sub_acc);
                            return true;
                        }
                    }
                }
            }
            //Now look for sub folders
            foreach (var child in dataDef.Children)
            {//First lookup in directly accessible
                if (child is DataDefinition)
                {
                    DataDefinition data = child as DataDefinition;
                    if (data.Children.Count > 0)
                    {
                        List<string> sub_acc = new List<string>();
                        sub_acc.Add(dataDef.Name);
                        if (AccessPathForName(table, data, name, sub_acc))
                        {
                            acc.AddRange(sub_acc);
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        /// <summary>
        /// Lookup up backward the access path of a name in the given DataDefinition entry.
        /// </summary>
        /// <param name="dataDef">The DataDefinition entry.</param>
        /// <param name="name">The name to look for</param>
        /// <param name="acc">The accumulator of the data access path</param>
        /// <returns>true if an access pathe exists, false otherwise</returns>
        private static bool LookupAccessPathForName(SymbolTable table, DataDefinition dataDef, string name, List<string> acc)
        {
            DataDefinition root = dataDef;
            do
            {
                if (AccessPathForName(table, root, name, acc))
                {//Add parrent access path
                    DataDefinition inner_root = root;
                    while (inner_root.Parent is DataDefinition)
                    {
                        DataDefinition parent = inner_root.Parent as DataDefinition;
                        acc.Insert(0, parent.Name);
                        inner_root = parent;
                    }
                    return true;
                }
                root = root.Parent is DataDefinition ? root.Parent as DataDefinition : null;
            } while (root != null);
            return false;
        }

        private static System.Text.StringBuilder GetIndent(int level, int indent, bool isFirst)
        {
            var str = new System.Text.StringBuilder();
            if (level == 1 || level == 77) return str;
            if (!isFirst)
                str.Append("    ");
            for (int i = 1; i < indent; i++) str.Append("  ");
            return str;
        }

        private static readonly string[] BoolTypeTemplate = {
            " {2}{1}  {0}-value PIC X VALUE {3}.",
            " {2}    88  {0}       VALUE 'T'.",
            " {2}    88  {0}-false VALUE 'F' ",
            "                      X'00' thru 'S'",
            "                      'U' thru X'FF'."
        };

        private static readonly string[] PointerUsageTemplate = {
            " {2}{1}  {0} Pointer.",
            " {2}{1}  redefines {0}.",
            " {2}    {3}  {4}{5} pic S9(05) comp-5."

        };

        public static List<ITextLine> InsertChildren(ColumnsLayout? layout, List<string> rootProcedures, List<Tuple<string, string>> rootVariableName, DataDefinition type, int level)
        {
            return InsertChildren(layout, rootProcedures, rootVariableName, type, level, 1, null);
        }

        private static List<ITextLine> InsertChildren(ColumnsLayout? layout, List<string> rootProcedures, List<Tuple<string,string>> rootVariableName, DataDefinition type, int level, int indent, CopyDirective usedCopy)
        {
            var lines = new List<ITextLine>();
            // List of all the CopyDirectives that have been added to the lines
            foreach (var child in type.Children)
            {
                bool bIsInsideCopy = child.IsInsideCopy();

                //Handle Typedef whose body is inside a COPY
                if (child.IsFlagSet(Flag.InsideTypedefFromCopy))
                {
                    if (child.IsFlagSet(Flag.IsTypedefCopyNode))
                    {
                        lines.AddRange(child.Lines);
                        continue;
                    }
                }

                //If the child is coming from a copy, we want to add the clause copy to the lines
                if (bIsInsideCopy && child.CodeElement != null)
                {
                    //There is already a mechanism to write the clause COPY coming from the main program in LinearNodeSourceCodeMapper. 
                    //This is how we recover the clause copy from the dependencies.
                    //Here we check if the typedef has been defined in another file than the one declaring the datatype.
                    if (!type.Root.Programs.Any(p => rootProcedures.Contains(p.Name)))
                    {
                        CopyDirective copy = child.CodeElement.FirstCopyDirective;
                        //The first data coming from a copy is used to recover the Clause COPY, the other would be only a repetition of this one, so we skip them.
                        //Even with the same name, two different Clause COPY are differentiated by their token lines
                        if (usedCopy != copy)
                        {
                            lines.AddRange(CopyDirectiveToTextLines(copy));
                            usedCopy = copy;
                            continue;
                        }
                    }
                }

                if (child is TypedDataNode) continue;
                //Special cases BOOL / POINTER
                if (child is TypeCobol.Compiler.Nodes.DataDescription)
                {
                    // For BOOL
                    string attr_type = (string)child["type"];
                    if (attr_type != null && attr_type.ToUpper().Equals("BOOL"))
                    {
                        if (!bIsInsideCopy)
                        {
                            string attr_name = (string) child["name"];
                            string margin = "";
                            for (int i = 0; i < indent; i++)
                                margin += "  ";
                            string slevel = level.ToString("00");
                            string svalue = child["value"] as string;
                            foreach (string str in BoolTypeTemplate)
                            {
                                string sline = string.Format(str, attr_name, slevel, margin,
                                    svalue?.Length == 0 ? "LOW-VALUE" : svalue);
                                TextLineSnapshot line = new TextLineSnapshot(-1, sline, null);
                                lines.Add(line);
                            }
                        }
                        continue;
                    }
                    else
                    {
                        // For POINTER
                        var attr_usage = child["usage"];
                        if (attr_usage != null && attr_usage.ToString().ToUpper().Equals("POINTER"))
                        {
                            if (!bIsInsideCopy)
                            {
                                string attr_name = (string) child["name"];
                                string margin = "";
                                for (int i = 0; i < indent; i++)
                                    margin += "  ";
                                string slevel = level.ToString("00");
                                string shash = (string) child["hash"];
                                foreach (string str in PointerUsageTemplate)
                                {
                                    string sline = string.Format(str,
                                        attr_name,
                                        slevel,
                                        margin,
                                        (level + 1).ToString("00"),
                                        attr_name.Length > 22 ? attr_name.Substring(0, 22) : attr_name,
                                        shash);
                                    TextLineSnapshot line = new TextLineSnapshot(-1, sline, null);
                                    lines.Add(line);
                                }
                            }
                            continue;
                        }
                    }
                }

                if (child is IndexDefinition)
                    continue;//Ignore Index Definition

                System.Diagnostics.Debug.Assert(child is DataDefinition);
                var typed = child is DataDefinition ? (DataDefinition)child : null;
                if (typed == null)
                {//Unexpected typed value.                    
                    continue;
                }

                bool isCustomTypeToo = !(child is TypeDefinition) && (typed.TypeDefinition != null);
                var dataDefinitionEntry = typed.CodeElement;
                if (dataDefinitionEntry != null)
                {
                    var texts = CreateDataDefinition(child, child.SymbolTable, layout, rootProcedures, rootVariableName, typed, dataDefinitionEntry, level, indent, isCustomTypeToo, false, isCustomTypeToo? typed.TypeDefinition: null);
                    if (!bIsInsideCopy)
                        lines.AddRange(texts);
                }
                else
                {//Humm ... It will be a bug.
                    System.Diagnostics.Debug.Assert(child.CodeElement is DataDefinitionEntry);
                }
                if (isCustomTypeToo)
                {
                    List< Tuple<string,string> > newRootVariableName = new List<Tuple<string, string>>();
                    newRootVariableName.Add(new Tuple<string, string>(typed.Name, typed.TypeDefinition.Name));
                    newRootVariableName.AddRange(rootVariableName);
                    var texts = InsertChildren(layout, rootProcedures, newRootVariableName, typed.TypeDefinition,
                        level + 1, indent + 1, usedCopy);
                    lines.AddRange(texts);
                }
                else
                {
                    var texts = InsertChildren(layout, rootProcedures, rootVariableName, typed, level + 1,
                        indent + 1, usedCopy);
                    lines.AddRange(texts);
                }
            }
            return lines;
        }

        public bool IsLeaf { get { return true; } }
    }
}
