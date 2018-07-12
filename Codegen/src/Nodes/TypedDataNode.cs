using System.Linq;
using TypeCobol.Compiler.Scanner;

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

        private DataDescription Node;
        public TypedDataNode(DataDescription node) : base(null) { this.Node = node; }

        public override CodeElement CodeElement { get { return this.Node.CodeElement; } }

        private List<ITextLine> _cache = null;
        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                if (_cache == null)
                {
                    _cache = new List<ITextLine>();
                    if (this.Node.IsPartOfATypeDef) return _cache;

                    var data = this.Node.CodeElement();
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
                        _cache.AddRange(InsertChildren(Layout, rootProcedures, rootVars, customtype, customtype, level + 1, 1));
                    }
                }
                return _cache;
            }
        }

        /// <summary>
        ///  Flush Consumed tokens into a buffer
        /// 
        /// </summary>
        /// <param name="i">The start index in the list of consumed tokens</param>
        /// <param name="consumedTokens">The consumed tokens list</param>
        /// <param name="sb">The String buffer to flush into</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <param name="tokenFilter">A Token filter that can be used to produce another text for a Token</param>
        internal static void FlushConsumedTokens(ColumnsLayout? layout, int i, IList<Compiler.Scanner.Token> consumedTokens, StringBuilder sb, out bool bHasPeriod, Func<Compiler.Scanner.Token, string> tokenFilter = null)
        {
            System.Diagnostics.Contracts.Contract.Assert(i >= -1);
            Compiler.Scanner.Token baseToken = null;
            if (i < consumedTokens.Count && consumedTokens.Count > 0)
            {
                baseToken = i == -1 ? consumedTokens[++i] : consumedTokens[i++];
            }
            else
            {
                i += 1;
            }
            bHasPeriod = false;
            while (i < consumedTokens.Count)
            {
                if ((i != consumedTokens.Count - 1) || (consumedTokens[i].TokenType != Compiler.Scanner.TokenType.PeriodSeparator))
                    if (baseToken?.Line == consumedTokens[i].Line)
                        sb.Append(string.Intern(" "));//Add a space but not before a Period Separator
                if (baseToken?.Line != consumedTokens[i].Line)
                {
                    int nPad = consumedTokens[i].Column;
                    if (layout.HasValue)
                    {
                        if (layout.Value == ColumnsLayout.CobolReferenceFormat)
                        {
                            nPad = Math.Max(0, consumedTokens[i].Column - 8);
                        }
                    }
                    string pad = new string(' ', nPad);
                    sb.Append('\n');
                    sb.Append(pad);
                    baseToken = consumedTokens[i];
                }
                sb.Append(tokenFilter != null ? tokenFilter(consumedTokens[i]) : consumedTokens[i].Text);
                if (i == consumedTokens.Count - 1)
                    bHasPeriod = consumedTokens[i].TokenType == Compiler.Scanner.TokenType.PeriodSeparator;
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
            bHasPeriod = false;
            StringBuilder sb = new StringBuilder();
            if (data_def.ConsumedTokens != null)
            {
                FlushConsumedTokens(layout, -1, data_def.ConsumedTokens, sb, out bHasPeriod);
            }
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
        /// Determines if the given list of tokens, contains any token of the specified type.
        /// </summary>
        /// <param name="tokens">The List of tokens.</param>
        /// <param name="startIndex">The starting index in the token list</param>
        /// <param name="type">The specified type.</param>
        /// <returns>True if yes, false otherwise.</returns>
        internal static bool ContainsToken(IList<Token> tokens, int startIndex, TokenType type)
        {
            if (tokens != null)
            {
                for (int i = startIndex; i < tokens.Count; i++)
                    if (tokens[i].TokenType == type)
                        return true;
            }
            return false;
        }

        /// <summary>
        /// Determines if the given list of tokens, contains any token of the specified type.
        /// </summary>
        /// <param name="tokens">The List of tokens.</param>
        /// <param name="type">The specified type.</param>
        /// <returns>True if yes, false otherwise.</returns>
        internal static bool ContainsToken(IList<Token> tokens, TokenType type)
        {
            return ContainsToken(tokens, 0, type);
        }

        /// <summary>
        /// Tries to detect a TYPEDEF construction for a scalar type.
        /// </summary>
        /// <param name="customtype">The TypeDef definition node</param>
        /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
        /// <param name="bIgnoreGlobal">true if the GLOBAL keyword must be ignored</param>
        /// <returns>The string representing the TYPEDEF type</returns>
        internal static string ExtractAnyCobolScalarTypeDef(ColumnsLayout? layout, TypeDefinition customtype, out bool bHasPeriod, bool bIgnoreGlobal)
        {
            bHasPeriod = false;
            StringBuilder sb = new StringBuilder();
            if (customtype.CodeElement != null)
            {
                if (customtype.CodeElement.ConsumedTokens != null)
                {
                    int i = 0;
                    //Ignore TYPEDEF Keyword
                    while (i < customtype.CodeElement.ConsumedTokens.Count && customtype.CodeElement.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.TYPEDEF)
                        i++;

                    //Ignore any STRONG or STRICT keywords
                    if ((i + 1) < customtype.CodeElement.ConsumedTokens.Count && (customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.STRONG || customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.STRICT))
                        i++;

                    //Ignore any PUBLIC or PRIVATE keywords
                    if ((i + 1) < customtype.CodeElement.ConsumedTokens.Count && (customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.PUBLIC || customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.PRIVATE))
                        i++;

                    //Ignore GLOBAL Keyword
                    if (bIgnoreGlobal)
                    {
                        if ((i + 1) < customtype.CodeElement.ConsumedTokens.Count &&
                            customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.GLOBAL)
                            i++;
                    }

                    ++i;
                    //If we reached the last Tokens and if it's a PeriodSeparator then don't return a text
                    if (i < customtype.CodeElement.ConsumedTokens.Count && customtype.CodeElement.ConsumedTokens[i].TokenType == Compiler.Scanner.TokenType.PeriodSeparator)
                    {
                        bHasPeriod = false;
                        return "";
                    }
                    FlushConsumedTokens(layout, i - 1, customtype.CodeElement.ConsumedTokens, sb, out bHasPeriod);
                }
            }
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
            globalSeen = false;
            bHasPeriod = false;
            StringBuilder sb = new StringBuilder();
            if (dataDescEntry.ConsumedTokens != null)
            {
                int i = 0;
                while (i < dataDescEntry.ConsumedTokens.Count && dataDescEntry.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.TYPE)
                    i++;
                i++;//Ignore the Name of the Type.

                ++i;

                //Ignore qualified type name
                while (i < dataDescEntry.ConsumedTokens.Count &&
                       dataDescEntry.ConsumedTokens[i].TokenType == Compiler.Scanner.TokenType.QualifiedNameSeparator)
                {                    
                    i += 2; //skip  :: and the next type name
                }
                globalSeen = ContainsToken(dataDescEntry.ConsumedTokens, i - 1, TokenType.GLOBAL);
                FlushConsumedTokens(layout, i - 1, dataDescEntry.ConsumedTokens, sb, out bHasPeriod, tokenFilter);
            }
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
            globalSeen = false;
            bHasPeriod = false;
            StringBuilder sb = new StringBuilder();
            if (consumedTokens != null)
            {
                int i = 0;
                while (i < consumedTokens.Count && consumedTokens[i].TokenType != Compiler.Scanner.TokenType.PIC && consumedTokens[i].TokenType != Compiler.Scanner.TokenType.PICTURE)
                    i++;
                if (i < consumedTokens.Count)
                {
                    if (consumedTokens[i].TokenType == TokenType.GLOBAL)
                    {
                        globalSeen = true;
                    }
                    sb.Append(string.Intern(" "));
                    sb.Append(consumedTokens[i].Text);
                }
                FlushConsumedTokens(layout, i, consumedTokens, sb, out bHasPeriod);
            }
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
            bHasPeriod = false;
            StringBuilder sb = new StringBuilder();
            if (dataDescEntry.ConsumedTokens != null)
            {
                int i = 0;
                while (i < dataDescEntry.ConsumedTokens.Count && dataDescEntry.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.LevelNumber)
                    i++;
                FlushConsumedTokens(layout, i, dataDescEntry.ConsumedTokens, sb, out bHasPeriod, tokenFilter);
            }
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
        private static Dictionary<Compiler.Scanner.Token, string> BuiltIndexMap(Node rootNode, List<string> rootProcedures, List<Tuple<string, string>> rootVariableName, SymbolDefinition[] indexes, TypeCobol.Compiler.Nodes.DataDefinition ownerDefinition)
        {
            Dictionary<Compiler.Scanner.Token, string> map = new Dictionary<Compiler.Scanner.Token, string>(indexes.Length);
            List<string> pathProcedures;
            List<string> pathVariables;
            GeneratorHelper.ComputeProperPaths(ownerDefinition, out pathProcedures, out pathVariables);
            List<string> list_items = new List<string>();
            //list_items.AddRange(pathProcedures);

            //Add root procedures

            for (int j = rootProcedures.Count - 1; j >= 0; j--)
            {
                list_items.Add(rootProcedures[j]);
            }

            //Add Root variables
            for (int j = rootVariableName.Count - 1; j >= 0; j--)
            {
                list_items.Add(rootVariableName[j].Item1);
                if (j != 0 && rootVariableName[j].Item2 != null && rootVariableName[j].Item2.Trim().Length > 0)
                    list_items.Add(rootVariableName[j].Item2);
            }

            list_items.AddRange(pathVariables);
            string qn = string.Join(".", list_items.ToArray());
            AddIndexMap(rootNode, rootNode.QualifiedName.ToString(), indexes, map);
            AddIndexMap(ownerDefinition, qn, indexes, map);
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
                            string hash_name = GeneratorHelper.ComputeIndexHashName(qualified_name, parentNode);
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
                if (!data.OccursDependingOn.MainSymbolReference.IsQualified)
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
                indexesMap = BuiltIndexMap(rootNode, rootProcedures, rootVariableName, data.Indexes, ownerDefinition);
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
                if (data != null && !data.OccursDependingOn.MainSymbolReference.IsQualified)
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
                        if (qualSymRef != null && qualSymRef.IsTypeCobolQualified)
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
        /// Append in the given StringBuilder the name and any global attribute of the the given DataDefition object.
        /// </summary>
        /// <param name="buffer">The String Buffer</param>
        /// <param name="dataDef">The Data Definition object</param>
        /// <param name="globalSeen">Global token hass been already seen</param>
        internal static void AppendNameAndGlobalDataDef(StringBuilder buffer, DataDefinitionEntry dataDef, bool globalSeen)
        {
            if (dataDef.Name != null)
            {
                buffer.Append(' ').Append(dataDef.Name);
                if (!globalSeen)
                {
                    if (dataDef is CommonDataDescriptionAndDataRedefines)
                    {
                        CommonDataDescriptionAndDataRedefines cdadr = dataDef as CommonDataDescriptionAndDataRedefines;
                        if (cdadr.IsGlobal)
                        {
                            Token gtoken = GetToken(dataDef.ConsumedTokens, TokenType.GLOBAL);
                            buffer.Append(' ').Append(gtoken.Text);
                        }
                    }
                }
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

                    string text = !(bHasDependingOn || bHasIndexes) ? ExtractAnyCobolScalarTypeDef(layout, customtype, out bHasPeriod, false) : "";

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
                    var dataDefinitionEntry = data.CodeElement as DataDefinitionEntry;
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
        public static List<ITextLine> InsertChildren(ColumnsLayout? layout, List<string> rootProcedures, List< Tuple<string,string> > rootVariableName, DataDefinition ownerDefinition, DataDefinition type, int level, int indent)
        {
            var lines = new List<ITextLine>();
            foreach (var child in type.Children)
            {
                if (child is TypedDataNode) continue;
                //Special cases BOOL / POINTER
                if (child is TypeCobol.Compiler.Nodes.DataDescription)
                {
                    // For BOOL
                    string attr_type = (string)child["type"];
                    if (attr_type != null && attr_type.ToUpper().Equals("BOOL"))
                    {
                        string attr_name = (string)child["name"];
                        string margin = "";
                        for (int i = 0; i < indent; i++)
                            margin += "  ";
                        string slevel = level.ToString("00");
                        string svalue = child["value"] as string;
                        foreach (string str in BoolTypeTemplate)
                        {
                            string sline = string.Format(str, attr_name, slevel, margin, svalue?.Length == 0 ? "LOW-VALUE" : svalue);
                            TextLineSnapshot line = new TextLineSnapshot(-1, sline, null);
                            lines.Add(line);
                        }
                        continue;
                    }
                    else
                    {
                        // For POINTER
                        var attr_usage = child["usage"];
                        if (attr_usage != null && attr_usage.ToString().ToUpper().Equals("POINTER"))
                        {
                            string attr_name = (string)child["name"];
                            string margin = "";
                            for (int i = 0; i < indent; i++)
                                margin += "  ";
                            string slevel = level.ToString("00");
                            string shash = (string)child["hash"];
                            foreach (string str in PointerUsageTemplate)
                            {
                                string sline = string.Format(str,
                                                             attr_name, 
                                                             slevel,
                                                             margin,
                                                             (level+1).ToString("00"),
                                                             attr_name.Length > 22 ? attr_name.Substring(0, 22) : attr_name,
                                                             shash);
                                TextLineSnapshot line = new TextLineSnapshot(-1, sline, null);
                                lines.Add(line);
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
                var dataDefinitionEntry = typed.CodeElement as DataDefinitionEntry;
                if (dataDefinitionEntry != null)
                {
                    lines.AddRange(CreateDataDefinition(child, child.SymbolTable, layout, rootProcedures, rootVariableName, typed, dataDefinitionEntry, level, indent, isCustomTypeToo, false, isCustomTypeToo ? typed.TypeDefinition : null));
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
                    lines.AddRange(InsertChildren(layout, rootProcedures, newRootVariableName, typed, typed.TypeDefinition, level + 1, indent + 1));
                }
                else
                    lines.AddRange(InsertChildren(layout, rootProcedures, rootVariableName, typed, typed, level + 1, indent + 1));

            }
            return lines;
        }

        public bool IsLeaf { get { return true; } }
    }

}
