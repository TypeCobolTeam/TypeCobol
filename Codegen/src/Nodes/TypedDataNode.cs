namespace TypeCobol.Codegen.Nodes {

    using System.Collections.Generic;
    using System.Text;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.CodeModel;
    using TypeCobol.Compiler.Nodes;
    using TypeCobol.Compiler.Text;



internal class TypedDataNode: DataDescription, Generated {

	private DataDescription Node;
	public TypedDataNode(DataDescription node): base(null) { this.Node = node; }

	public override CodeElement CodeElement { get { return this.Node.CodeElement; } }

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				if (this.Node.IsPartOfATypeDef) return _cache;

				var data = this.Node.CodeElement();
				int level = (int)data.LevelNumber.Value;				
				var customtype = this.Node.SymbolTable.GetType(data.DataType);
                _cache.AddRange(CreateDataDefinition(Layout, data, level, 0, true, true, customtype[0]));
				if (customtype.Count > 0) _cache.AddRange(InsertChildren(Layout, this.Node.SymbolTable, customtype[0], level+1, 1));
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
        internal static void FlushConsumedTokens(ColumnsLayout? layout, int i, IList<Compiler.Scanner.Token> consumedTokens, StringBuilder sb, out bool bHasPeriod)
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
                    if (baseToken.Line == consumedTokens[i].Line)
                        sb.Append(string.Intern(" "));//Add a space but not before a Period Separator
                if (baseToken.Line != consumedTokens[i].Line)
                {
                    int nPad = consumedTokens[i].Column;
                    if (layout.HasValue)
                    {
                        if (layout.Value == ColumnsLayout.CobolReferenceFormat)
                        {
                            nPad = System.Math.Max(0, consumedTokens[i].Column - 8);
                        }
                    }
                    string pad = new string(' ', nPad);                    
                    sb.Append('\n');
                    sb.Append(pad);
                    baseToken = consumedTokens[i];
                }
                sb.Append(consumedTokens[i].Text);
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
        internal static string ConsumedTokenToString(ColumnsLayout ? layout, DataDefinitionEntry data_def, out bool bHasPeriod)
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
    /// Tries to detect a TYPEDEF construction for a scalar type.
    /// </summary>
    /// <param name="customtype">The TypeDef definition node</param>
    /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
    /// <returns>The string representing the TYPEDEF type</returns>
    internal static string ExtractAnyCobolScalarTypeDef(ColumnsLayout ? layout, TypeDefinition customtype, out bool bHasPeriod)
    {
        bHasPeriod = false;
        StringBuilder sb = new StringBuilder();
        if (customtype.CodeElement != null)
        {
            if (customtype.CodeElement.ConsumedTokens != null)
            {
                int i = 0;
                //Ignore TYPEDEF Keyword
                while (i < customtype.CodeElement.ConsumedTokens.Count  && customtype.CodeElement.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.TYPEDEF)
                    i++;

                //Ignore any STRONG or STRICT keywords
                if((i+1) < customtype.CodeElement.ConsumedTokens.Count && (customtype.CodeElement.ConsumedTokens[i+1].TokenType == Compiler.Scanner.TokenType.STRONG || customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.STRICT))
                    i++;

                //Ignore any PUBLIC or PRIVATE keywords
                if((i+1) < customtype.CodeElement.ConsumedTokens.Count && (customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.PUBLIC || customtype.CodeElement.ConsumedTokens[i + 1].TokenType == Compiler.Scanner.TokenType.PRIVATE))
                    i++;

                ++i;
                //If we reached the last Tokens and if it's a PeriodSeparator then don't return a text
                if (i < customtype.CodeElement.ConsumedTokens.Count && customtype.CodeElement.ConsumedTokens[i].TokenType == Compiler.Scanner.TokenType.PeriodSeparator) {
                    bHasPeriod = true;
                    return "";
                }
                FlushConsumedTokens(layout, i-1, customtype.CodeElement.ConsumedTokens, sb, out bHasPeriod);
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
    internal static string ExtractTokensValuesAfterTypeName(ColumnsLayout ? layout, DataDescriptionEntry dataDescEntry, out bool bHasPeriod)
    {
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
                   dataDescEntry.ConsumedTokens[i].TokenType == Compiler.Scanner.TokenType.QualifiedNameSeparator) {
                i += 2; //skip  :: and the next type name
            }
            
            FlushConsumedTokens(layout, i - 1, dataDescEntry.ConsumedTokens, sb, out bHasPeriod);
        }
        return sb.ToString();
    }

    /// <summary>
    /// Tries to detect a (PIC|PICTURE) construction for a Data Description Entry.
    /// </summary>
    /// <param name="dataDescEntry">The Data Description Entry Node</param>
    /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
    /// <returns>The string representing the PIC clause </returns>
    internal static string ExtractPicTokensValues(ColumnsLayout? layout, DataDescriptionEntry dataDescEntry, out bool bHasPeriod)
    {
        bHasPeriod = false;
        StringBuilder sb = new StringBuilder();
        if (dataDescEntry.ConsumedTokens != null)
        {
            int i = 0;
            while (i < dataDescEntry.ConsumedTokens.Count && dataDescEntry.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.PIC && dataDescEntry.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.PICTURE)
                i++;
            if (i < dataDescEntry.ConsumedTokens.Count)
            {
                sb.Append(string.Intern(" "));
                sb.Append(dataDescEntry.ConsumedTokens[i].Text);
            }
            FlushConsumedTokens(layout, i, dataDescEntry.ConsumedTokens, sb, out bHasPeriod);
        }
        return sb.ToString();
    }

    /// <summary>
    /// Extract All Tokens after encountering a LevelNumber token
    /// </summary>
    /// <param name="dataDescEntry">The Data Description Entry Node</param>
    /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
    /// <returns>The string representing all tokens after a LevelNumber </returns>
    internal static string ExtractTokensValuesAfterLevel(ColumnsLayout ? layout, DataDescriptionEntry dataDescEntry, out bool bHasPeriod)
    {
        bHasPeriod = false;
        StringBuilder sb = new StringBuilder();
        if (dataDescEntry.ConsumedTokens != null)
        {
            int i = 0;
            while (i < dataDescEntry.ConsumedTokens.Count && dataDescEntry.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.LevelNumber)
                i++;
            FlushConsumedTokens(layout, i, dataDescEntry.ConsumedTokens, sb, out bHasPeriod);
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
            foreach(var item in items)
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

    internal static List<ITextLine> CreateDataDefinition(ColumnsLayout ? layout, DataDefinitionEntry data_def, int level, int indent, bool isCustomType, bool isFirst, TypeDefinition customtype = null) {        
        var data = data_def as DataDescriptionEntry;
	    if (data != null)
        {
            bool bHasPeriod = false;
            var line = GetIndent(level, indent, isFirst);
		    line.Append(level.ToString("00"));
            if (!isCustomType)
            {
                string text = ExtractPicTokensValues(layout, data, out bHasPeriod);
                if (text.Length > 0) {
                    if (data_def.Name != null)
                        line.Append(' ').Append(data.Name);
                    line.Append(text);
                }
                else if (data.Picture != null && !string.IsNullOrEmpty(data.Picture.ToString()))
                {
                    if (data_def.Name != null)
                        line.Append(' ').Append(data.Name);
                    line.Append(" PIC ").Append(data.Picture);
                }
                else
                {//Try to extract after a Level.
                    text = ExtractTokensValuesAfterLevel(layout, data, out bHasPeriod);
                    if (text.Length > 0)
                    {
                        line.Append(text);
                    }
                    else
                    {
                        if (data_def.Name != null)
                            line.Append(' ').Append(data.Name);
                    }
                }
            }
            else if (customtype != null)
            {   //This variable will have no subtypes as children at all
                //So Auto detect a type based on scalar COBOL typedef.            
                if (data_def.Name != null)
                    line.Append(' ').Append(data.Name);
                string text = ExtractAnyCobolScalarTypeDef(layout, customtype, out bHasPeriod);
                if (text.Length != 0)
                {
                    line.Append(text);
                }
                else
                {
                    text = ExtractTokensValuesAfterTypeName(layout, data, out bHasPeriod);
                    if (text.Length != 0)
                        line.Append(text);
                }
            }
            else
            {
                if (data_def.Name != null)
                    line.Append(' ').Append(data.Name);
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

    private static System.Text.StringBuilder GetIndent(int level, int indent, bool isFirst)
    {
		var str = new System.Text.StringBuilder();
        if (level == 1 || level == 77) return str;
        if (!isFirst)
		    str.Append("    ");
		for(int i=1; i<indent; i++) str.Append("  ");
		return str;
	}

    private readonly static string[] BoolTypeTemplate = {
        " {2}{1}  {0}-value PIC X VALUE LOW-VALUE.",
        " {2}    88  {0}       VALUE 'T'.",
        " {2}    88  {0}-false VALUE 'F'.",
    };
	public static List<ITextLine> InsertChildren(ColumnsLayout ? layout, SymbolTable table, DataDefinition type, int level, int indent) {
		var lines = new List<ITextLine>();
		foreach(var child in type.Children) {
			if (child is TypedDataNode) continue;
            //Special case type BOOL
            if (child is TypeCobol.Compiler.Nodes.DataDescription)
            {
                string attr_type = (string)child["type"];
                if (attr_type != null)
                {
                    if (attr_type.ToUpper().Equals("BOOL"))
                    {
                        string attr_name = (string)child["name"];
                        string margin = "";
                        for (int i = 0; i < indent; i++)
                            margin += "  ";
                        string slevel = level.ToString("00");
                        foreach (string str in BoolTypeTemplate)
                        {
                            string sline = string.Format(str, attr_name, slevel, margin);
                            TextLineSnapshot line = new TextLineSnapshot(-1, sline, null);
                            lines.Add(line);
                        }
                        continue;
                    }
                }
            }

			var typed = (DataDefinition) child;
			var types = table.GetType(typed.DataType);
			bool isCustomTypeToo = !(child is TypeDefinition) && (types.Count > 0);
		    var dataDefinitionEntry = typed.CodeElement as DataDefinitionEntry;
		    if (dataDefinitionEntry != null)
            {
                lines.AddRange(CreateDataDefinition(layout, dataDefinitionEntry, level, indent, isCustomTypeToo, false, isCustomTypeToo ? types[0] : null));
            }
            else
            {//Humm ... It will be a bug.
                System.Diagnostics.Debug.Assert(child.CodeElement is DataDefinitionEntry);
            }
            if (isCustomTypeToo)
                lines.AddRange(InsertChildren(layout, table, types[0], level + 1, indent + 1));
            else
                lines.AddRange(InsertChildren(layout, table, typed, level + 1, indent + 1));
		}
		return lines;
	}

	public bool IsLeaf { get { return true; } }
}

}
