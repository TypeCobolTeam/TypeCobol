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
				var customtype = this.Node.SymbolTable.GetType(new URI(data.DataType.Name));
                _cache.Add(CreateDataDefinition(data, level, 0, true, true, (TypeDefinition)customtype[0]));
				if (customtype.Count > 0) _cache.AddRange(InsertChildren(this.Node.SymbolTable, (TypeDefinition)customtype[0], level+1, 1));
			}
			return _cache;
		}
	}

    /// <summary>
    /// Tries to detect a TYPEDEF construction for a scalar type.
    /// </summary>
    /// <param name="customtype">The TypeDef definition node</param>
    /// <param name="bHasPeriod">out true if a period separator has been encountered, false otherwise.</param>
    /// <returns>The string representing the TYPEDEF type</returns>
    internal static string ExtractAnyCobolScalarTypeDef(TypeDefinition customtype, out bool bHasPeriod)
    {
        bHasPeriod = false;
        StringBuilder sb = new StringBuilder();
        if (customtype.CodeElement != null)
        {
            if (customtype.CodeElement.ConsumedTokens != null)
            {
                int i = 0;
                while (i < customtype.CodeElement.ConsumedTokens.Count  && customtype.CodeElement.ConsumedTokens[i].TokenType != Compiler.Scanner.TokenType.TYPEDEF)
                    i++;

                while (++i < customtype.CodeElement.ConsumedTokens.Count)
                {
                    sb.Append(string.Intern(" "));
                    sb.Append(customtype.CodeElement.ConsumedTokens[i].Text);
                    if (i == customtype.CodeElement.ConsumedTokens.Count - 1)
                        bHasPeriod = customtype.CodeElement.ConsumedTokens[i].TokenType == Compiler.Scanner.TokenType.PeriodSeparator;
                }
            }
        }
        return sb.ToString();
    }

	internal static ITextLine CreateDataDefinition(DataDescriptionEntry data, int level, int indent, bool isCustomType, bool isFirst, TypeDefinition customtype = null) {
        bool bHasPeriod = false;
        var line = GetIndent(level, indent, isFirst);
		line.Append(level.ToString("00"));
		if (data.Name != null) 
            line.Append(' ').Append(data.Name);
		if (!isCustomType) 
            line.Append(" PIC ").Append(data.Picture);
        else if (customtype != null && customtype.Children.Count == 0)
        {   //This variable will have no subtypes as children at all
            //So Auto detect a type based on scalar COBOL typedef.            
            string text = ExtractAnyCobolScalarTypeDef(customtype, out bHasPeriod);
            line.Append(text);
        }
        if (!bHasPeriod)
        {
            line.Append('.');
        }
		return new TextLineSnapshot(-1, line.ToString(), null);
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

	public static List<ITextLine> InsertChildren(SymbolTable table, DataDefinition type, int level, int indent) {
		var lines = new List<ITextLine>();
		foreach(var child in type.Children) {
			if (child is TypedDataNode) continue;
			var typed = (ITypedNode)child;
			var types = table.GetType(new URI(typed.DataType.Name));
			bool isCustomTypeToo = !(child is TypeDefinition) && (types.Count > 0);
            lines.Add(CreateDataDefinition((DataDescriptionEntry)child.CodeElement, level, indent, isCustomTypeToo, false, isCustomTypeToo ? (TypeDefinition)types[0] : null));
			if (isCustomTypeToo) lines.AddRange(InsertChildren(table, (TypeDefinition)types[0], level+1, indent+1));
		}
		return lines;
	}

	public bool IsLeaf { get { return true; } }
}

}
