namespace TypeCobol.Codegen.Nodes {
	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Text;

internal class ProcedureStyleCall: Compiler.Nodes.Call, Generated {
	private Compiler.Nodes.ProcedureStyleCall Node;
	private CallStatement call;
    //The Original Staement
    private ProcedureStyleCallStatement Statement;
    //Does this call has a CALL-END ?
    private bool HasCallEnd;

    /// <summary>
    /// Arguments Mode.
    /// </summary>
    public enum ArgMode
    {
        Input,
        InOut,
        Output
    };

	public ProcedureStyleCall(Compiler.Nodes.ProcedureStyleCall node)
		: base(null) {
		this.Node = node;
		Statement = (ProcedureStyleCallStatement)Node.CodeElement;       
		call = new CallStatement();
        call.ProgramOrProgramEntryOrProcedureOrFunction = new SymbolReferenceVariable(StorageDataType.ProgramName, Statement.ProcedureCall.ProcedureName);
        call.InputParameters = new List<CallSiteParameter>(Statement.ProcedureCall.Arguments);
		call.OutputParameter = null;
        //Add any optional CALL-END statement
        foreach (var child in node.Children)
        {
            this.Add(child);            
            if (child.CodeElement != null && child.CodeElement.Type == TypeCobol.Compiler.CodeElements.CodeElementType.CallStatementEnd)
            {
                HasCallEnd = true;
            }
        }
	}

	public override CodeElement CodeElement { get { return this.Node.CodeElement; } }

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				_cache = new List<ITextLine>();
				var hash = Node.FunctionDeclaration.Hash;
			    var callString = $"CALL '{hash}' {(Node.FunctionCall.Arguments.Length == 0 ? null : "USING")}";
				var callTextLine = new TextLineSnapshot(-1, callString, null);
				_cache.Add(callTextLine);
				var indent = new string(' ', callTextLine.Length + 1);
                TypeCobol.Compiler.CodeElements.ParameterSharingMode previousSharingMode = (TypeCobol.Compiler.CodeElements.ParameterSharingMode)(-1);
                int previousSpan = 0;
                if (Statement.ProcedureCall.InputParameters != null)
                    foreach (var parameter in Statement.ProcedureCall.InputParameters)
                    {
                        var name = ToString(parameter, Node.SymbolTable, ArgMode.Input, ref previousSharingMode, ref previousSpan);
                        _cache.Add(new TextLineSnapshot(-1, indent + name, null));
                    }

                previousSharingMode = (TypeCobol.Compiler.CodeElements.ParameterSharingMode)(-1);
                previousSpan = 0;
                if (Statement.ProcedureCall.InoutParameters != null)
                    foreach (var parameter in Statement.ProcedureCall.InoutParameters)
                    {
                        var name = ToString(parameter, Node.SymbolTable, ArgMode.InOut, ref previousSharingMode, ref previousSpan);
                        _cache.Add(new TextLineSnapshot(-1, indent + name, null));
                    }

                previousSharingMode = (TypeCobol.Compiler.CodeElements.ParameterSharingMode)(-1);
                previousSpan = 0;
                if (Statement.ProcedureCall.OutputParameters != null)
                    foreach (var parameter in Statement.ProcedureCall.OutputParameters)
                    {
                        var name = ToString(parameter, Node.SymbolTable, ArgMode.Output, ref previousSharingMode, ref previousSpan);
                        _cache.Add(new TextLineSnapshot(-1, indent + name, null));
                    }

                if (!HasCallEnd)
                {
                    var call_end = new TextLineSnapshot(-1, "    end-call ", null);
                    _cache.Add(call_end);
                }
			}
			return _cache;
		}
	}

	private string ToString(TypeCobol.Compiler.CodeElements.CallSiteParameter parameter, Compiler.CodeModel.SymbolTable table, ArgMode mode,
        ref TypeCobol.Compiler.CodeElements.ParameterSharingMode previousSharingMode, ref int previousSpan) {
        Variable variable = parameter.StorageAreaOrValue;
		var name = variable.ToString();
        string share_mode = "";
        int defaultSpan = string.Intern("by reference ").Length;
        if (parameter.SharingMode.Token != null)
        {
            if (previousSharingMode != parameter.SharingMode.Value)
            {
                share_mode = "by " + parameter.SharingMode.Token.Text;
                share_mode += new string(' ', defaultSpan - share_mode.Length);
                previousSharingMode = parameter.SharingMode.Value;
            }
        }
        else
        {
            if (mode == ArgMode.InOut || mode == ArgMode.Output)
            {
                if (previousSharingMode != TypeCobol.Compiler.CodeElements.ParameterSharingMode.ByReference)
                {
                    share_mode = string.Intern("by reference ");
                    previousSharingMode = TypeCobol.Compiler.CodeElements.ParameterSharingMode.ByReference;
                }
            }
        }
        if (share_mode.Length == 0)
        {
            share_mode = new string(' ', previousSpan == 0 ? defaultSpan : previousSpan);
        }
        else
        {
            previousSpan = share_mode.Length;
        }

		if (variable.IsLiteral)
            return share_mode + name;
		var found = table.GetVariable(variable);
		if (found.Count < 1) return "?NOT_FOUND?";
//		if (found.Count > 1) return "?AMBIGUOUS?";
		var data = found[0] as Compiler.Nodes.DataDescription;
		if (data.DataType == DataType.Boolean) name += "-value";
        return share_mode + name;
	}

	public bool IsLeaf { get { return true; } }
}

}
