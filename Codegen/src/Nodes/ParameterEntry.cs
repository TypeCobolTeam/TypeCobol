﻿using TypeCobol.Compiler;

namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.CodeModel;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.Text;


/// <summary>
/// TODO#245: this should NOT be necessary.
/// Instead, grammar should be refactored so INPUT/OUTPUT/INOUT/RETURNING _AND_ USING parameters
/// are created as CodeElements and put in procedure header OR function profile node only in semantic phase.
/// </summary>
internal class ParameterEntry: Node, CodeElementHolder<ParameterDescriptionEntry>, Generated {
	public ParameterDescription Description { get; private set; }
	public ParameterEntry(ParameterDescriptionEntry entry, SymbolTable table): base(entry) {
		this.SymbolTable = table;
	}

	private List<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				string name = this.CodeElement().Name;
				_cache = new List<ITextLine>();
				TypeDefinition customtype = null;
				if (this.CodeElement().DataType == DataType.Boolean) {
					_cache.Add(new TextLineSnapshot(-1, "01 "+name+"-value PIC X     VALUE LOW-VALUE.", null));
					_cache.Add(new TextLineSnapshot(-1, "    88 "+name+"       VALUE 'T'.", null));
					_cache.Add(new TextLineSnapshot(-1, "    88 "+name+"-false VALUE 'F' ", null));
                    _cache.Add(new TextLineSnapshot(-1, "                      X'00' thru 'S'", null));
                    _cache.Add(new TextLineSnapshot(-1, "                      'U' thru X'FF'.", null));
                } else {
                    bool bHasPeriod = false;
                    bool bIgnoreUsage = false;
                        var str = new System.Text.StringBuilder();
					str.Append("01 ").Append(name);
					AlphanumericValue picture = null;
                    //Type exists from Cobol 2002
				    string typedef = null;
					if (this.CodeElement().DataType.CobolLanguageLevel >= TypeCobol.Compiler.CobolLanguageLevel.Cobol2002) {
						var found = this.SymbolTable.GetType(this.CodeElement().DataType);
						if (found.Count > 0) {
							customtype = found[0];
						    typedef = TypedDataNode.ExtractAnyCobolScalarTypeDef(Layout, customtype, out bHasPeriod, true);
						    if (typedef.Length != 0)
						    {
						        str.Append(typedef);
						    }
						}
					} else picture = this.CodeElement().Picture;

                    if (picture != null)
                    {
                        //If we have a picture, try to extract the original pic string declaration.
                        string picIt = TypedDataNode.ExtractPicTokensValues(Layout, this.CodeElement().ConsumedTokens, out bHasPeriod);
                        if (picIt.Length != 0)
                        {
                            str.Append(picIt);
                            bIgnoreUsage = true;
                        }
                        else
                            str.Append(" PIC ").Append(picture);
                    }
				    if (!bIgnoreUsage)
				    {
				        if (this.CodeElement().Usage != null)
				        {
				            str.Append(" ").Append(this.CodeElement().Usage.Token.SourceText);
				        }
				    }

				    if (picture == null && this.CodeElement().Usage == null && this.CodeElement().DataType.CobolLanguageLevel == Compiler.CobolLanguageLevel.Cobol85)
                    {//JCM humm... Type without picture lookup enclosing scope.
                        var found = this.SymbolTable.GetType(this.CodeElement().DataType);
                        if (found.Count > 0)
                        {
                            customtype = found[0];
                            picture = customtype.CodeElement().Picture;
                            if (picture != null)
                            {
                                //If we have a picture, try to extract the original pic string declaration.
                                string picIt = TypedDataNode.ExtractPicTokensValues(Layout, customtype.CodeElement(), out bHasPeriod);
                                if (picIt.Length != 0)
                                    str.Append(picIt);
                                else
                                    str.Append(" PIC ").Append(picture);                                    
                            }
                        }
                    }
                    if (!bHasPeriod)
					    str.Append('.');
					_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));

					// TCRFUN_CODEGEN_PARAMETERS_IN_LINKAGE_SECTION
					foreach(var child in GetCodeElementHolderChildren<DataConditionEntry>()) {
						str.Clear();
						var entry = child.CodeElement();
						str.Append("    ").Append("88 ").Append(entry.Name);
						if (entry.ConditionValues != null && entry.ConditionValues.Length > 0) {
							str.Append(" VALUE");
							foreach(var value in entry.ConditionValues)
								str.Append(" \'").Append(value).Append('\'');
						} else
						if (entry.ConditionValuesRanges != null && entry.ConditionValuesRanges.Length > 0) {
							str.Append(" VALUES");
							foreach(var range in entry.ConditionValuesRanges)
								str.Append(" \'").Append(range.MinValue).Append("\' THRU \'").Append(range.MaxValue).Append('\'');
						}
                        str.Append('.');
						_cache.Add(new TextLineSnapshot(-1, str.ToString(), null));
					}
				}
                if (customtype != null)
                {
                    List<string> rootProcedures;
                    List<System.Tuple<string, string>> rootVars;
                    GeneratorHelper.ComputeTypedProperPaths(this, this.CodeElement(), customtype, out rootProcedures, out rootVars);
                    _cache.AddRange(TypedDataNode.InsertChildren(Layout, rootProcedures, rootVars, customtype, customtype, 2, 1));
                }
			}
			return _cache;
		}
	}
	public bool IsLeaf { get { return true; } }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            //Generated Node doesn't need to be visited
            return false;
        }
    }

}
