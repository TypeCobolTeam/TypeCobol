﻿using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class FunctionDeclaration: Compiler.Nodes.FunctionDeclaration, Generated {

		string ProgramName = null;

		public FunctionDeclaration(Compiler.Nodes.FunctionDeclaration node): base(node.CodeElement()) {
			ProgramName = node.Label;
			foreach(var child in node.Children) {
				if (child is Compiler.Nodes.ProcedureDivision) {
					CreateOrUpdateLinkageSection(node, node.CodeElement().Profile);
					var sentences = new List<Node>();
					foreach(var sentence in child.Children)
						sentences.Add(sentence);
					var pdiv = new ProcedureDivision(node, sentences);
					children.Add(pdiv);
				} else
				if (child.CodeElement is FunctionDeclarationEnd) {
					children.Add(new ProgramEnd(new URI(ProgramName)));
				} else {
					// TCRFUN_CODEGEN_NO_ADDITIONAL_DATA_SECTION
					// TCRFUN_CODEGEN_DATA_SECTION_AS_IS
					children.Add(child);
				}
			}
		}

		private void CreateOrUpdateLinkageSection(Compiler.Nodes.FunctionDeclaration node, ParametersProfile profile) {
			var linkage = node.Get<Compiler.Nodes.LinkageSection>("linkage");
			var parameters = profile.InputParameters.Count + profile.InoutParameters.Count + profile.OutputParameters.Count + (profile.ReturningParameter != null? 1:0);
			IReadOnlyList<DataDefinition> data = new List<DataDefinition>().AsReadOnly();
			if (linkage == null && parameters > 0) {
				var datadiv = node.Get<Compiler.Nodes.DataDivision>("data-division");
				if (datadiv == null) {
					datadiv = new DataDivision();
					children.Add(datadiv);
				}
				linkage = new LinkageSection();
				datadiv.Add(linkage);
			}
			if (linkage != null) data = linkage.Children();
			// TCRFUN_CODEGEN_PARAMETERS_ORDER
			var generated = new List<string>();
			foreach(var parameter in profile.InputParameters) {
				if (!generated.Contains(parameter.Name) && !Contains(data, parameter.Name)) {
					linkage.Add(CreateParameterEntry(parameter, node.SymbolTable));
					generated.Add(parameter.Name);
				}
			}
			foreach(var parameter in profile.InoutParameters) {
				if (!generated.Contains(parameter.Name) && !Contains(data, parameter.Name)) {
					linkage.Add(CreateParameterEntry(parameter, node.SymbolTable));
					generated.Add(parameter.Name);
				}
			}
			foreach(var parameter in profile.OutputParameters) {
				if (!generated.Contains(parameter.Name) && !Contains(data, parameter.Name)) {
					linkage.Add(CreateParameterEntry(parameter, node.SymbolTable));
					generated.Add(parameter.Name);
				}
			}
			if (profile.ReturningParameter != null) {
				if (!generated.Contains(profile.ReturningParameter.Name) && !Contains(data, profile.ReturningParameter.Name)) {
					linkage.Add(CreateParameterEntry(profile.ReturningParameter, node.SymbolTable));
					generated.Add(profile.ReturningParameter.Name);
				}
			}
		}
		private ParameterEntry CreateParameterEntry(ParameterDescription parameter, Compiler.CodeModel.SymbolTable table) {
			var generated = new ParameterEntry((ParameterDescriptionEntry)parameter.CodeElement, table);
			foreach(var child in parameter.Children) generated.Add(child);
			return generated;
		}

		private bool Contains(IReadOnlyList<DataDefinition> data, string dataname) {
			foreach(var node in data)
				if (dataname.Equals(node.Name))
					return true;
			return false;
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>(); // TCRFUN_CODEGEN_AS_NESTED_PROGRAM
					_cache.Add(new TextLineSnapshot(-1, "*_________________________________________________________________", null));
					_cache.Add(new TextLineSnapshot(-1, "IDENTIFICATION DIVISION.", null));
					_cache.Add(new TextLineSnapshot(-1, "PROGRAM-ID. "+ProgramName+'.', null));
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return false; } }
	}
}
