﻿using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	internal class FunctionDeclaration: Node, Generated {

		QualifiedName ProgramName = null;

		public FunctionDeclaration(Node node) {
			var header = (FunctionDeclarationHeader)node.CodeElement;
			ProgramName = header.Name;
			FunctionDeclarationProfile profile = null;
			foreach(var child in node.Children) {
				var ce = child.CodeElement;
				if (child.CodeElement is FunctionDeclarationProfile) {
					profile = (FunctionDeclarationProfile)child.CodeElement;
					CreateOrUpdateLinkageSection(node, profile.Profile);
					var pdiv = new ProcedureDivision(profile);
					foreach(var sentence in child.Children)
						pdiv.Children.Add(sentence);
					Children.Add(pdiv);
				} else
				if (child.CodeElement is FunctionDeclarationEnd) {
					Children.Add(new TypeCobol.Codegen.Nodes.ProgramEnd(ProgramName));
				} else {
					// TCRFUN_CODEGEN_NO_ADDITIONAL_DATA_SECTION
					// TCRFUN_CODEGEN_DATA_SECTION_AS_IS
					Children.Add(child);
				}
			}
		}

		private void CreateOrUpdateLinkageSection(Node node, ParametersProfile profile) {
			var linkage = node.Get("linkage");
			var parameters = profile.InputParameters.Count + profile.InoutParameters.Count + profile.OutputParameters.Count + (profile.ReturningParameter != null? 1:0);
			IList<Node> data = new List<Node>();
			if (linkage == null && parameters > 0) {
				linkage = new LinkageSection();
				Children.Add(linkage);
			}
			if (linkage != null) data = linkage.GetChildren(typeof(DataDescriptionEntry));
			// TCRFUN_CODEGEN_PARAMETERS_ORDER
			var generated = new List<string>();
			foreach(var parameter in profile.InputParameters) {
				if (!generated.Contains(parameter.Name.Name) && !Contains(data, parameter.Name.Name)) {
					linkage.Add(new ParameterEntry(parameter));
					generated.Add(parameter.Name.Name);
				}
			}
			foreach(var parameter in profile.InoutParameters) {
				if (!generated.Contains(parameter.Name.Name) && !Contains(data, parameter.Name.Name)) {
					linkage.Add(new ParameterEntry(parameter));
					generated.Add(parameter.Name.Name);
				}
			}
			foreach(var parameter in profile.OutputParameters) {
				if (!generated.Contains(parameter.Name.Name) && !Contains(data, parameter.Name.Name)) {
					linkage.Add(new ParameterEntry(parameter));
					generated.Add(parameter.Name.Name);
				}
			}
			if (profile.ReturningParameter != null) {
				if (!generated.Contains(profile.ReturningParameter.Name.Name) && !Contains(data, profile.ReturningParameter.Name.Name)) {
					linkage.Add(new ParameterEntry(profile.ReturningParameter));
					generated.Add(profile.ReturningParameter.Name.Name);
				}
			}
		}

		private bool Contains(IList<Node> data, string name) {
			foreach(var node in data)
				if (name.Equals(((DataDescriptionEntry)node.CodeElement).Name.Name))
					return true;
			return false;
		}

		private IList<ITextLine> _cache = null;
		public override IEnumerable<ITextLine> Lines {
			get {
				if (_cache == null) {
					_cache = new List<ITextLine>(); // TCRFUN_CODEGEN_AS_NESTED_PROGRAM
					_cache.Add(new TextLineSnapshot(-1, "PROGRAM-ID. "+ProgramName.Head+'.', null));
				}
				return _cache;
			}
		}

		public bool IsLeaf { get { return false; } }
	}
}
