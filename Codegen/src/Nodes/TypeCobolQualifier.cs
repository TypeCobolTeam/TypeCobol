namespace TypeCobol.Codegen.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.Scanner;
	using TypeCobol.Compiler.Text;



internal class TypeCobolQualifier: Compiler.Nodes.Node, Generated {

    Compiler.Nodes.Node Node;
	public TypeCobolQualifier(Compiler.Nodes.Node node): base(node.CodeElement) {
        Node = node;
		foreach(var child in node.Children) Add(child);
	}

	private IList<ITextLine> _cache = null;
	public override IEnumerable<ITextLine> Lines {
		get {
			if (_cache == null) {
				var text = CodeElement.SourceText;
				var statement = CodeElement as MoveSimpleStatement;
				if (statement != null) {
					int index = 0;
					foreach(var var in statement.Vars.Keys)
						ReplaceIfTypeCobol(ref text, var, statement.ConsumedTokens, ref index);
				}
				_cache = new List<ITextLine>();
				_cache.Add(new TextLineSnapshot(-1, text, null));
			}
			return _cache;
		}
	}
	private void ReplaceIfTypeCobol(ref string source, StorageArea variable, IList<Compiler.Scanner.Token> tokens, ref int index) {
		string head = variable.SymbolReference.ToString();
		string tail = null;
		if (variable.SymbolReference.IsQualifiedReference) {
			var qualified = (QualifiedSymbolReference)variable.SymbolReference;
			head = qualified.First.ToString();
			tail = qualified.Tail.ToString();
		}
		Token first, last = null;
		int ifirst = IndexOf(tokens, out first, head, index);
		int ilast  = ifirst+1;
		if (tail != null) ilast = IndexOf(tokens, out last, tail, index);
		int begin, end;
		GetSpan(first, last, out begin, out end);
		index = System.Math.Max(ifirst, ilast);

		var typecobol = variable.SymbolReference as TypeCobolQualifiedSymbolReference;
		if (typecobol == null) return; // nothing to do
		var str = new System.Text.StringBuilder();
		begin -= 7; // indexes are decaled because of the 7 first columns of a COBOL file
		end   -= 7; // (6 characters for sequence number + indicator character)
		str.Append(source.Substring(0,begin));
		string delimiter = " OF ";
		foreach(var reference in typecobol) str.Append(reference).Append(delimiter);
		if (typecobol.Count > 0) str.Length -= delimiter.Length;
		str.Append(source.Substring(end,source.Length-end));
		source = str.ToString();
	}
	private int IndexOf(IList<Token> tokens, out Token found, string text, int index = 0) {
		for(int c=index; c<tokens.Count; c++) {
			if (text.Equals(tokens[c].Text)) {
				found = tokens[c];
				return c;
			}
		}
		found = null;
		throw new System.ArgumentException("No token with text=\""+text+"\" found among "+tokens.Count+" tokens (starting from "+index+")");
	}
	private void GetSpan(Token first, Token last, out int begin, out int end) {
		// if SymbolReference is unqualified, last is nil
		if (last == null) {
			begin = first.StartIndex;
			end   = first.StopIndex+1;
		} else
		// as we can have either head::tail (TypeCobol) OR tail OF|IN head (COBOL),
		// we can have first before last OR first _after_ last in tokens in ConsumedTokens
		if (first.StartIndex < last.StartIndex) {
			begin = first.StartIndex;
			end   = last.StopIndex+1;
		} else {
			begin = last.StartIndex;
			end   = first.StopIndex+1;
		}
	}

	public bool IsLeaf { get { return false; } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            //Generated Node doesn't need to be visited
            return false;
        }
    }

}
