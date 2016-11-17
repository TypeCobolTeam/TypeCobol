namespace TypeCobol.Codegen.Nodes {
    using System.Collections.Generic;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.Text;

    internal class ProcedureStyleCall : Compiler.Nodes.Call, Generated {
        private Compiler.Nodes.ProcedureStyleCall Node;

        public ProcedureStyleCall(Compiler.Nodes.ProcedureStyleCall node)
            : base(null) {
            this.Node = node;
        }

        public override CodeElement CodeElement {
            get { return this.Node.CodeElement; }
        }

        private List<ITextLine> _cache = null;

        public override IEnumerable<ITextLine> Lines {
            get {
                if (_cache == null) {
                    var procedureCall = ((ProcedureStyleCallStatement) Node.CodeElement).ProcedureCall;
                    _cache = new List<ITextLine>();

                    var hash = GetHash(procedureCall.ProcedureName.Name);

                    var callTextLine = new TextLineSnapshot(-1, "    CALL " + hash + " USING ", null);
                    _cache.Add(callTextLine);
                    var prefix = new string(' ', callTextLine.Length + 1);

                    //Style 1, maybe more TC style but use only if we have a 72 column wrapper
                    /*
                    StringBuilder sb = new StringBuilder();
                    foreach (var parameter in procedureCall.InputParameters) {
                        sb.Append(" " + parameter.StorageAreaOrValue);
                    }
                    _cache.Add(new TextLineSnapshot(-1, prefix + sb, null));
                    sb.Clear();
                    foreach (var parameter in procedureCall.InoutParameters)
                    {
                        sb.Append(" " + parameter.StorageAreaOrValue);
                    }
                    _cache.Add(new TextLineSnapshot(-1, prefix + sb, null));
                    sb.Clear();
                    foreach (var parameter in procedureCall.OutputParameters)
                    {
                        sb.Append(" " + parameter.StorageAreaOrValue);
                    }
                    _cache.Add(new TextLineSnapshot(-1, prefix + sb, null));
                    sb.Clear();
                    */

                    //Style 2, which is more like our legacy Cobol 85 code style
                    // but perhaps less TC style
                    foreach (var parameter in procedureCall.Arguments) {
                        _cache.Add(new TextLineSnapshot(-1, prefix + parameter.StorageAreaOrValue, null));
                    }

                    //_cache.Add(new TextLineSnapshot(-1, str, null));
                }
                return _cache;
            }
        }

        private string GetHash(string function) {
            var found = Node.SymbolTable.GetFunction(new Compiler.CodeElements.Expressions.URI(function));
            if (found.Count != 1) return function;
            return ((Compiler.Nodes.FunctionDeclaration) found[0]).Hash;
        }

        public bool IsLeaf {
            get { return true; }
        }
    }
}