using System;
using System.Reflection;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Parser {
	internal partial class TypeCobolElementBuilder: TypeCobol.Compiler.Parser.Generated.TypeCobolBaseListener {
		internal CodeElementBuilder Builder = new CodeElementBuilder();

        public CodeElement CodeElement {
            get { return Builder.CodeElement; }
            private set { throw new System.NotSupportedException(); }
        }
        public CodeElementDispatcher Dispatcher {
			get { return Builder.Dispatcher; }
			internal set { Builder.Dispatcher = value; }
		}

		internal TypeCobolElementBuilder() {
//			var s = new CodeGenerator().GenerateCompositeBuilder(GetType(), Builder.GetType());
//			System.Console.WriteLine(s.ToString());
		}



		public override void EnterSomeRule(Generated.TypeCobolParser.SomeRuleContext context) {
			System.Console.WriteLine(context);
		}
	}
}
