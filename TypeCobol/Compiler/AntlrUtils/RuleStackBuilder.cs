using Antlr4.Runtime;
using System.Text;

namespace TypeCobol.Compiler.AntlrUtils
{
    class RuleStackBuilder
    {
        public string GetRuleStack(RuleContext context)
        {
            var builder = new StringBuilder(GetRuleName(context));
            var parent = context.parent;
            while (parent != null)
            {
                var ruleName = GetRuleName(parent);
                // Hide the root rule which is useful for error recovery and perf, but does not exist in the pure Cobol grammar
                if (ruleName == "cobolCodeElements") break;

                builder.Insert(0, ruleName + '>');
                parent = parent.parent;
            }
            return builder.ToString();
        }

        private string GetRuleName(RuleContext context)
        {
            string classname = context.GetType().Name;
            var builder = new StringBuilder(classname);
            builder.Length -= "Context".Length;
            builder[0] = System.Char.ToLower(classname[0]);
            // could have done "RULE_" + builder.ToString(), then find int value of the field of CodeElementsParser 
            // corresponding to it, then got the exact name in CodeElementsParser.ruleNames, but ... :P
            return builder.ToString();
        }
    }
}
