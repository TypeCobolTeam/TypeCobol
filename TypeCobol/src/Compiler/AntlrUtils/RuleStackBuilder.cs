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
                builder.Insert(0, GetRuleName(parent) + '>');
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
