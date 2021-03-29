using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Analysis;

namespace TypeCobol.Compiler.SqlAnalysis
{
    /// <summary>
    /// This class is derivated from Qualis.CobolAnalyzers.RuleSystem.RuleChecker
    /// Maybe we should think to put the basic RuleSystem in the Analysis namespace.
    /// </summary>
    internal abstract class SqlAbstractRuleChecker<T>
    {
        protected readonly Action<Violation> _addViolationAction;
        protected readonly string _ruleId;

        protected SqlAbstractRuleChecker(string ruleId, Action<Violation> addViolation)
        {
            _addViolationAction = addViolation;
            _ruleId = ruleId;
        }

        public abstract void Check(T item);
    }
}
