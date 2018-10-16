using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// The Node Model
    /// </summary>
    public class Node : AttributedEntity, ITranspilable
    {
        /// <summary>
        /// All Guards on this Node
        /// </summary>
        public List<Tuple<List<Condition>, Pattern>> Guards
        {
            get;
            private set;
        }

        /// <summary>
        /// Pattern, Guards matching.
        /// </summary>
        public Dictionary<Pattern, List<Condition>> PatternGuards
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Node()
        {
            Guards = new List<Tuple<List<Condition>, Pattern>>();
            PatternGuards = new Dictionary<Pattern, List<Condition>>();
        }
        public string TranspiledCode => throw new NotImplementedException();

        /// <summary>
        /// Add a Pattern with its Guard.
        /// </summary>
        /// <param name="pattern">The pattern to add</param>
        /// <param name="guard">The Guard condition</param>
        internal void AddPatternWithGuard(Pattern pattern, Condition guard)
        {
            List<Condition> guards = null;
            if (PatternGuards.ContainsKey(pattern))
            {
                guards = PatternGuards[pattern];
            }
            else
            {
                guards = new List<Condition>();
                Guards.Add(new Tuple<List<Condition>, Pattern>(guards, pattern));
                PatternGuards[pattern] = guards;
            }
            guards.Add(guard);
        }

        /// <summary>
        /// Visitor Method
        /// </summary>
        /// <typeparam name="R"></typeparam>
        /// <typeparam name="D"></typeparam>
        /// <param name="v"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        public R Accept<R, D>(IModelVisitor<R, D> v, D data)
        {
            return v.Visit(this, data);
        }
    }
}
