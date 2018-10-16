using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A Single pattern, a pattern is seen as an Action.
    /// </summary>
    public class Pattern : Action
    {
        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Pattern()
        {
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
