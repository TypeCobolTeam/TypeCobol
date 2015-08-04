using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.CodeElements
{
    public interface FlowControl
    {
        /// <summary>
        ///     Nests a CodeElement (presumably a statement) in this FlowControl statement.
        /// </summary>
        /// <param name="e">CodeElement to be nested</param>
        /// <return>False if e is out of scope of this FlowControl.</return>
        bool AddNestedElement(CodeElement e);

        /// <summary>
        ///     Closes scope. No more CodeElements can be nested after a call to CloseSclope.
        /// </summary>
        void CloseScope();
    }
}
