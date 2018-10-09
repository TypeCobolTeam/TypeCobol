using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// An action
    /// </summary>
    public class Action : AttributedEntity
    {
        /// <summary>
        /// The action's code
        /// </summary>
        public String Code
        {
            get;
            set;
        }

        /// <summary>
        /// Empty constructor
        /// </summary>
        public Action()
        {
            Code = "";
        }

        /// <summary>
        /// Code constructor.
        /// </summary>
        /// <param name="code"></param>
        public Action(string code)
        {
            Code = code;
        }
    }
}
