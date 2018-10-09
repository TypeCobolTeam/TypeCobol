using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    public class Attribute
    {
        /// <summary>
        /// The attribute's System type.
        /// </summary>
        public System.Type Type
        {
            get;
            set;
        }

        /// <summary>
        /// The attribute's name
        /// </summary>
        public string Name
        {
            get;
            set;
        }

        /// <summary>
        /// The Attribute's value.
        /// </summary>
       public object Value
        {
            get;
            set;
        }
    }
}
