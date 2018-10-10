using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// The base class of an attributed entity: an entity which have attributes
    /// </summary>
    public class AttributedEntity : Dictionary<string,Attribute>
    {
        /// <summary>
        /// Empty constructor
        /// </summary>
        public AttributedEntity()
        {

        }
    }
}
