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
    public class AttributedEntity
    {
        /// <summary>
        /// Attributes
        /// </summary>
        public Dictionary<string, Attribute> Attributes
        {
            get;
            set;
        }
        /// <summary>
        /// Empty constructor
        /// </summary>
        public AttributedEntity()
        {
            Attributes = new Dictionary<string, Attribute>();
        }
    }
}
