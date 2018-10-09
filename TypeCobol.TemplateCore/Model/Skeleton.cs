using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A Single Skeleton template
    /// </summary>
    public class Skeleton : AttributedEntity
    {
        /// <summary>
        /// Conditions on the Skeleton.
        /// </summary>
        public Conditions Conditions
        {
            get;
            set;
        }

        /// <summary>
        /// Patterns of the Skeleton.
        /// </summary>
        public Patterns Patterns
        {
            get;
            set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Skeleton()
        {

        }
    }
}
