using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Model;

namespace TypeCobol.TemplateCore.Controller
{
    /// <summary>
    /// Skeletons Controller class, that manages the tranpilation of a set of Skeletons
    /// to their C# code.
    /// </summary>
    public class SkeletonsController : ITranspilable
    {
        /// <summary>
        /// The list of Skeleton
        /// </summary>
        public Skeletons SkeletonsList
        {
            get;
            set;
        }

        /// <summary>
        /// The Nodes 
        /// </summary>
        public Dictionary<string, Node> Nodes
        {
            get;
            private set;
        }

        public string TranspiledCode => throw new NotImplementedException();


        /// <summary>
        /// Empty constructor.
        /// </summary>
        public SkeletonsController() : this(null)
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="skeletons">The list of Skeletons</param>
        public SkeletonsController(Skeletons skeletons)
        {
            this.SkeletonsList = skeletons;
            Nodes = new Dictionary<string, Node>();
        }

        /// <summary>
        /// Perform the transpilation of all actions on Nodes.
        /// </summary>
        public void TranspileNodeActions()
        {
            //First Construction the Dictionary of Nodes as model of nodes.
            SkeletonsNodesModeler modeler = new SkeletonsNodesModeler();
            Nodes = new Dictionary<string, Node>();
            this.SkeletonsList.Accept(modeler, Nodes);

            //Now that we have node model

        }
    }
}
