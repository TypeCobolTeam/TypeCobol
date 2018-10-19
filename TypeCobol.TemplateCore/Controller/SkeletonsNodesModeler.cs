using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.TemplateCore.Model;

namespace TypeCobol.TemplateCore.Controller
{
    /// <summary>
    /// Model Creator over a Skeletons Model visitor.
    /// </summary>
    public class SkeletonsNodesModeler : IModelVisitor<Node, Dictionary<string, Node>>
    {
        /// <summary>
        /// The Current Pattern
        /// </summary>
        Pattern CurretPattern
        {
            get;
            set;
        }

        /// <summary>
        /// The Current Skeleton
        /// </summary>
        Skeleton CurrentSkeleton
        {
            get;
            set;
        }

        public Node Visit(Condition that, Dictionary<string, Node> data)
        {
            Node targeNode = null;
            Model.Attribute nameAttribute = that.Attributes[AttributeNames.Node];
            string node = nameAttribute.Value.ToString();
            if (!data.ContainsKey(node))
            {//We must create a new node.
                Node n = new Node();
                n.Attributes[AttributeNames.Node] = nameAttribute;
                data[node] = n;
                targeNode = n;
            }
            else
            {
                targeNode = data[node];
            }
            targeNode.AddPatternWithGuard(CurretPattern, that, CurrentSkeleton);
            return targeNode;
        }

        public Node Visit(Conditions that, Dictionary<string, Node> data)
        {
            foreach(var condition in that.ConditionList)
            {
                condition.Accept(this, data);
            }
            return null;
        }

        public Node Visit(Pattern that, Dictionary<string, Node> data)
        {
            CurretPattern = that;
            return null;
        }

        public Node Visit(Patterns that, Dictionary<string, Node> data)
        {
            throw new NotImplementedException();
        }

        public Node Visit(Skeleton that, Dictionary<string, Node> data)
        {
            CurrentSkeleton = that;
            if (that.Conditions == null)
            {
                System.Console.Error.WriteLine(string.Format(Resource.SkeletonwithNoConditionsIgnored, that.Attributes[AttributeNames.Name].Value));
                return null;
            }
            //For each pattern we must visit all conditions.
            foreach (var pattern in that.Patterns.PatternList)
            {
                pattern.Accept(this, data);
                that.Conditions.Accept(this, data);
            }
            return null;
        }

        public Node Visit(Skeletons that, Dictionary<string, Node> data)
        {
            foreach (var skeleton in that.SkeletonList)
            {
                skeleton.Accept(this, data);
            }
            return null;
        }

        public Node Visit(Node that, Dictionary<string, Node> data)
        {
            throw new NotImplementedException();
        }
    }
}
