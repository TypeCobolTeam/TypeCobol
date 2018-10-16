using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// Interface for a Visitor on the model
    /// </summary>
    /// <typeparam name="R"></typeparam>
    /// <typeparam name="D"></typeparam>
    public interface IModelVisitor<R,D>
    {
        R Visit(Condition that, D data);
        R Visit(Conditions that, D data);
        R Visit(Pattern that, D data);
        R Visit(Patterns that, D data);
        R Visit(Skeleton that, D data);
        R Visit(Skeletons that, D data);
        R Visit(Node that, D data);
    }
}
