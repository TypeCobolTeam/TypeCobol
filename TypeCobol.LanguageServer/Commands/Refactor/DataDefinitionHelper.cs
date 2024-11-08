using System.Text;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    // TODO Move content to DataDefinitionExtensions and convert to extension methods
    internal static class DataDefinitionHelper
    {
        private record struct LeftmostCharacterPositionInfo(long DeltaParent, List<(string Index, long OccurenceSize)> EnclosingOccurs)
        {
            public LeftmostCharacterPositionInfo()
                : this(1, new List<(string Index, long OccurenceSize)>())
            {

            }

            public string ToExpression()
            {
                if (DeltaParent == 1 && EnclosingOccurs.Count == 0)
                    return null;

                var builder = new StringBuilder();
                builder.Append(DeltaParent);
                if (EnclosingOccurs.Count > 0)
                {
                    foreach (var enclosingOccurs in EnclosingOccurs)
                    {
                        builder.Append(" + (");
                        builder.Append(enclosingOccurs.Index);
                        builder.Append(" - 1) * ");
                        builder.Append(enclosingOccurs.OccurenceSize);
                    }
                }

                return builder.ToString();
            }
        }

        public static (string Name, string ReferenceModifier) GetClosestAccessor(DataDefinition dataDefinition, string[] indices)
        {
            var info = new LeftmostCharacterPositionInfo();

            var currentDefinition = dataDefinition;
            var parentDefinition = currentDefinition.Parent as DataDefinition;
            int index = indices.Length - 1;
            while (parentDefinition != null && string.IsNullOrEmpty(currentDefinition.Name))
            {
                info.DeltaParent += currentDefinition.StartPosition - parentDefinition.StartPosition;
                if (currentDefinition.IsTableOccurence)
                {
                    long occurenceSize = currentDefinition.PhysicalLength / currentDefinition.MaxOccurencesCount;
                    string indexName = indices[index];
                    info.EnclosingOccurs.Add((indexName, occurenceSize));
                    index--;
                }

                currentDefinition = parentDefinition;
                parentDefinition = currentDefinition.Parent as DataDefinition;
            }

            if (string.IsNullOrEmpty(currentDefinition.Name))
            {
                // Could not find any named data: no accessor is available for the given DataDefinition
                return (null, null);
            }

            string leftMostCharacterPositionExpression = info.ToExpression();
            string referenceModifier = leftMostCharacterPositionExpression != null ? $"({leftMostCharacterPositionExpression}:{dataDefinition.PhysicalLength})" : null;
            return (currentDefinition.Name, referenceModifier);
        }

        public static bool IsGroup(DataDefinition dataDefinition)
        {
            // TODO Improve this !
            return dataDefinition.ChildrenCount > 0;
        }
    }
}
