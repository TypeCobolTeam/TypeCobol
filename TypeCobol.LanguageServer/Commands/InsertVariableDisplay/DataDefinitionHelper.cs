using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Helper class to compute an 'accessor' for a given DataDefinition.
    /// An accessor captures all needed information to get access to the targeted data at runtime: if the targeted
    /// data is named, the accessor is actually the data itself. For a FILLER or anonymous data, the accessor
    /// will contain the first named data that can be used to access the targeted data and the associated
    /// position and length in order to read the correct memory area. Position and length combined form the
    /// reference modifier.
    /// </summary>
    internal static class DataDefinitionHelper
    {
        /// <summary>
        /// Private struct used to compute the leftmost character position expression.
        /// </summary>
        /// <param name="DeltaParent">Delta in bytes of the targeted data from its parent data.
        /// Starts at 1 since the position of a data is 1-based in Cobol.</param>
        /// <param name="EnclosingOccurs">List of tuples made of the name of an index and size of an occurence.
        /// Each tuple corresponds to an OCCURS dimension that need to be traversed to get to the targeted data,
        /// this is used to access data enclosed in anonymous arrays.</param>
        private record struct LeftmostCharacterPositionInfo(long DeltaParent, List<(string Index, long OccurenceSize)> EnclosingOccurs)
        {
            public LeftmostCharacterPositionInfo()
                : this(1, new List<(string Index, long OccurenceSize)>())
            {

            }

            public List<string> BuildReferenceModifier(long physicalLength)
            {
                // Default position: no need to create an expression. The whole data is used directly.
                if (DeltaParent == 1 && EnclosingOccurs.Count == 0)
                    return null;

                // Start with delta parent preceded by an opening parenthesis
                List<string> words = ['(' + DeltaParent.ToString() ];

                if (EnclosingOccurs.Count > 0)
                {
                    // Add offset for each traversed OCCURS dimension
                    foreach (var enclosingOccurs in EnclosingOccurs)
                    {
                        // Shift from the total size of occurrences based on current index value
                        // <deltaParent> + (Idx1 - 1) * <occ1Size> + (Idx2 - 1) * <occ2Size> + (Idx3 - 1) * <occ3Size>...
                        words.Add("+");
                        words.Add('(' + enclosingOccurs.Index);
                        words.Add("-");
                        words.Add("1)");
                        words.Add("*");
                        words.Add(enclosingOccurs.OccurenceSize.ToString());
                    }
                }

                // Add length and closing parenthesis on last word
                string lastWord = words[^1] + ':' + physicalLength + ')';
                words[^1] = lastWord;

                return words;
            }
        }

        /// <summary>
        /// Accessor for a DataDefinition.
        /// </summary>
        /// <param name="Data">DataDefinition to use to access originally targeted DataDefinition.</param>
        /// <param name="IndicesCount">Number of indices to use on the accessor data. When the target data is nested under
        /// anonymous arrays, the data used to access the target is on a different OCCURS dimension. Only some of the first
        /// indices are used and this number indicates the count of them.</param>
        /// <param name="ReferenceModifier">Reference modifier to apply on Data as a list of words. When null it means no reference modifier is required.</param>
        public record struct DataAccessor(DataDefinition Data, int IndicesCount, List<string> ReferenceModifier);

        /// <summary>
        /// Compute accessor for given DataDefinition and indices.
        /// </summary>
        /// <param name="dataDefinition">Targeted DataDefinition.</param>
        /// <param name="indices">Indices required to access the targeted data (subscripts) when it is located in one
        /// or more nested OCCURS.</param>
        /// <returns></returns>
        public static DataAccessor GetAccessor(DataDefinition dataDefinition, string[] indices)
        {
            var info = new LeftmostCharacterPositionInfo();

            // Find the closest named data
            var currentDefinition = dataDefinition;
            var parentDefinition = currentDefinition.Parent as DataDefinition;
            int index = indices.Length - 1; // Current index used, starting from innermost as we are walking up the chain of parents
            while (parentDefinition != null && string.IsNullOrEmpty(currentDefinition.Name))
            {
                info.DeltaParent += currentDefinition.StartPosition - parentDefinition.StartPosition; // Update delta
                if (currentDefinition.IsTableOccurence)
                {
                    // Capture enclosing OCCURS details
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
                return new DataAccessor(null, 0, null);
            }

            // Compute reference modifier
            var referenceModifier = info.BuildReferenceModifier(dataDefinition.PhysicalLength);
            int indicesCount = index + 1; // Number of indices used to access the data
            return new DataAccessor(currentDefinition, indicesCount, referenceModifier);
        }
    }
}
