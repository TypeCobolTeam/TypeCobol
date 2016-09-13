using System;
using System.IO;
using System.Linq;

namespace TypeCobol.LanguageServices.CodeAnalysis.Statistics
{
    /// <summary>
    /// Class used by StatsGenerator to compute statistics about
    /// a specific element of the Cobol Language - of type <E> .
    /// </summary>
    class StatsCounter<E>
    {
        private int[] elementTypes;
        private int maxElementType;

        private int programCount;

        private long[] currentProgramCounters;

        private long[] totalCounters;
        private long[] totalMins;
        private long[] totalMaxs;

        private long currentProgramCounter;

        private long totalCounter;
        private long totalMin;
        private long totalMax;

        private long[] distributionBoundaries;
        private long[] totalDistributionCounters;

        public StatsCounter(long[] distributionBoundaries)
        {
            elementTypes = (int[])Enum.GetValues(typeof(E));
            maxElementType = elementTypes.Max();

            currentProgramCounters = new long[maxElementType + 1];

            totalCounters = new long[maxElementType + 1];
            totalMins = new long[maxElementType + 1];
            for (int elementTypeIndex = 0; elementTypeIndex <= maxElementType; elementTypeIndex++)
            {
                totalMins[elementTypeIndex] = long.MaxValue;
            }
            totalMaxs = new long[maxElementType + 1];

            totalMin = long.MaxValue;

            this.distributionBoundaries = distributionBoundaries;
            totalDistributionCounters = new long[distributionBoundaries.Length];
        }

        public void OnBeginProgram()
        {
            programCount++;
            currentProgramCounter = 0;
            for (int elementTypeIndex = 0; elementTypeIndex <= maxElementType; elementTypeIndex++)
            {
                currentProgramCounters[elementTypeIndex] = 0;
            }
        }

        public void OnElement(int elementTypeIndex)
        {
            currentProgramCounters[elementTypeIndex]++;
            currentProgramCounter++;
        }

        public void OnEndProgram()
        {
            for (int elementTypeIndex = 0; elementTypeIndex <= maxElementType; elementTypeIndex++)
            {
                totalCounters[elementTypeIndex] += currentProgramCounters[elementTypeIndex];
                if (currentProgramCounters[elementTypeIndex] < totalMins[elementTypeIndex]) totalMins[elementTypeIndex] = currentProgramCounters[elementTypeIndex];
                if (currentProgramCounters[elementTypeIndex] > totalMaxs[elementTypeIndex]) totalMaxs[elementTypeIndex] = currentProgramCounters[elementTypeIndex];
            }

            totalCounter += currentProgramCounter;
            if (currentProgramCounter < totalMin) totalMin = currentProgramCounter;
            if (currentProgramCounter > totalMax) totalMax = currentProgramCounter;

            for (int i = 0; i < distributionBoundaries.Length; i++)
            {
                if (currentProgramCounter <= distributionBoundaries[i])
                {
                    totalDistributionCounters[i]++;
                    break;
                }
            }
        }

        public void DisplayResults(TextWriter writer)
        {
            writer.WriteLine("Statistics for " + typeof(E).Name + " on " + programCount + " programs :");
            writer.WriteLine(" - Average number per program : " + (totalCounter / programCount));
            writer.WriteLine(" - Minimum number per program : " + totalMin);
            writer.WriteLine(" - Maximum number per program : " + totalMax);
            writer.WriteLine(" - Distribution of counts per program : ");
            for (int i = 0; i < distributionBoundaries.Length; i++)
            {
                writer.WriteLine("   . " + (i == 0 ? 0 : distributionBoundaries[i - 1]) + " - " + (distributionBoundaries[i] == long.MaxValue ? "+++" : distributionBoundaries[i].ToString()) + " : " + ((totalDistributionCounters[i] * 10000) / programCount / (float)100).ToString() + " %");
            }
            writer.WriteLine(" - Details per element type : ");
            foreach (var elementTypeIndex in elementTypes.OrderByDescending(elementTypeIndex => elementTypeIndex >= 0 ? totalCounters[elementTypeIndex] : 0))
            {
                if (elementTypeIndex >= 0)
                {
                    float elementTypePercentage = ((totalCounters[elementTypeIndex] * 10000 / totalCounter) / (float)100);
                    if (elementTypePercentage >= 0.1)
                    {
                        writer.WriteLine("   . " + Enum.GetName(typeof(E), elementTypeIndex) + " : " + elementTypePercentage + " %");
                        writer.WriteLine("      + Average number per program : " + (totalCounters[elementTypeIndex] / programCount));
                        writer.WriteLine("      + Minimum number per program : " + totalMins[elementTypeIndex]);
                        writer.WriteLine("      + Maximum number per program : " + totalMaxs[elementTypeIndex]);
                    }
                }
            }
            writer.WriteLine();
        }
    }
}
