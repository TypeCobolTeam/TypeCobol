using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using JetBrains.Annotations;
using TypeCobol.Tools;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Helper class to load custom analyzers from an external .NET DLL file.
    /// </summary>
    public static class AnalyzerProviderLoader
    {
        /// <summary>
        /// Add each AnalyzerProvider from the given assemblies.
        /// </summary>
        /// <param name="analyzerProviderWrapper">AnalyzerProviderWrapper instance into which the external analyzers should be added.</param>
        /// <param name="assemblyFilePaths">List of paths of .NET Assembly files.</param>
        /// <param name="logger">Method to log potential exceptions.</param>
        public static void AddCustomProviders(this AnalyzerProviderWrapper analyzerProviderWrapper, IEnumerable<string> assemblyFilePaths, [NotNull] Action<string> logger)
        {
            if (analyzerProviderWrapper == null || assemblyFilePaths == null) return;

            foreach (var assemblyFilePath in assemblyFilePaths)
            {
                try
                {
                    var provider = UnsafeLoadProvider(assemblyFilePath);
                    analyzerProviderWrapper.AddProvider(provider);
                }
                catch (Exception exception)
                {
                    logger($"Failed to load analyzer provider from path {assemblyFilePath}{Environment.NewLine}{exception.GetType().FullName} has been thrown.{Environment.NewLine}{exception.Message}");
                }
            }
        }

        /// <summary>
        /// Look for a suitable type into the assembly given and instantiate a new IAnalyzerProvider from it.
        /// <remarks>Caller must catch potential exceptions.</remarks>
        /// </summary>
        /// <param name="assemblyFilePath">Path to the assembly.</param>
        /// <returns>A non-null instance of IAnalyzerProvider.</returns>
        public static IAnalyzerProvider UnsafeLoadProvider(string assemblyFilePath)
        {
            //Implementing type must be a public class, non-abstract, with a public parameterless constructor
            //Only one provider allowed per assembly file
            return Assembly.LoadFrom(assemblyFilePath).Activate<IAnalyzerProvider>().Single();
        }
    }
}
