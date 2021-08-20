using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Tools.APIHelpers;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Helper class to load custom analyzers from an external .NET DLL file.
    /// </summary>
    public static class AnalyzerProviderLoader
    {
        private static readonly Type _IAnalyzerProviderType = typeof(IAnalyzerProvider);

        /// <summary>
        /// Add each AnalyzerProvider from the given assemblies.
        /// </summary>
        /// <param name="compositeAnalyzerProvider">CompositeAnalyzerProvider instance into which the external analyzers should be added.</param>
        /// <param name="assemblyFilePaths">List of paths of .NET Assembly files.</param>
        /// <param name="addDiagnostic">Method to log a diagnostic linked to a file.</param>
        public static void AddCustomProviders(this CompositeAnalyzerProvider compositeAnalyzerProvider, IEnumerable<string> assemblyFilePaths, Action<string, Diagnostic> addDiagnostic)
        {
            if (compositeAnalyzerProvider == null || assemblyFilePaths == null) return;

            foreach (var assemblyFilePath in assemblyFilePaths)
            {
                try
                {
                    var provider = LoadProvider(assemblyFilePath);
                    compositeAnalyzerProvider.AddProvider(provider);
                }
                catch (Exception exception)
                {
                    var diagnostic = new Diagnostic(MessageCode.AnalyzerFailure, Diagnostic.Position.Default, $"Failed to load analyzer provider on path {assemblyFilePath}", exception.Message, exception);
                    addDiagnostic(assemblyFilePath, diagnostic);
                }
            }
        }

        /// <summary>
        /// Look for a suitable type into the assembly given and instantiate a new IAnalyzerProvider from it.
        /// <remarks>Caller must catch potential exceptions.</remarks>
        /// </summary>
        /// <param name="assemblyFilePath">Path to the assembly.</param>
        /// <returns>A non-null instance of IAnalyzerProvider.</returns>
        public static IAnalyzerProvider LoadProvider(string assemblyFilePath)
        {
            var assembly = Assembly.LoadFrom(assemblyFilePath);
            var constructor = assembly
                .GetTypes()
                .Where(t => t.IsPublic && t.IsClass && !t.IsAbstract) //public non-abstract classes
                .Where(t => t.GetInterfaces().Contains(_IAnalyzerProviderType)) //which implement IAnalyzerProvider
                .Select(t => t.GetConstructor(Type.EmptyTypes)) //with a public parameterless constructor
                .Single(); //only one provider allowed per assembly file
            return (IAnalyzerProvider) constructor.Invoke(null);
        }
    }
}
