using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.Utilities
{
    public static class Extensions
    {
        public static TypeCobol.ExecutionStep? ExecutionStep(this LsrTestingOptions lsrOptions, TypeCobol.ExecutionStep? defaultValue)
        {
            switch (lsrOptions)
            {
                case LsrTestingOptions.NoLsrTesting:
                case LsrTestingOptions.LsrSourceDocumentTesting:
                    return defaultValue;
                case LsrTestingOptions.LsrScanningPhaseTesting:
                    return TypeCobol.ExecutionStep.Scanner;
                case LsrTestingOptions.LsrPreprocessingPhaseTesting:
                    return TypeCobol.ExecutionStep.Preprocessor;
                case LsrTestingOptions.LsrParsingPhaseTesting:
                    return TypeCobol.ExecutionStep.CodeElement;
                case LsrTestingOptions.LsrSemanticPhaseTesting:
                    return TypeCobol.ExecutionStep.SemanticCrossCheck;
                case LsrTestingOptions.LsrCodeAnalysisPhaseTesting:
                    return TypeCobol.ExecutionStep.CodeAnalysis;
            }
            return defaultValue;
        }

        public static string ToLanguageServerOption(this LsrTestingOptions lsrOptions)
        {
            switch (lsrOptions)
            {
                case LsrTestingOptions.LsrSourceDocumentTesting:
                    return "-tsource";
                case LsrTestingOptions.LsrScanningPhaseTesting:
                    return "-tscanner";
                case LsrTestingOptions.LsrPreprocessingPhaseTesting:
                    return "-tpreprocess";
                case LsrTestingOptions.LsrParsingPhaseTesting:
                    return "-tparser";
                case LsrTestingOptions.LsrSemanticPhaseTesting:
                    return "-tsemantic";
                case LsrTestingOptions.LsrCodeAnalysisPhaseTesting:
                    return "-tcodeanalysis";
                case LsrTestingOptions.NoLsrTesting:
                default:
                    return "";
            }
        }

        /// <summary>
        /// Gets the original URI (which was set by the client)
        /// DON'T use ToString() as it returns the canonically unescaped form of the URI
        /// (it may cause issue if the path contains some blanks which need to be escaped)
        /// </summary>
        /// <param name="uri"></param>
        /// <returns></returns>
        public static string GetOriginalUri(this Uri uri)
        {
            return uri.OriginalString;
        }
    }
}
