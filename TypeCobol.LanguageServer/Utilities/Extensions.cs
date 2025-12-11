namespace TypeCobol.LanguageServer.Utilities
{
    public static class Extensions
    {
        public static ExecutionStep ExecutionStep(this LsrTestingOptions lsrOptions, ExecutionStep defaultValue)
        {
            switch (lsrOptions)
            {
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

            // NoLsrTesting, LsrSourceDocumentTesting or invalid values => use provided default value
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
    }
}
