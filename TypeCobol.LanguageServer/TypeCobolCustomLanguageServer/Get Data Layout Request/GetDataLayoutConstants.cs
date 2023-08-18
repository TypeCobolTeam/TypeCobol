namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol
{
    /// <summary>
    /// Some constants related to the GetDataLayout request
    /// </summary>
    public static class GetDataLayoutConstants
    {
        // Output types (add TREE in the future?)
        internal const string OUTPUT_TYPE_CSV = "CSV";

        // For outputType = CSV
        internal const string SEPARATOR = ";";
        internal const string HEADER = "LineNumber;LevelNumber;VariableName;PictureTypeOrUsage;Start;End;Length";
    }
}
