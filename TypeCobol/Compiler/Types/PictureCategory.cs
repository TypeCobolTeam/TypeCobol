namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Category of a Picture Type
    /// </summary>
    public enum PictureCategory : sbyte
    {
        Error = 0, //Invalid Picture Type --> Constructed from an Invalid Picture String.
        Alphabetic = 0x1 << 0,
        Numeric = 0x1 << 1,
        AlphaNumeric = Alphabetic | Numeric,
        Edited = 0x1 << 2,
        NumericEdited = Numeric | Edited,
        AlphaNumericEdited = AlphaNumeric | Edited,
        Dbcs = 0x1 << 3,
        ExternalFloat = 0x1 << 4 | Numeric | Edited,
        National = 0x1 << 5,
        NationalEdited = National | Edited
    }
}
