namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Category of a Picture Type
    /// </summary>
    public enum PictureCategory
    {
        Invalid = 0,
        Alphabetic = 0x01 << 0,
        Alphanumeric = 0x01 << 1,
        AlphanumericEdited = 0x01 << 2,
        DBCS = 0x01 << 3,
        National = 0x01 << 4,
        NationalEdited = 0x01 << 5,
        ExternalFloatingPoint = 0x01 << 6,
        Numeric = 0x01 << 7,
        NumericEdited = 0x01 << 8
    }
}
