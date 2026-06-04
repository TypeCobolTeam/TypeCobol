namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Exception indicating that source code is not supported by GetDataLayoutRequest
    /// For example:
    /// Source code contains at least one data definition using a SYNCHRONIZED clause and included in an OCCURS.
    /// As it may lead to wrong data length and positions in GetDataLayoutRequest, it is better to throw this exception in such situations.
    /// </summary>
    public class DataLayoutNotSupportedException : Exception
    {
        public DataLayoutNotSupportedException(string msg) : base(msg)
        {
        }
    }
}
