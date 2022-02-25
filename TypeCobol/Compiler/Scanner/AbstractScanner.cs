namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Base class for Cobol Scanner and SQL Scanner
    /// </summary>
    public abstract class AbstractScanner
    {
        protected int currentIndex;

        protected AbstractScanner(int startIndex)
        {
            currentIndex = startIndex;
        }

        public int CurrentIndex => currentIndex;

        public Token GetNextToken() => GetTokenStartingFrom(currentIndex);

        public abstract Token GetTokenStartingFrom(int startIndex);
    }
}
