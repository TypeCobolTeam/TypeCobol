namespace TypeCobol.LanguageServer.Commands
{
    /// <summary>
    /// Abstraction for environment. Override in unit tests to get consistent results
    /// across environments.
    /// </summary>
    public interface IEnvironmentVariableProvider
    {
        DateTime Now { get; }

        string UserName { get; }
    }

    /// <summary>
    /// Real implementation of the environment.
    /// </summary>
    public class RealEnvironment : IEnvironmentVariableProvider
    {
        public static readonly RealEnvironment Instance = new RealEnvironment();

        private RealEnvironment()
        {

        }

        public DateTime Now => DateTime.Now;

        public string UserName => Environment.UserName;
    }
}
