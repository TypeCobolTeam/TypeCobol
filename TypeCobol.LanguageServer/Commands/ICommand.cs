using Newtonsoft.Json.Linq;

namespace TypeCobol.LanguageServer.Commands
{
    /// <summary>
    /// Describe any command implemented by this server.
    /// </summary>
    internal interface ICommand
    {
        private static readonly Dictionary<string, Func<TypeCobolServer, ICommand>> _Commands;

        static ICommand()
        {
            _Commands = new Dictionary<string, Func<TypeCobolServer, ICommand>>()
            {
                { "refactor/adjustFillers", AdjustFillers.Create }
                // Register all supported commands here
            };
        }

        /// <summary>
        /// All supported commands, to be sent to the client through the ServerCapabilities object.
        /// </summary>
        public static string[] SupportedCommands => _Commands.Keys.ToArray();

        /// <summary>
        /// Search for the command designated by the given name and instantiate it.
        /// </summary>
        /// <param name="commandName">Command identifier.</param>
        /// <param name="server">TypeCobolServer instance requesting the command.</param>
        /// <param name="command">Out param, the newly created command if found, null otherwise.</param>
        /// <returns>True when the command is found, False otherwise.</returns>
        public static bool TryActivateCommand(string commandName, TypeCobolServer server, out ICommand command)
        {
            if (_Commands.TryGetValue(commandName, out var activator))
            {
                command = activator(server);
                return true;
            }

            command = null;
            return false;
        }

        /// <summary>
        /// Run the command.
        /// </summary>
        /// <param name="arguments">Command arguments.</param>
        /// <returns>Command result.</returns>
        object Run(object[] arguments);
    }

    /// <summary>
    /// Base class for commands.
    /// </summary>
    internal abstract class AbstractCommand : ICommand
    {
        /// <summary>
        /// Helper method for reading untyped args of the command.
        /// Does not work for JSON primitive values
        /// </summary>
        /// <typeparam name="T">Target type for the argument.</typeparam>
        /// <param name="argument">Argument value to convert.</param>
        /// <param name="instance">[out] New instance of T converted from given argument value, default when given arg is not a JObject.</param>
        /// <returns>True when conversion succeeded, False otherwise.</returns>
        protected static bool TryReadArgumentAs<T>(object argument, out T instance)
        {
            if (argument is JObject jObject)
            {
                instance = jObject.ToObject<T>();
                return true;
            }

            instance = default;
            return false;
        }

        protected TypeCobolServer Server { get; }

        protected AbstractCommand(TypeCobolServer typeCobolServer)
        {
            Server = typeCobolServer;
        }

        public abstract object Run(object[] arguments);
    }
}
