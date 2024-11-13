using Newtonsoft.Json.Linq;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Commands.Refactor
{
    /// <summary>
    /// A refactoring processor is responsible for implementing the refactoring logic
    /// independently of the language server currently running. This is an abstraction
    /// for an automated modification of source text.
    /// </summary>
    public interface IRefactoringProcessor
    {
        /// <summary>
        /// Arguments deserialization helper. Returns an object read or parsed from
        /// given arguments array and index.
        /// </summary>
        /// <typeparam name="T">Expected type of the argument.</typeparam>
        /// <param name="arguments">Arguments array.</param>
        /// <param name="index">Argument index.</param>
        /// <param name="required">True to consider the argument as required: when missing, an ArgumentException
        /// will be thrown. False to consider the argument as optional: the default value for the argument type
        /// will be returned.</param>
        /// <returns>Instance of T, possibly null.</returns>
        static T Expect<T>(object[] arguments, int index, bool required)
        {
            if (arguments == null || index > arguments.Length - 1)
            {
                return DefaultOrThrowWhenRequired();
            }

            object argument = arguments[index];
            switch (argument)
            {
                case T result:
                    return result;
                case JToken jToken:
                    if (jToken.Type == JTokenType.Null)
                    {
                        ThrowWhenRequired();
                    }

                    return jToken.ToObject<T>();
            }

            return DefaultOrThrowWhenRequired();

            T DefaultOrThrowWhenRequired()
            {
                ThrowWhenRequired();
                return default;
            }

            void ThrowWhenRequired()
            {
                if (required)
                {
                    throw new ArgumentException("Invalid arguments for command.", nameof(arguments));
                }
            }
        }

        /// <summary>
        /// Collect and parse refactoring arguments.
        /// </summary>
        /// <param name="arguments">Untyped argument array, possibly null.</param>
        /// <returns>Non-null TextDocumentIdentifier found in args, an exception should be thrown if no identifier could be found.</returns>
        TextDocumentIdentifier PrepareRefactoring(object[] arguments);

        /// <summary>
        /// Perform validation of the refactoring target.
        /// </summary>
        /// <param name="compilationUnit">The program that should be modified by this refactoring.</param>
        void CheckTarget(CompilationUnit compilationUnit);

        /// <summary>
        /// Compute text edits corresponding to this refactoring.
        /// </summary>
        /// <param name="compilationUnit">Target of the refactoring.</param>
        /// <returns>A tuple made of a label describing the modification that has been performed and a list of TextEdits.</returns>
        (string Label, List<TextEdit> TextEdits) PerformRefactoring(CompilationUnit compilationUnit);
    }
}
