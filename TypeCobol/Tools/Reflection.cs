using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using JetBrains.Annotations;
using TypeCobol.Logging;

namespace TypeCobol.Tools
{
    public static class Reflection
    {
        public static bool IsTypeOf(Type type, Type targetType)
        {
            if (!targetType.IsGenericType)
            {
                return targetType.IsAssignableFrom(type);
            }
            
            if (type.IsGenericType && targetType.IsGenericType)
            {
                Type instType = type.MakeGenericType(targetType.GetGenericArguments());
                return targetType.IsAssignableFrom(instType);
            }

            return false;
        }
    }

    /// <summary>
    /// Provides methods to help working with custom parser extensions.
    /// </summary>
    public class ExtensionManager
    {
        private readonly List<Assembly> _loadedExtensions;

        /// <summary>
        /// Creates a new ExtensionManager. All extensions are dynamically loaded during
        /// the call. Errors are traced into LoggingSystem so a basic logger has to be registered
        /// prior calling this constructor.
        /// </summary>
        /// <param name="extensions">A non-null list of assembly file paths.</param>
        public ExtensionManager([NotNull] List<string> extensions)
        {
            Debug.Assert(extensions != null);
            _loadedExtensions = new List<Assembly>(extensions.Count);
            foreach (var extension in extensions)
            {
                try
                {
                    var assembly = Assembly.LoadFrom(extension);
                    _loadedExtensions.Add(assembly);
                }
                catch (Exception exception)
                {
                    LoggingSystem.LogException(exception);
                }
            }
        }

        /// <summary>
        /// Creates all instances of implementations found in parser extensions for the given interface type.
        /// </summary>
        /// <typeparam name="TInterface">Non-generic interface type.</typeparam>
        /// <param name="parameters">Constructor parameters to use at instantiation time.</param>
        /// <returns>List of new instances.</returns>
        public List<TInterface> Activate<TInterface>(params object[] parameters)
            where TInterface : class
        {
            Type targetType = typeof(TInterface);

            if (!targetType.IsInterface)
            {
                throw new ArgumentException($"Target type '{targetType.FullName}' must be an interface type.");
            }

            if (targetType.IsGenericType)
            {
                throw new NotSupportedException("Activation of generic types is not supported.");
            }

            List<TInterface> result = new List<TInterface>();
            foreach (var extension in _loadedExtensions)
            {
                foreach (var type in extension.GetTypes())
                {
                    if (type.IsPublic && type.IsClass && !type.IsAbstract && type.GetInterfaces().Contains(targetType))
                    {
                        try
                        {
                            var instance = (TInterface) Activator.CreateInstance(type, parameters);
                            result.Add(instance);
                        }
                        catch (Exception exception)
                        {
                            LoggingSystem.LogException(exception);
                        }
                    }
                }
            }

            return result;
        }
    }
}
