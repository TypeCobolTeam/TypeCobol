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

    public class ExtensionManager
    {
        private readonly List<Assembly> _loadedExtensions;

        public ExtensionManager([NotNull] List<string> extensions)
        {
            Debug.Assert(extensions != null);
            _loadedExtensions = new List<Assembly>();
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

        public List<TInterface> Create<TInterface>(params object[] parameters)
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
