using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

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

        public static IEnumerable<TInterface> Activate<TInterface>(this Assembly assembly, params object[] parameters)
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

            if (assembly == null) yield break;

            foreach (var type in assembly.GetTypes())
            {
                if (type.IsPublic && type.IsClass && !type.IsAbstract && type.GetInterfaces().Contains(targetType))
                {
                    yield return (TInterface) Activator.CreateInstance(type, parameters);
                }
            }
        }
    }
}
