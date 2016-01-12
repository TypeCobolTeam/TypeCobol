using System;
using System.Collections.Generic;
using System.Reflection;

namespace TypeCobol.Tools
{
    class Reflection
    {

        public static bool IsTypeOf(Type type, Type iface) {
            return iface.IsAssignableFrom(type);
        }

        public static List<Type> GetTypesInNamespace<T>(Assembly assembly, string nspace) {
            var types = new List<Type>();
            foreach(var type in assembly.GetTypes()) {
                if (type.Namespace.StartsWith(nspace)) {
                    if (IsTypeOf(type, typeof(T))) types.Add(type);
                }
            }
            return types;
        }

        public static List<T> GetInstances<T>(Assembly assembly, string nspace) {
            var instances = new List<T>();
            var types = GetTypesInNamespace<T>(assembly, nspace);
            foreach (var type in types) {
                instances.Add((T)System.Activator.CreateInstance(type));
            }
            return instances;
        }

    }
}
