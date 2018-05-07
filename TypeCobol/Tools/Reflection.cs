using System;
using System.Collections.Generic;
using System.Reflection;

namespace TypeCobol.Tools {

public class Reflection {

	public static bool IsTypeOf(Type type, Type iface) {
	    if (!type.IsGenericType && !iface.IsGenericType)
	    {
	        return iface.IsAssignableFrom(type);
	    }
	    else if (type.IsGenericType && iface.IsGenericType)
	    {
	        System.Type instType = type.MakeGenericType(iface.GetGenericArguments());
	        return iface.IsAssignableFrom(instType);
	    }
	    else
	    {
	        return false;
	    }
	}

	public static List<Type> GetTypesInNamespace(string nspace, Assembly assembly = null) {
		var types = new List<Type>();
		if (assembly == null) assembly = Assembly.GetExecutingAssembly();
		foreach(var type in assembly.GetTypes()) {
			if (type.Namespace == null) continue;
			if (type.Namespace.StartsWith(nspace)) {
				types.Add(type);
			}
		}
		return types;
	}

	public static List<Type> GetTypesInNamespace<T>(string nspace, Assembly assembly = null) {
		var types = new List<Type>();
		if (assembly == null) assembly = Assembly.GetExecutingAssembly();
		foreach(var type in assembly.GetTypes()) {
			if (type.Namespace == null) continue;
			if (type.Namespace.StartsWith(nspace)) {
				if (IsTypeOf(type, typeof(T))) types.Add(type);
			}
		}
		return types;
	}

	public static List<T> GetInstances<T>(Assembly assembly, string nspace) {
		var instances = new List<T>();
		var types = GetTypesInNamespace<T>(nspace, assembly);
	    System.Type iface = typeof(T);
		foreach (var type in types) {
			if (type.IsAbstract) continue;//cannot instanciate abstract types
		    if (!type.IsGenericType && !iface.IsGenericType)
		    {
		        instances.Add((T) System.Activator.CreateInstance(type));
		    }
            else if (type.IsGenericType && iface.IsGenericType)
		    {
                System.Type instType = type.MakeGenericType(iface.GetGenericArguments());
                instances.Add((T)System.Activator.CreateInstance(instType));
            }
        }
		return instances;
	}

	public static IEnumerable<T> GetValues<T>() {
		return (T[])Enum.GetValues(typeof(T));
	}
}

}
