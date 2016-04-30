﻿using System;
using System.Collections.Generic;
using System.Reflection;

namespace TypeCobol.Tools
{
	public class Reflection {

		public static bool IsTypeOf(Type type, Type iface) {
			return iface.IsAssignableFrom(type);
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
			foreach (var type in types) {
				instances.Add((T)System.Activator.CreateInstance(type));
			}
			return instances;
		}

	}
}
