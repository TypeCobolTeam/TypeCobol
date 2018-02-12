using System;
using System.Collections.Generic;
using JetBrains.Annotations;

namespace TypeCobol.Tools {
    public static class DictionnaryHelper {
        public static void AddRangeWithoutCheckingDuplicate<T, S>(this IDictionary<T, S> source, [NotNull] IDictionary<T, S> collection) {
            if (collection == null) {
                throw new ArgumentNullException("Collection is null");
            }

            foreach (var item in collection) {
                source.Add(item.Key, item.Value);
            }
        }
    }
}