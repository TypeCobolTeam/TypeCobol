#if NET40
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TypeCobol
{
    public interface IReadOnlyList<out T> : IReadOnlyCollection<T>, IEnumerable<T>, IEnumerable
    {

    }
}
#endif