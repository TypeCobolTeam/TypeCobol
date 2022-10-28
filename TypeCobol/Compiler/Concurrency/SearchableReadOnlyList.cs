using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Concurrency
{


    /// <summary>
    /// TODO it's not a real ReadOnyList, try to inherits from ReadOnlyCollection or override all *modification* method and a throw an exception.
    /// But as this List is only used thru its interface ISearchableReadOnlyList it's not a real problem.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class SearchableReadOnlyList<T> : List<T>, ISearchableReadOnlyList<T>
    {

        public SearchableReadOnlyList(ICollection<T> initialCol) : base(initialCol)
        {

        }
        public ImmutableList<TOutput> ConvertAll<TOutput>(Func<T, TOutput> converter)
        {
            throw new NotImplementedException();
        }

        public IEnumerator<T> GetEnumerator(int startIndex, bool reversed)
        {
            return new ReverseEnumerator(this, startIndex, reversed);
        }

        public int IndexOf(object item2, int initialIndex)
        {
            T item = (T) item2; //TODO check type
            IEqualityComparer<T> equalityComparer = EqualityComparer<T>.Default;

            // Force the initial index into the bounds of the current version of the list
            if (initialIndex < 0)
            {
                initialIndex = 0;
            }
            if (initialIndex >= this.Count)
            {
                initialIndex = this.Count - 1;
            }

            // First try : check if the item did not move since the first version
            T candidateItem = this[initialIndex];
            if (equalityComparer.Equals(item, candidateItem))
            {
                return initialIndex;
            }

            // If the element moved since the first version, 
            // there is a high probability it is still located "near" its initial position.
            // The value below defines what "near" means in number of lines
            int proximitySpanSize = 50;


            int minSearched = Math.Max(0, initialIndex - proximitySpanSize);
            int maxSearched = Math.Min(this.Count-1, initialIndex + proximitySpanSize);

            for (int i = minSearched; i <= maxSearched; i++)
            {
                if (equalityComparer.Equals(this[i], item))
                {
                    return i;
                }
            }

            //Iterate the whole list, start with elements before minSearched
            for (int i = 0; i < minSearched; i++)
            {
                if (equalityComparer.Equals(this[i], item))
                {
                    return i;
                }
            }

            //Iterate the whole list,continue with elements after maxSearched
            for (int i = maxSearched+1; i < this.Count; i++)
            {
                if (equalityComparer.Equals(this[i], item))
                {
                    return i;
                }
            }

            return -1;
        }



        [Serializable]
        public struct ReverseEnumerator : IEnumerator<T>, IDisposable, IEnumerator
        {
            private List<T> _list;
            private int _index;
            private int _initialIndex;
            private T _current;
            private bool _reverse;

            //Technical : As this Enumerator is meant to iterate over immutableList, there is no need to check for field _version of list.

            internal ReverseEnumerator(List<T> list, int initialIndex = 0, bool reverse = false)
            {
                this._list = list;
                this._initialIndex = initialIndex;
                this._index = initialIndex;
                this._current = default(T);
                this._reverse = reverse;
            }

            /// <summary>Releases all resources used by the <see cref="T:System.Collections.Generic.List`1.Enumerator" />.</summary>
            public void Dispose()
            {
            }

            /// <summary>Advances the enumerator to the next element of the <see cref="T:System.Collections.Generic.List`1" />.</summary>
            /// <returns>
            /// <see langword="true" /> if the enumerator was successfully advanced to the next element; <see langword="false" /> if the enumerator has passed the end of the collection.</returns>
            /// <exception cref="T:System.InvalidOperationException">The collection was modified after the enumerator was created. </exception>
            public bool MoveNext()
            {
                var list = this._list;
                if (this._index < 0 || (uint)this._index >= (uint)list.Count)
                    return this.MoveNextRare();
                this._current = list[this._index];
                this._index += _reverse ? -1 : 1;
                return true;
            }

            private bool MoveNextRare()
            {
                this._index = this._list.Count + 1;
                this._current = default(T);
                return false;
            }

            /// <summary>Gets the element at the current position of the enumerator.</summary>
            /// <returns>The element in the <see cref="T:System.Collections.Generic.List`1" /> at the current position of the enumerator.</returns>
            public T Current
            {
                get
                {
                    return this._current;
                }
            }

            object IEnumerator.Current
            {
                get
                {
                    if (this._index <= 0 || this._index >= this._list.Count + 1)
                        throw new Exception("Enumerator not initialized or past the last item");
                    return this.Current;
                }
            }

            void IEnumerator.Reset()
            {
                this._index = _initialIndex;
                this._current = default(T);
            }
        }
    }
}
