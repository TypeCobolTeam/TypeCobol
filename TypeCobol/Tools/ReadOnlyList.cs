#if NET40
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TypeCobol
{
    [Serializable]
    public class ReadOnlyList<T> : IReadOnlyList<T>
    {
        private IList _list;

        internal ReadOnlyList(IList l)
        {
            _list = l;
        }

        public ReadOnlyList()
        {
        }

        public virtual int Count
        {
            get { return _list.Count; }
        }

        public virtual bool IsReadOnly
        {
            get { return true; }
        }

        public virtual bool IsFixedSize
        {
            get { return true; }
        }

        public virtual bool IsSynchronized
        {
            get { return _list.IsSynchronized; }
        }

        public virtual Object this[int index]
        {
            get
            {
                return _list[index];
            }
            set
            {
                throw new NotSupportedException();
            }
        }

        public virtual Object SyncRoot
        {
            get { return _list.SyncRoot; }
        }

        T IReadOnlyCollection<T>.this[int index]
        {
            get { return (T)_list[index]; }
        }

        public virtual int Add(Object obj)
        {
            throw new NotSupportedException();
        }

        public virtual void Clear()
        {
            throw new NotSupportedException();
        }

        public virtual bool Contains(Object obj)
        {
            return _list.Contains(obj);
        }

        public virtual void CopyTo(Array array, int index)
        {
            _list.CopyTo(array, index);
        }

        public virtual IEnumerator GetEnumerator()
        {
            return _list.GetEnumerator();
        }

        public virtual int IndexOf(Object value)
        {
            return _list.IndexOf(value);
        }

        public virtual void Insert(int index, Object obj)
        {
            throw new NotSupportedException();
        }

        public virtual void Remove(Object value)
        {
            throw new NotSupportedException();
        }

        public virtual void RemoveAt(int index)
        {
            throw new NotSupportedException();
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return (IEnumerator<T>)this.GetEnumerator();
        }
    }
}
#endif