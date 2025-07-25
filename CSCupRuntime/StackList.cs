using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Runtime.Serialization;
using System.Text;
using System.Threading.Tasks;

// ==++==
// 
//   Copyright (c) Microsoft Corporation.  All rights reserved.
// 
// ==--==
/*=============================================================================
**
** Class: Stack
**
** Purpose: An array implementation of a generic stack.
**
** Date: January 28, 2003
**
=============================================================================*/
namespace CSCupRuntime
{
    using System;
    using System.Diagnostics;
    using System.Diagnostics.CodeAnalysis;

    // A simple stack of objects.  Internally it is implemented as an array,
    // so Push can be O(n).  Pop is O(1).

    [DebuggerDisplay("Count = {Count}")]
    [System.Runtime.InteropServices.ComVisible(false)]
    public class StackList<T> : IEnumerable<T>,
        System.Collections.ICollection,
        IReadOnlyCollection<T>
    {
        private T[] _array;     // Storage for stack elements
        private int _version;        // Used to keep enumerator in sync w/ collection.

        private Object _syncRoot;

        private const int _defaultCapacity = 4;
        static readonly T[] _emptyArray = new T[0];

        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Stack"]/*' />
        public StackList()
        {
            _array = _emptyArray;
            Count = 0;
            _version = 0;
        }

        // Create a stack with a specific initial capacity.  The initial capacity
        // must be a non-negative number.
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Stack1"]/*' />
        public StackList(int capacity)
        {
            Contract.Assert(capacity >= 0);
            _array = new T[capacity];
            Count = 0;
            _version = 0;
        }

        // Fills a Stack with the contents of a particular collection.  The items are
        // pushed onto the stack in the same order they are read by the enumerator.
        //
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Stack2"]/*' />
        public StackList(IEnumerable<T> collection)
        {
            Contract.Assert(collection != null, "collection null");
            

            ICollection<T> c = collection as ICollection<T>;
            if (c != null)
            {
                int count = c.Count;
                _array = new T[count];
                c.CopyTo(_array, 0);
                Count = count;
            }
            else
            {
                Count = 0;
                _array = new T[_defaultCapacity];

                using (IEnumerator<T> en = collection.GetEnumerator())
                {
                    while (en.MoveNext())
                    {
                        Push(en.Current);
                    }
                }
            }
        }

        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Count"]/*' />
        public int Count { get; private set; }

        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.IsSynchronized"]/*' />
        bool System.Collections.ICollection.IsSynchronized
        {
            get { return false; }
        }

        
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.SyncRoot"]/*' />
        Object System.Collections.ICollection.SyncRoot
        {
            get
            {
                if (_syncRoot == null)
                {
                    System.Threading.Interlocked.CompareExchange<Object>(ref _syncRoot, new Object(), null);
                }
                return _syncRoot;
            }
        }

        // Removes all Objects from the Stack.
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Clear"]/*' />
        public void Clear()
        {
            Array.Clear(_array, 0, Count); // Don't need to doc this but we clear the elements so that the gc can reclaim the references.
            Count = 0;
            _version++;
        }

        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Contains"]/*' />
        public bool Contains(T item)
        {
            int count = Count;

            EqualityComparer<T> c = EqualityComparer<T>.Default;
            while (count-- > 0)
            {
                if (item == null)
                {
                    if (_array[count] == null)
                        return true;
                }
                else if (_array[count] != null && c.Equals(_array[count], item))
                {
                    return true;
                }
            }
            return false;
        }

        // Copies the stack into an array.
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.CopyTo"]/*' />
        public void CopyTo(T[] array, int arrayIndex)
        {
            Contract.Assert(array != null, "array null");
            Contract.Assert(arrayIndex >= 0 && arrayIndex < array.Length, "ArgumentOutOfRange_NeedNonNegNum");
            Contract.Assert(array.Length - arrayIndex >= Count, "Argument_InvalidOffLen");
            

            Array.Copy(_array, 0, array, arrayIndex, Count);
            Array.Reverse(array, arrayIndex, Count);
        }

        void System.Collections.ICollection.CopyTo(Array array, int arrayIndex)
        {
            Contract.Assert(array!=null, "array null");
            Contract.Assert(array.Rank == 1, "Arg_RankMultiDimNotSupported");
            Contract.Assert(array.GetLowerBound(0) == 0, "Arg_NonZeroLowerBound");
            Contract.Assert(arrayIndex >= 0 && arrayIndex < array.Length, "ArgumentOutOfRange_NeedNonNegNum");
            Contract.Assert(array.Length - arrayIndex >= Count, "Argument_InvalidOffLen");


                Array.Copy(_array, 0, array, arrayIndex, Count);
                Array.Reverse(array, arrayIndex, Count);

        }

        // Returns an IEnumerator for this Stack.
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.GetEnumerator"]/*' />
        public Enumerator GetEnumerator()
        {
            return new Enumerator(this);
        }

        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.IEnumerable.GetEnumerator"]/*' />
        /// <internalonly/>
        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return new Enumerator(this);
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return new Enumerator(this);
        }

        public void TrimExcess()
        {
            int threshold = (int)(((double)_array.Length) * 0.9);
            if (Count < threshold)
            {
                T[] newarray = new T[Count];
                Array.Copy(_array, 0, newarray, 0, Count);
                _array = newarray;
                _version++;
            }
        }

        public T ElementAtFromBottom(int index)
        {
            Contract.Assert(Count != 0, "InvalidOperation_EmptyStack");
            Contract.Assert(index >= 0 && index < _array.Length, "ArgumentOutOfRange_NeedNonNegNum");

            return _array[index];
        }
        public T ElementAtFromTop(int index)
        {
            Contract.Assert(Count != 0, "InvalidOperation_EmptyStack");
            Contract.Assert(index >= 0 && index < _array.Length, "ArgumentOutOfRange_NeedNonNegNum");

            return _array[Count - index - 1];
        }

        // Returns the top object on the stack without removing it.  If the stack
        // is empty, Peek throws an InvalidOperationException.
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Peek"]/*' />
        public T Peek()
        {
            Contract.Assert(Count != 0, "InvalidOperation_EmptyStack");
            return _array[Count - 1];
        }

        // Pops an item from the top of the stack.  If the stack is empty, Pop
        // throws an InvalidOperationException.
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Pop"]/*' />
        public T Pop()
        {
            Contract.Assert(Count!=0, "InvalidOperation_EmptyStack");

            _version++;
            T item = _array[--Count];
            _array[Count] = default(T);     // Free memory quicker.
            return item;
        }

        // Pushes an item to the top of the stack.
        // 
        /// <include file='doc\Stack.uex' path='docs/doc[@for="Stack.Push"]/*' />
        public void Push(T item)
        {
            if (Count == _array.Length)
            {
                T[] newArray = new T[(_array.Length == 0) ? _defaultCapacity : 2 * _array.Length];
                Array.Copy(_array, 0, newArray, 0, Count);
                _array = newArray;
            }
            _array[Count++] = item;
            _version++;
        }

        // Copies the Stack to an array, in the same order Pop would return the items.
        public T[] ToArray()
        {
            T[] objArray = new T[Count];
            int i = 0;
            while (i < Count)
            {
                objArray[i] = _array[Count - i - 1];
                i++;
            }
            return objArray;
        }

        /// <include file='doc\Stack.uex' path='docs/doc[@for="StackEnumerator"]/*' />

        [SuppressMessage("Microsoft.Performance", "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes", Justification = "not an expected scenario")]
        public struct Enumerator : IEnumerator<T>,
            System.Collections.IEnumerator
        {
            private StackList<T> _stack;
            private int _index;
            private int _version;
            private T currentElement;

            internal Enumerator(StackList<T> stack)
            {
                _stack = stack;
                _version = _stack._version;
                _index = -2;
                currentElement = default(T);
            }

            /// <include file='doc\Stack.uex' path='docs/doc[@for="StackEnumerator.Dispose"]/*' />
            public void Dispose()
            {
                _index = -1;
            }

            /// <include file='doc\Stack.uex' path='docs/doc[@for="StackEnumerator.MoveNext"]/*' />
            public bool MoveNext()
            {
                bool retval;
                Contract.Assert(_version == _stack._version, "InvalidOperation_EnumFailedVersion");
                if (_index == -2)
                {  // First call to enumerator.
                    _index = _stack.Count - 1;
                    retval = (_index >= 0);
                    if (retval)
                        currentElement = _stack._array[_index];
                    return retval;
                }
                if (_index == -1)
                {  // End of enumeration.
                    return false;
                }

                retval = (--_index >= 0);
                if (retval)
                    currentElement = _stack._array[_index];
                else
                    currentElement = default(T);
                return retval;
            }

            /// <include file='doc\Stack.uex' path='docs/doc[@for="StackEnumerator.Current"]/*' />
            public T Current
            {
                get {
                    Contract.Assert(_index != -2, "Enum not started");
                    Contract.Assert(_index != -1, "Enum ended");
                    return currentElement;
                }
            }

            Object System.Collections.IEnumerator.Current
            {
                get {
                    Contract.Assert(_index != -2, "Enum not started");
                    Contract.Assert(_index != -1, "Enum ended");
                    return currentElement;
                }
            }

            void System.Collections.IEnumerator.Reset() {
                Contract.Assert(_version == _stack._version);
                _index = -2;
                currentElement = default(T);
            }
        }
    }
   
}