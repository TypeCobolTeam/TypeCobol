// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// Inline copy from Microsoft System.Collections.Immutable package.
// Extract of all the classes necessary to compile ImmutableList.
// Copy from the Github master branch of dotnet/corefx - 16/08/2015.
// The idea is to trim down this class to the only methods we use in the compiler.
// Then it should be easier to migrate the code base to Java, even if this
// class is heavily optimized for the CLR (with value types and compiler hints).

// DO NOT FORGET to inlcude credit to Microsoft and a copy of the MIT license 
// in any distribution of the compiler. 

using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    ///  A list of elements that can only be modified by creating a new instance of the list.
    /// </summary>
    /// <typeparam name="T">The type of element stored in the list.</typeparam>
    /// <remarks>
    /// Mutations on this list generate new lists.  Incremental changes to a list share as much memory as possible with the prior versions of a list,
    /// while allowing garbage collection to clean up any unique list data that is no longer being referenced.
    /// </remarks>
    [SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix", Justification = "Ignored")]
    public interface IImmutableList<T> : IReadOnlyList<T>
    {
        /// <summary>
        /// Gets an empty list that retains the same sort or unordered semantics that this instance has.
        /// </summary>
        [Pure]
        IImmutableList<T> Clear();

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// first occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
        /// that starts at the specified index and contains the specified number of elements.
        /// </summary>
        /// <param name="item">
        /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="index">
        /// The zero-based starting index of the search. 0 (zero) is valid in an empty
        /// list.
        /// </param>
        /// <param name="count">
        /// The number of elements in the section to search.
        /// </param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of item within the range of
        /// elements in the <see cref="ImmutableList{T}"/> that starts at index and
        /// contains count number of elements, if found; otherwise, -1.
        /// </returns>
        [Pure]
        int IndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer);

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// last occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
        /// that contains the specified number of elements and ends at the specified
        /// index.
        /// </summary>
        /// <param name="item">
        /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="index">The starting position of the search. The search proceeds from <paramref name="index"/> toward the beginning of this instance.</param>
        /// <param name="count">The number of elements in the section to search.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of <paramref name="item"/> within the range of elements
        /// in the <see cref="ImmutableList{T}"/> that contains <paramref name="count"/> number of elements
        /// and ends at <paramref name="index"/>, if found; otherwise, -1.
        /// </returns>
        [Pure]
        int LastIndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer);

        /// <summary>
        /// Adds the specified value to this list.
        /// </summary>
        /// <param name="value">The value to add.</param>
        /// <returns>A new list with the element added.</returns>
        [Pure]
        IImmutableList<T> Add(T value);

        /// <summary>
        /// Adds the specified values to this list.
        /// </summary>
        /// <param name="items">The values to add.</param>
        /// <returns>A new list with the elements added.</returns>
        [Pure]
        IImmutableList<T> AddRange(IEnumerable<T> items);

        /// <summary>
        /// Inserts the specified value at the specified index.
        /// </summary>
        /// <param name="index">The index at which to insert the value.</param>
        /// <param name="element">The element to insert.</param>
        /// <returns>The new immutable list.</returns>
        [Pure]
        IImmutableList<T> Insert(int index, T element);

        /// <summary>
        /// Inserts the specified values at the specified index.
        /// </summary>
        /// <param name="index">The index at which to insert the value.</param>
        /// <param name="items">The elements to insert.</param>
        /// <returns>The new immutable list.</returns>
        [Pure]
        IImmutableList<T> InsertRange(int index, IEnumerable<T> items);

        /// <summary>
        /// Removes the specified value from this list.
        /// </summary>
        /// <param name="value">The value to remove.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>A new list with the element removed, or this list if the element is not in this list.</returns>
        [Pure]
        IImmutableList<T> Remove(T value, IEqualityComparer<T> equalityComparer);

        /// <summary>
        /// Removes all the elements that match the conditions defined by the specified
        /// predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to remove.
        /// </param>
        /// <returns>
        /// The new list.
        /// </returns>
        [Pure]
        IImmutableList<T> RemoveAll(Predicate<T> match);

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="items">The items to remove if matches are found in this list.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>
        /// A new list with the elements removed.
        /// </returns>
        [Pure]
        IImmutableList<T> RemoveRange(IEnumerable<T> items, IEqualityComparer<T> equalityComparer);

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="index">The starting index to begin removal.</param>
        /// <param name="count">The number of elements to remove.</param>
        /// <returns>
        /// A new list with the elements removed.
        /// </returns>
        [Pure]
        IImmutableList<T> RemoveRange(int index, int count);

        /// <summary>
        /// Removes the element at the specified index.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <returns>A new list with the elements removed.</returns>
        [Pure]
        IImmutableList<T> RemoveAt(int index);

        /// <summary>
        /// Replaces an element in the list at a given position with the specified element.
        /// </summary>
        /// <param name="index">The position in the list of the element to replace.</param>
        /// <param name="value">The element to replace the old element with.</param>
        /// <returns>The new list -- even if the value being replaced is equal to the new value for that position.</returns>
        [Pure]
        IImmutableList<T> SetItem(int index, T value);

        /// <summary>
        /// Replaces the first equal element in the list with the specified element.
        /// </summary>
        /// <param name="oldValue">The element to replace.</param>
        /// <param name="newValue">The element to replace the old element with.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>The new list -- even if the value being replaced is equal to the new value for that position.</returns>
        /// <exception cref="ArgumentException">Thrown when the old value does not exist in the list.</exception>
        [Pure]
        IImmutableList<T> Replace(T oldValue, T newValue, IEqualityComparer<T> equalityComparer);
    }

    /// <summary>
    /// An interface that describes the methods that the <see cref="ImmutableList{T}"/> and <see cref="ImmutableList{T}.Builder"/> types have in common.
    /// </summary>
    /// <typeparam name="T">The type of element in the collection.</typeparam>
    internal interface IImmutableListQueries<T> : ISearchableReadOnlyList<T>
    {
        /// <summary>
        /// Creates a shallow copy of a range of elements in the source <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="index">
        /// The zero-based <see cref="ImmutableList{T}"/> index at which the range
        /// starts.
        /// </param>
        /// <param name="count">
        /// The number of elements in the range.
        /// </param>
        /// <returns>
        /// A shallow copy of a range of elements in the source <see cref="ImmutableList{T}"/>.
        /// </returns>
        ImmutableList<T> GetRange(int index, int count);

        /// <summary>
        /// Copies the entire <see cref="ImmutableList{T}"/> to a compatible one-dimensional
        /// array, starting at the beginning of the target array.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements
        /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
        /// zero-based indexing.
        /// </param>
        void CopyTo(T[] array);

        /// <summary>
        /// Copies the entire <see cref="ImmutableList{T}"/> to a compatible one-dimensional
        /// array, starting at the specified index of the target array.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements
        /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
        /// zero-based indexing.
        /// </param>
        /// <param name="arrayIndex">
        /// The zero-based index in <paramref name="array"/> at which copying begins.
        /// </param>
        void CopyTo(T[] array, int arrayIndex);

        /// <summary>
        /// Copies a range of elements from the <see cref="ImmutableList{T}"/> to
        /// a compatible one-dimensional array, starting at the specified index of the
        /// target array.
        /// </summary>
        /// <param name="index">
        /// The zero-based index in the source <see cref="ImmutableList{T}"/> at
        /// which copying begins.
        /// </param>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements
        /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
        /// zero-based indexing.
        /// </param>
        /// <param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param>
        /// <param name="count">The number of elements to copy.</param>
        void CopyTo(int index, T[] array, int arrayIndex, int count);
        
        /// <summary>
        /// Retrieves all the elements that match the conditions defined by the specified
        /// predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to search for.
        /// </param>
        /// <returns>
        /// A <see cref="ImmutableList{T}"/> containing all the elements that match
        /// the conditions defined by the specified predicate, if found; otherwise, an
        /// empty <see cref="ImmutableList{T}"/>.
        /// </returns>
        ImmutableList<T> FindAll(Predicate<T> match);

        /// <summary>
        /// Searches the entire sorted <see cref="IReadOnlyList{T}"/> for an element
        /// using the default comparer and returns the zero-based index of the element.
        /// </summary>
        /// <param name="item">The object to locate. The value can be null for reference types.</param>
        /// <returns>
        /// The zero-based index of <paramref name="item"/> in the sorted <see cref="IReadOnlyList{T}"/>,
        /// if <paramref name="item"/> is found; otherwise, a negative number that is the bitwise complement
        /// of the index of the next element that is larger than <paramref name="item"/> or, if there is
        /// no larger element, the bitwise complement of <see cref="IReadOnlyCollection{T}.Count"/>.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// The default comparer <see cref="Comparer{T}.Default"/> cannot
        /// find an implementation of the <see cref="IComparable{T}"/> generic interface or
        /// the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
        /// </exception>
        int BinarySearch(T item);

        /// <summary>
        ///  Searches the entire sorted <see cref="IReadOnlyList{T}"/> for an element
        ///  using the specified comparer and returns the zero-based index of the element.
        /// </summary>
        /// <param name="item">The object to locate. The value can be null for reference types.</param>
        /// <param name="comparer">
        /// The <see cref="IComparer{T}"/> implementation to use when comparing
        /// elements.-or-null to use the default comparer <see cref="Comparer{T}.Default"/>.
        /// </param>
        /// <returns>
        /// The zero-based index of <paramref name="item"/> in the sorted <see cref="IReadOnlyList{T}"/>,
        /// if <paramref name="item"/> is found; otherwise, a negative number that is the bitwise complement
        /// of the index of the next element that is larger than <paramref name="item"/> or, if there is
        /// no larger element, the bitwise complement of <see cref="IReadOnlyCollection{T}.Count"/>.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// <paramref name="comparer"/> is null, and the default comparer <see cref="Comparer{T}.Default"/>
        /// cannot find an implementation of the <see cref="IComparable{T}"/> generic interface
        /// or the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
        /// </exception>
        int BinarySearch(T item, IComparer<T> comparer);

        /// <summary>
        /// Searches a range of elements in the sorted <see cref="IReadOnlyList{T}"/>
        /// for an element using the specified comparer and returns the zero-based index
        /// of the element.
        /// </summary>
        /// <param name="index">The zero-based starting index of the range to search.</param>
        /// <param name="count"> The length of the range to search.</param>
        /// <param name="item">The object to locate. The value can be null for reference types.</param>
        /// <param name="comparer">
        /// The <see cref="IComparer{T}"/> implementation to use when comparing
        /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
        /// </param>
        /// <returns>
        /// The zero-based index of <paramref name="item"/> in the sorted <see cref="IReadOnlyList{T}"/>,
        /// if <paramref name="item"/> is found; otherwise, a negative number that is the bitwise complement
        /// of the index of the next element that is larger than <paramref name="item"/> or, if there is
        /// no larger element, the bitwise complement of <see cref="IReadOnlyCollection{T}.Count"/>.
        /// </returns>
        /// <exception cref="ArgumentOutOfRangeException">
        /// <paramref name="index"/> is less than 0.-or-<paramref name="count"/> is less than 0.
        /// </exception>
        /// <exception cref="ArgumentException">
        /// <paramref name="index"/> and <paramref name="count"/> do not denote a valid range in the <see cref="IReadOnlyList{T}"/>.
        /// </exception>
        /// <exception cref="InvalidOperationException">
        /// <paramref name="comparer"/> is null, and the default comparer <see cref="Comparer{T}.Default"/>
        /// cannot find an implementation of the <see cref="IComparable{T}"/> generic interface
        /// or the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
        /// </exception>
        int BinarySearch(int index, int count, T item, IComparer<T> comparer);
    }

    /*UPDATE to MICROSOFT code ->*/ public interface ISearchableReadOnlyList<out T> : IReadOnlyList<T>
    {
        /// <summary>
        /// Converts the elements in the current <see cref="ImmutableList{T}"/> to
        /// another type, and returns a list containing the converted elements.
        /// </summary>
        /// <param name="converter">
        /// A <see cref="Func{T, TResult}"/> delegate that converts each element from
        /// one type to another type.
        /// </param>
        /// <typeparam name="TOutput">
        /// The type of the elements of the target array.
        /// </typeparam>
        /// <returns>
        /// A <see cref="ImmutableList{T}"/> of the target type containing the converted
        /// elements from the current <see cref="ImmutableList{T}"/>.
        /// </returns>
        ImmutableList<TOutput> ConvertAll<TOutput>(Func<T, TOutput> converter);

        /// <summary>
        /// Performs the specified action on each element of the list.
        /// </summary>
        /// <param name="action">The <see cref="Action{T}"/> delegate to perform on each element of the list.</param>
        void ForEach(Action<T> action);
        
        /// <summary>
        /// Determines whether the <see cref="ImmutableList{T}"/> contains elements
        /// that match the conditions defined by the specified predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to search for.
        /// </param>
        /// <returns>
        /// true if the <see cref="ImmutableList{T}"/> contains one or more elements
        /// that match the conditions defined by the specified predicate; otherwise,
        /// false.
        /// </returns>
        bool Exists(Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the first occurrence within the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The first element that matches the conditions defined by the specified predicate,
        /// if found; otherwise, the default value for type <typeparamref name="T"/>.
        /// </returns>
        T Find(Predicate<T> match);
        
        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the first occurrence within
        /// the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        int FindIndex(Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the first occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that extends
        /// from the specified index to the last element.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the search.</param>
        /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element to search for.</param>
        /// <returns>
        /// The zero-based index of the first occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        int FindIndex(int startIndex, Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the first occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that starts
        /// at the specified index and contains the specified number of elements.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the search.</param>
        /// <param name="count">The number of elements in the section to search.</param>
        /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element to search for.</param>
        /// <returns>
        /// The zero-based index of the first occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        int FindIndex(int startIndex, int count, Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the last occurrence within the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The last element that matches the conditions defined by the specified predicate,
        /// if found; otherwise, the default value for type <typeparamref name="T"/>.
        /// </returns>
        T FindLast(Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the last occurrence within
        /// the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        int FindLastIndex(Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the last occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that extends
        /// from the first element to the specified index.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the backward search.</param>
        /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.</param>
        /// <returns>
        /// The zero-based index of the last occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        int FindLastIndex(int startIndex, Predicate<T> match);

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the last occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that contains
        /// the specified number of elements and ends at the specified index.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the backward search.</param>
        /// <param name="count">The number of elements in the section to search.</param>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        int FindLastIndex(int startIndex, int count, Predicate<T> match);

        /// <summary>
        /// Determines whether every element in the <see cref="ImmutableList{T}"/>
        /// matches the conditions defined by the specified predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions to check against
        /// the elements.
        /// </param>
        /// <returns>
        /// true if every element in the <see cref="ImmutableList{T}"/> matches the
        /// conditions defined by the specified predicate; otherwise, false. If the list
        /// has no elements, the return value is true.
        /// </returns>
        bool TrueForAll(Predicate<T> match);        

        // --- START of EXTENSION to MICROSOFT code ---
        // --- Methods added for TypeCobol ---

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <param name="startIndex">The index of the first element to enumerate.</param>
        /// <param name="count">The number of elements in this collection.</param>
        /// <param name="reversed"><c>true</c> if the list should be enumerated in reverse order.</param>
        IEnumerator<T> GetEnumerator(int startIndex, int count, bool reversed);

        /// <summary>
        /// Searches for the specified object around the initial index of the object 
        /// in the first version of the immutable list.
        /// This method is useful only when the two following conditions are true :
        /// - the searched item is present only once in the list
        /// - the probability that the searched item did not move far from its initial position is very high
        /// </summary>
        /// <param name="item">
        /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="initialIndex">
        /// The zero-based index of the serached object in the first version of the immutable list.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of <paramref name="item"/> within the range of
        /// elements in the <see cref="ImmutableList{T}"/> that starts at <paramref name="index"/> and
        /// contains <paramref name="count"/> number of elements, if found; otherwise, -1.
        /// </returns>
        int IndexOf(object item, int initialIndex);

        // --- END of EXTENSION to MICROSOFT code ---
    }

    /// <summary>
    /// Describes an ordered collection of elements.
    /// </summary>
    /// <typeparam name="T">The type of element in the collection.</typeparam>
    internal interface IOrderedCollection<out T> : IEnumerable<T>
    {
        /// <summary>
        /// Gets the number of elements in the collection.
        /// </summary>
        int Count { get; }

        /// <summary>
        /// Gets the element in the collection at a given index.
        /// </summary>
        T this[int index] { get; }
    }

    /// <summary>
    /// An interface that must be implemented by collections that want to avoid
    /// boxing their own enumerators when using the 
    /// <see cref="ImmutableExtensions.GetEnumerableDisposable{T, TEnumerator}(IEnumerable{T})"/>
    /// method.
    /// </summary>
    /// <typeparam name="T">The type of value to be enumerated.</typeparam>
    /// <typeparam name="TEnumerator">The type of the enumerator struct.</typeparam>
    internal interface IStrongEnumerable<out T, TEnumerator>
        where TEnumerator : struct, IStrongEnumerator<T>
    {
        /// <summary>
        /// Gets the strongly-typed enumerator.
        /// </summary>
        /// <returns></returns>
        TEnumerator GetEnumerator();
    }

    /// <summary>
    /// An <see cref="IEnumerator{T}"/>-like interface that does not derive from <see cref="IDisposable"/>.
    /// </summary>
    /// <typeparam name="T">The type of value to be enumerated.</typeparam>
    /// <remarks>
    /// This interface is useful because some enumerator struct types do not want to implement
    /// <see cref="IDisposable"/> since it increases the size of the generated code in foreach.
    /// </remarks>
    internal interface IStrongEnumerator<T>
    {
        /// <summary>
        /// Returns the current element.
        /// </summary>
        T Current { get; }

        /// <summary>
        /// Advances to the next element.
        /// </summary>
        bool MoveNext();
    }

    /// <summary>
    /// An interface for binary tree nodes that allow our common enumerator to walk the graph.
    /// </summary>
    internal interface IBinaryTree
    {
        /// <summary>
        /// Gets the depth of the tree below this node.
        /// </summary>
        int Height { get; }

        /// <summary>
        /// Gets a value indicating whether this node is empty.
        /// </summary>
        bool IsEmpty { get; }

        /// <summary>
        /// Gets the number of non-empty nodes at this node and below.
        /// </summary>
        /// <exception cref="NotSupportedException">Thrown if the implementation does not store this value at the node.</exception>
        int Count { get; }
        /// <summary>
        /// Gets the left branch of this node.
        /// </summary>
        IBinaryTree Left { get; }

        /// <summary>
        /// Gets the right branch of this node.
        /// </summary>
        IBinaryTree Right { get; }
    }

    /// <summary>
    /// An interface for binary tree nodes that allow our common enumerator to walk the graph.
    /// </summary>
    /// <typeparam name="T">The type of value for each node.</typeparam>
    internal interface IBinaryTree<out T> : IBinaryTree
    {
        /// <summary>
        /// Gets the value represented by the current node.
        /// </summary>
        T Value { get; }

        /// <summary>
        /// Gets the left branch of this node.
        /// </summary>
        new IBinaryTree<T> Left { get; }

        /// <summary>
        /// Gets the right branch of this node.
        /// </summary>
        new IBinaryTree<T> Right { get; }
    }


    /// <summary>
    /// A set of initialization methods for instances of <see cref="ImmutableList{T}"/>.
    /// </summary>
    public static class ImmutableList
    {
        /// <summary>
        /// Returns an empty collection.
        /// </summary>
        /// <typeparam name="T">The type of items stored by the collection.</typeparam>
        /// <returns>The immutable collection.</returns>
        [Pure]
        public static ImmutableList<T> Create<T>()
        {
            return ImmutableList<T>.Empty;
        }

        /// <summary>
        /// Creates a new immutable collection prefilled with the specified item.
        /// </summary>
        /// <typeparam name="T">The type of items stored by the collection.</typeparam>
        /// <param name="item">The item to prepopulate.</param>
        /// <returns>The new immutable collection.</returns>
        [Pure]
        public static ImmutableList<T> Create<T>(T item)
        {
            return ImmutableList<T>.Empty.Add(item);
        }

        /// <summary>
        /// Creates a new immutable collection prefilled with the specified items.
        /// </summary>
        /// <typeparam name="T">The type of items stored by the collection.</typeparam>
        /// <param name="items">The items to prepopulate.</param>
        /// <returns>The new immutable collection.</returns>
        [Pure]
        public static ImmutableList<T> CreateRange<T>(IEnumerable<T> items)
        {
            return ImmutableList<T>.Empty.AddRange(items);
        }

        /// <summary>
        /// Creates a new immutable collection prefilled with the specified items.
        /// </summary>
        /// <typeparam name="T">The type of items stored by the collection.</typeparam>
        /// <param name="items">The items to prepopulate.</param>
        /// <returns>The new immutable collection.</returns>
        [Pure]
        public static ImmutableList<T> Create<T>(params T[] items)
        {
            return ImmutableList<T>.Empty.AddRange(items);
        }

        /// <summary>
        /// Creates a new immutable list builder.
        /// </summary>
        /// <typeparam name="T">The type of items stored by the collection.</typeparam>
        /// <returns>The immutable collection builder.</returns>
        [Pure]
        public static ImmutableList<T>.Builder CreateBuilder<T>()
        {
            return Create<T>().ToBuilder();
        }

        /// <summary>
        /// Enumerates a sequence exactly once and produces an immutable list of its contents.
        /// </summary>
        /// <typeparam name="TSource">The type of element in the sequence.</typeparam>
        /// <param name="source">The sequence to enumerate.</param>
        /// <returns>An immutable list.</returns>
        [Pure]
        public static ImmutableList<TSource> ToImmutableList<TSource>(this IEnumerable<TSource> source)
        {
            var existingList = source as ImmutableList<TSource>;
            if (existingList != null)
            {
                return existingList;
            }

            return ImmutableList<TSource>.Empty.AddRange(source);
        }

        /// <summary>
        /// Replaces the first equal element in the list with the specified element.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="oldValue">The element to replace.</param>
        /// <param name="newValue">The element to replace the old element with.</param>
        /// <returns>The new list -- even if the value being replaced is equal to the new value for that position.</returns>
        /// <exception cref="ArgumentException">Thrown when the old value does not exist in the list.</exception>
        [Pure]
        public static IImmutableList<T> Replace<T>(this IImmutableList<T> list, T oldValue, T newValue)
        {
            Requires.NotNull(list, "list");
            return list.Replace(oldValue, newValue, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Removes the specified value from this list.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="value">The value to remove.</param>
        /// <returns>A new list with the element removed, or this list if the element is not in this list.</returns>
        [Pure]
        public static IImmutableList<T> Remove<T>(this IImmutableList<T> list, T value)
        {
            Requires.NotNull(list, "list");
            return list.Remove(value, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="items">The items to remove if matches are found in this list.</param>
        /// <returns>
        /// A new list with the elements removed.
        /// </returns>
        [Pure]
        public static IImmutableList<T> RemoveRange<T>(this IImmutableList<T> list, IEnumerable<T> items)
        {
            Requires.NotNull(list, "list");
            return list.RemoveRange(items, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// first occurrence within the <see cref="IImmutableList{T}"/>.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of item within the range of
        /// elements in the <see cref="IImmutableList{T}"/> that extends from index
        /// to the last element, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int IndexOf<T>(this IImmutableList<T> list, T item)
        {
            Requires.NotNull(list, "list");
            return list.IndexOf(item, 0, list.Count, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// first occurrence within the <see cref="IImmutableList{T}"/>.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="equalityComparer">The equality comparer to use in the search.</param>
        /// <returns>
        /// The zero-based index of the first occurrence of item within the range of
        /// elements in the <see cref="IImmutableList{T}"/> that extends from index
        /// to the last element, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int IndexOf<T>(this IImmutableList<T> list, T item, IEqualityComparer<T> equalityComparer)
        {
            Requires.NotNull(list, "list");
            return list.IndexOf(item, 0, list.Count, equalityComparer);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// first occurrence within the range of elements in the <see cref="IImmutableList{T}"/>
        /// that extends from the specified index to the last element.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="startIndex">
        /// The zero-based starting index of the search. 0 (zero) is valid in an empty
        /// list.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of item within the range of
        /// elements in the <see cref="IImmutableList{T}"/> that extends from index
        /// to the last element, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int IndexOf<T>(this IImmutableList<T> list, T item, int startIndex)
        {
            Requires.NotNull(list, "list");
            return list.IndexOf(item, startIndex, list.Count - startIndex, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// first occurrence within the range of elements in the <see cref="IImmutableList{T}"/>
        /// that extends from the specified index to the last element.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="startIndex">
        /// The zero-based starting index of the search. 0 (zero) is valid in an empty
        /// list.
        /// </param>
        /// <param name="count">
        /// The number of elements in the section to search.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of item within the range of
        /// elements in the <see cref="IImmutableList{T}"/> that extends from index
        /// to the last element, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int IndexOf<T>(this IImmutableList<T> list, T item, int startIndex, int count)
        {
            Requires.NotNull(list, "list");
            return list.IndexOf(item, startIndex, count, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// last occurrence within the entire <see cref="IImmutableList{T}"/>.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of item within the entire the
        /// <see cref="IImmutableList{T}"/>, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int LastIndexOf<T>(this IImmutableList<T> list, T item)
        {
            Requires.NotNull(list, "list");

            if (list.Count == 0)
            {
                // Avoid argument out of range exceptions.
                return -1;
            }

            return list.LastIndexOf(item, list.Count - 1, list.Count, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// last occurrence within the entire <see cref="IImmutableList{T}"/>.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="equalityComparer">The equality comparer to use in the search.</param>
        /// <returns>
        /// The zero-based index of the last occurrence of item within the entire the
        /// <see cref="IImmutableList{T}"/>, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int LastIndexOf<T>(this IImmutableList<T> list, T item, IEqualityComparer<T> equalityComparer)
        {
            Requires.NotNull(list, "list");

            if (list.Count == 0)
            {
                // Avoid argument out of range exceptions.
                return -1;
            }

            return list.LastIndexOf(item, list.Count - 1, list.Count, equalityComparer);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// last occurrence within the range of elements in the <see cref="IImmutableList{T}"/>
        /// that extends from the first element to the specified index.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="startIndex">
        /// The zero-based starting index of the backward search.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of item within the range of elements
        /// in the <see cref="IImmutableList{T}"/> that extends from the first element
        /// to index, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int LastIndexOf<T>(this IImmutableList<T> list, T item, int startIndex)
        {
            Requires.NotNull(list, "list");

            if (list.Count == 0 && startIndex == 0)
            {
                return -1;
            }

            return list.LastIndexOf(item, startIndex, startIndex + 1, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// last occurrence within the range of elements in the <see cref="IImmutableList{T}"/>
        /// that extends from the first element to the specified index.
        /// </summary>
        /// <param name="list">The list to search.</param>
        /// <param name="item">
        /// The object to locate in the <see cref="IImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="startIndex">
        /// The zero-based starting index of the backward search.
        /// </param>
        /// <param name="count">
        /// The number of elements in the section to search.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of item within the range of elements
        /// in the <see cref="IImmutableList{T}"/> that extends from the first element
        /// to index, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public static int LastIndexOf<T>(this IImmutableList<T> list, T item, int startIndex, int count)
        {
            Requires.NotNull(list, "list");
            return list.LastIndexOf(item, startIndex, count, EqualityComparer<T>.Default);
        }
    }


    /// <summary>
    /// An immutable list implementation.
    /// </summary>
    /// <typeparam name="T">The type of elements in the set.</typeparam>
    [DebuggerDisplay("Count = {Count}")]
    [DebuggerTypeProxy(typeof(ImmutableListDebuggerProxy<>))]
    public sealed partial class ImmutableList<T> : IImmutableList<T>, IList<T>, IList, IOrderedCollection<T>, IImmutableListQueries<T>, IStrongEnumerable<T, ImmutableList<T>.Enumerator>
    {
        /// <summary>
        /// An empty immutable list.
        /// </summary>
        [SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly ImmutableList<T> Empty = new ImmutableList<T>();

        /// <summary>
        /// The root node of the AVL tree that stores this set.
        /// </summary>
        private readonly Node _root;

        /// <summary>
        /// Initializes a new instance of the <see cref="ImmutableList{T}"/> class.
        /// </summary>
        internal ImmutableList()
        {
            _root = Node.EmptyNode;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ImmutableList{T}"/> class.
        /// </summary>
        /// <param name="root">The root of the AVL tree with the contents of this set.</param>
        private ImmutableList(Node root)
        {
            Requires.NotNull(root, "root");

            root.Freeze();
            _root = root;
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        public ImmutableList<T> Clear()
        {
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().IsEmpty);
            return Empty;
        }

        /// <summary>
        /// Searches the entire sorted <see cref="ImmutableList{T}"/> for an element
        /// using the default comparer and returns the zero-based index of the element.
        /// </summary>
        /// <param name="item">The object to locate. The value can be null for reference types.</param>
        /// <returns>
        /// The zero-based index of item in the sorted <see cref="ImmutableList{T}"/>,
        /// if item is found; otherwise, a negative number that is the bitwise complement
        /// of the index of the next element that is larger than item or, if there is
        /// no larger element, the bitwise complement of <see cref="ImmutableList{T}.Count"/>.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// The default comparer <see cref="Comparer{T}.Default"/> cannot
        /// find an implementation of the <see cref="IComparable{T}"/> generic interface or
        /// the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
        /// </exception>
        public int BinarySearch(T item)
        {
            return this.BinarySearch(item, null);
        }

        /// <summary>
        ///  Searches the entire sorted <see cref="ImmutableList{T}"/> for an element
        ///  using the specified comparer and returns the zero-based index of the element.
        /// </summary>
        /// <param name="item">The object to locate. The value can be null for reference types.</param>
        /// <param name="comparer">
        /// The <see cref="IComparer{T}"/> implementation to use when comparing
        /// elements.-or-null to use the default comparer <see cref="Comparer{T}.Default"/>.
        /// </param>
        /// <returns>
        /// The zero-based index of item in the sorted <see cref="ImmutableList{T}"/>,
        /// if item is found; otherwise, a negative number that is the bitwise complement
        /// of the index of the next element that is larger than item or, if there is
        /// no larger element, the bitwise complement of <see cref="ImmutableList{T}.Count"/>.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// <paramref name="comparer"/> is null, and the default comparer <see cref="Comparer{T}.Default"/>
        /// cannot find an implementation of the <see cref="IComparable{T}"/> generic interface
        /// or the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
        /// </exception>
        public int BinarySearch(T item, IComparer<T> comparer)
        {
            return this.BinarySearch(0, this.Count, item, comparer);
        }

        /// <summary>
        /// Searches a range of elements in the sorted <see cref="ImmutableList{T}"/>
        /// for an element using the specified comparer and returns the zero-based index
        /// of the element.
        /// </summary>
        /// <param name="index">The zero-based starting index of the range to search.</param>
        /// <param name="count"> The length of the range to search.</param>
        /// <param name="item">The object to locate. The value can be null for reference types.</param>
        /// <param name="comparer">
        /// The <see cref="IComparer{T}"/> implementation to use when comparing
        /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
        /// </param>
        /// <returns>
        /// The zero-based index of item in the sorted <see cref="ImmutableList{T}"/>,
        /// if item is found; otherwise, a negative number that is the bitwise complement
        /// of the index of the next element that is larger than item or, if there is
        /// no larger element, the bitwise complement of <see cref="ImmutableList{T}.Count"/>.
        /// </returns>
        /// <exception cref="ArgumentOutOfRangeException">
        /// <paramref name="index"/> is less than 0.-or-<paramref name="count"/> is less than 0.
        /// </exception>
        /// <exception cref="ArgumentException">
        /// <paramref name="index"/> and <paramref name="count"/> do not denote a valid range in the <see cref="ImmutableList{T}"/>.
        /// </exception>
        /// <exception cref="InvalidOperationException">
        /// <paramref name="comparer"/> is null, and the default comparer <see cref="Comparer{T}.Default"/>
        /// cannot find an implementation of the <see cref="IComparable{T}"/> generic interface
        /// or the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
        /// </exception>
        public int BinarySearch(int index, int count, T item, IComparer<T> comparer)
        {
            return _root.BinarySearch(index, count, item, comparer);
        }

        #region IImmutableList<T> Properties

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public bool IsEmpty
        {
            get
            {
                Contract.Ensures(Contract.Result<bool>() == (this.Count == 0));
                return _root.IsEmpty;
            }
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        IImmutableList<T> IImmutableList<T>.Clear()
        {
            return this.Clear();
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        public int Count
        {
            get
            {
                Contract.Ensures(Contract.Result<int>() >= 0);
                Contract.Ensures((Contract.Result<int>() == 0) == this.IsEmpty);
                return _root.Count;
            }
        }

        #endregion

        #region ICollection Properties

        /// <summary>
        /// See <see cref="ICollection"/>.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        object ICollection.SyncRoot
        {
            get { return this; }
        }

        /// <summary>
        /// See the <see cref="ICollection"/> interface.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        bool ICollection.IsSynchronized
        {
            get
            {
                // This is immutable, so it is always thread-safe.
                return true;
            }
        }

        #endregion

        #region IImmutableList<T> Indexers

        /// <summary>
        /// Gets the element of the set at the given index.
        /// </summary>
        /// <param name="index">The 0-based index of the element in the set to return.</param>
        /// <returns>The element at the given position.</returns>
        public T this[int index]
        {
            get
            {
                return _root[index];
            }
        }

        #endregion

        #region IOrderedCollection<T> Indexers

        /// <summary>
        /// Gets the element in the collection at a given index.
        /// </summary>
        T IOrderedCollection<T>.this[int index]
        {
            get
            {
                return this[index];
            }
        }

        #endregion

        #region Public methods

        /// <summary>
        /// Creates a collection with the same contents as this collection that
        /// can be efficiently mutated across multiple operations using standard
        /// mutable interfaces.
        /// </summary>
        /// <remarks>
        /// This is an O(1) operation and results in only a single (small) memory allocation.
        /// The mutable collection that is returned is *not* thread-safe.
        /// </remarks>
        [Pure]
        public Builder ToBuilder()
        {
            // We must not cache the instance created here and return it to various callers.
            // Those who request a mutable collection must get references to the collection
            // that version independently of each other.
            return new Builder(this);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> Add(T value)
        {
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().Count == this.Count + 1);
            var result = _root.Add(value);
            return this.Wrap(result);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> AddRange(IEnumerable<T> items)
        {
            Requires.NotNull(items, "items");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().Count >= this.Count);

            // Some optimizations may apply if we're an empty list.
            if (this.IsEmpty)
            {
                return this.FillFromEmpty(items);
            }

            var result = _root.AddRange(items);

            return this.Wrap(result);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> Insert(int index, T item)
        {
            Requires.Range(index >= 0 && index <= this.Count, "index");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().Count == this.Count + 1);
            return this.Wrap(_root.Insert(index, item));
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> InsertRange(int index, IEnumerable<T> items)
        {
            Requires.Range(index >= 0 && index <= this.Count, "index");
            Requires.NotNull(items, "items");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);

            var result = _root.InsertRange(index, items);

            return this.Wrap(result);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> Remove(T value)
        {
            return this.Remove(value, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> Remove(T value, IEqualityComparer<T> equalityComparer)
        {
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            int index = this.IndexOf(value, equalityComparer);
            return index < 0 ? this : this.RemoveAt(index);
        }

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="index">The starting index to begin removal.</param>
        /// <param name="count">The number of elements to remove.</param>
        /// <returns>A new list with the elements removed.</returns>
        [Pure]
        public ImmutableList<T> RemoveRange(int index, int count)
        {
            Requires.Range(index >= 0 && (index < this.Count || (index == this.Count && count == 0)), "index");
            Requires.Range(count >= 0 && index + count <= this.Count, "count");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);

            var result = _root;
            int remaining = count;
            while (remaining-- > 0)
            {
                result = result.RemoveAt(index);
            }

            return this.Wrap(result);
        }

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="items">The items to remove if matches are found in this list.</param>
        /// <returns>
        /// A new list with the elements removed.
        /// </returns>
        [Pure]
        public ImmutableList<T> RemoveRange(IEnumerable<T> items)
        {
            return this.RemoveRange(items, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="items">The items to remove if matches are found in this list.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>
        /// A new list with the elements removed.
        /// </returns>
        [Pure]
        public ImmutableList<T> RemoveRange(IEnumerable<T> items, IEqualityComparer<T> equalityComparer)
        {
            Requires.NotNull(items, "items");
            Requires.NotNull(equalityComparer, "equalityComparer");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().Count <= this.Count);

            // Some optimizations may apply if we're an empty list.
            if (this.IsEmpty)
            {
                return this;
            }

            // Let's not implement in terms of ImmutableList.Remove so that we're
            // not unnecessarily generating a new list object for each item.
            var result = _root;
            foreach (T item in items.GetEnumerableDisposable<T, Enumerator>())
            {
                int index = result.IndexOf(item, equalityComparer);
                if (index >= 0)
                {
                    result = result.RemoveAt(index);
                }
            }

            return this.Wrap(result);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> RemoveAt(int index)
        {
            Requires.Range(index >= 0 && index < this.Count, "index");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().Count == this.Count - 1);
            var result = _root.RemoveAt(index);
            return this.Wrap(result);
        }

        /// <summary>
        /// Removes all the elements that match the conditions defined by the specified
        /// predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to remove.
        /// </param>
        /// <returns>
        /// The new list.
        /// </returns>
        [Pure]
        public ImmutableList<T> RemoveAll(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);

            return this.Wrap(_root.RemoveAll(match));
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> SetItem(int index, T value)
        {
            return this.Wrap(_root.ReplaceAt(index, value));
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> Replace(T oldValue, T newValue)
        {
            return this.Replace(oldValue, newValue, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [Pure]
        public ImmutableList<T> Replace(T oldValue, T newValue, IEqualityComparer<T> equalityComparer)
        {
            Requires.NotNull(equalityComparer, "equalityComparer");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            Contract.Ensures(Contract.Result<ImmutableList<T>>().Count == this.Count);

            int index = this.IndexOf(oldValue, equalityComparer);
            if (index < 0)
            {
                throw new ArgumentException("Cannot find old value", "oldValue");
            }

            return this.SetItem(index, newValue);
        }

        /// <summary>
        /// Reverses the order of the elements in the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <returns>The reversed list.</returns>
        [Pure]
        public ImmutableList<T> Reverse()
        {
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            return this.Wrap(_root.Reverse());
        }

        /// <summary>
        /// Reverses the order of the elements in the specified range.
        /// </summary>
        /// <param name="index">The zero-based starting index of the range to reverse.</param>
        /// <param name="count">The number of elements in the range to reverse.</param> 
        /// <returns>The reversed list.</returns>
        [Pure]
        public ImmutableList<T> Reverse(int index, int count)
        {
            return this.Wrap(_root.Reverse(index, count));
        }

        /// <summary>
        /// Sorts the elements in the entire <see cref="ImmutableList{T}"/> using
        /// the default comparer.
        /// </summary>
        [Pure]
        public ImmutableList<T> Sort()
        {
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            return this.Wrap(_root.Sort());
        }

        /// <summary>
        /// Sorts the elements in the entire <see cref="ImmutableList{T}"/> using
        /// the specified <see cref="Comparison{T}"/>.
        /// </summary>
        /// <param name="comparison">
        /// The <see cref="Comparison{T}"/> to use when comparing elements.
        /// </param>
        /// <returns>The sorted list.</returns>
        [Pure]
        public ImmutableList<T> Sort(Comparison<T> comparison)
        {
            Requires.NotNull(comparison, "comparison");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            return this.Wrap(_root.Sort(comparison));
        }

        /// <summary>
        /// Sorts the elements in the entire <see cref="ImmutableList{T}"/> using
        /// the specified comparer.
        /// </summary>
        /// <param name="comparer">
        /// The <see cref="IComparer{T}"/> implementation to use when comparing
        /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
        /// </param>
        /// <returns>The sorted list.</returns>
        [Pure]
        public ImmutableList<T> Sort(IComparer<T> comparer)
        {
            Requires.NotNull(comparer, "comparer");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);
            return this.Wrap(_root.Sort(comparer));
        }

        /// <summary>
        /// Sorts the elements in a range of elements in <see cref="ImmutableList{T}"/>
        /// using the specified comparer.
        /// </summary>
        /// <param name="index">
        /// The zero-based starting index of the range to sort.
        /// </param>
        /// <param name="count">
        /// The length of the range to sort.
        /// </param>
        /// <param name="comparer">
        /// The <see cref="IComparer{T}"/> implementation to use when comparing
        /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
        /// </param>
        /// <returns>The sorted list.</returns>
        [Pure]
        public ImmutableList<T> Sort(int index, int count, IComparer<T> comparer)
        {
            Requires.Range(index >= 0, "index");
            Requires.Range(count >= 0, "count");
            Requires.Range(index + count <= this.Count, "count");
            Requires.NotNull(comparer, "comparer");
            Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);

            return this.Wrap(_root.Sort(index, count, comparer));
        }

        #endregion

        #region IImmutableListQueries<T> Methods

        /// <summary>
        /// Performs the specified action on each element of the list.
        /// </summary>
        /// <param name="action">The System.Action&lt;T&gt; delegate to perform on each element of the list.</param>
        public void ForEach(Action<T> action)
        {
            Requires.NotNull(action, "action");

            foreach (T item in this)
            {
                action(item);
            }
        }

        /// <summary>
        /// Copies the entire <see cref="ImmutableList{T}"/> to a compatible one-dimensional
        /// array, starting at the beginning of the target array.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements
        /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
        /// zero-based indexing.
        /// </param>
        public void CopyTo(T[] array)
        {
            Requires.NotNull(array, "array");
            Requires.Range(array.Length >= this.Count, "array");
            _root.CopyTo(array);
        }

        /// <summary>
        /// Copies the entire <see cref="ImmutableList{T}"/> to a compatible one-dimensional
        /// array, starting at the specified index of the target array.
        /// </summary>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements
        /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
        /// zero-based indexing.
        /// </param>
        /// <param name="arrayIndex">
        /// The zero-based index in array at which copying begins.
        /// </param>
        public void CopyTo(T[] array, int arrayIndex)
        {
            Requires.NotNull(array, "array");
            Requires.Range(arrayIndex >= 0, "arrayIndex");
            Requires.Range(array.Length >= arrayIndex + this.Count, "arrayIndex");
            _root.CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Copies a range of elements from the <see cref="ImmutableList{T}"/> to
        /// a compatible one-dimensional array, starting at the specified index of the
        /// target array.
        /// </summary>
        /// <param name="index">
        /// The zero-based index in the source <see cref="ImmutableList{T}"/> at
        /// which copying begins.
        /// </param>
        /// <param name="array">
        /// The one-dimensional <see cref="Array"/> that is the destination of the elements
        /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
        /// zero-based indexing.
        /// </param>
        /// <param name="arrayIndex">The zero-based index in array at which copying begins.</param>
        /// <param name="count">The number of elements to copy.</param>
        public void CopyTo(int index, T[] array, int arrayIndex, int count)
        {
            _root.CopyTo(index, array, arrayIndex, count);
        }

        /// <summary>
        /// Creates a shallow copy of a range of elements in the source <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="index">
        /// The zero-based <see cref="ImmutableList{T}"/> index at which the range
        /// starts.
        /// </param>
        /// <param name="count">
        /// The number of elements in the range.
        /// </param>
        /// <returns>
        /// A shallow copy of a range of elements in the source <see cref="ImmutableList{T}"/>.
        /// </returns>
        public ImmutableList<T> GetRange(int index, int count)
        {
            Requires.Range(index >= 0, "index");
            Requires.Range(count >= 0, "count");
            Requires.Range(index + count <= this.Count, "count");
            return this.Wrap(Node.NodeTreeFromList(this, index, count));
        }

        /// <summary>
        /// Converts the elements in the current <see cref="ImmutableList{T}"/> to
        /// another type, and returns a list containing the converted elements.
        /// </summary>
        /// <param name="converter">
        /// A <see cref="Func{T, TResult}"/> delegate that converts each element from
        /// one type to another type.
        /// </param>
        /// <typeparam name="TOutput">
        /// The type of the elements of the target array.
        /// </typeparam>
        /// <returns>
        /// A <see cref="ImmutableList{T}"/> of the target type containing the converted
        /// elements from the current <see cref="ImmutableList{T}"/>.
        /// </returns>
        public ImmutableList<TOutput> ConvertAll<TOutput>(Func<T, TOutput> converter)
        {
            Requires.NotNull(converter, "converter");
            return ImmutableList<TOutput>.WrapNode(_root.ConvertAll(converter));
        }

        /// <summary>
        /// Determines whether the <see cref="ImmutableList{T}"/> contains elements
        /// that match the conditions defined by the specified predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to search for.
        /// </param>
        /// <returns>
        /// true if the <see cref="ImmutableList{T}"/> contains one or more elements
        /// that match the conditions defined by the specified predicate; otherwise,
        /// false.
        /// </returns>
        public bool Exists(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.Exists(match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the first occurrence within the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The first element that matches the conditions defined by the specified predicate,
        /// if found; otherwise, the default value for type <typeparamref name="T"/>.
        /// </returns>
        public T Find(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.Find(match);
        }

        /// <summary>
        /// Retrieves all the elements that match the conditions defined by the specified
        /// predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to search for.
        /// </param>
        /// <returns>
        /// A <see cref="ImmutableList{T}"/> containing all the elements that match
        /// the conditions defined by the specified predicate, if found; otherwise, an
        /// empty <see cref="ImmutableList{T}"/>.
        /// </returns>
        public ImmutableList<T> FindAll(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.FindAll(match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the first occurrence within
        /// the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        public int FindIndex(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.FindIndex(match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the first occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that extends
        /// from the specified index to the last element.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the search.</param>
        /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element to search for.</param>
        /// <returns>
        /// The zero-based index of the first occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        public int FindIndex(int startIndex, Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            Requires.Range(startIndex >= 0, "startIndex");
            Requires.Range(startIndex <= this.Count, "startIndex");
            return _root.FindIndex(startIndex, match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the first occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that starts
        /// at the specified index and contains the specified number of elements.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the search.</param>
        /// <param name="count">The number of elements in the section to search.</param>
        /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element to search for.</param>
        /// <returns>
        /// The zero-based index of the first occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        public int FindIndex(int startIndex, int count, Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            Requires.Range(startIndex >= 0, "startIndex");
            Requires.Range(count >= 0, "count");
            Requires.Range(startIndex + count <= this.Count, "count");

            return _root.FindIndex(startIndex, count, match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the last occurrence within the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The last element that matches the conditions defined by the specified predicate,
        /// if found; otherwise, the default value for type <typeparamref name="T"/>.
        /// </returns>
        public T FindLast(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.FindLast(match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the last occurrence within
        /// the entire <see cref="ImmutableList{T}"/>.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        public int FindLastIndex(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.FindLastIndex(match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the last occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that extends
        /// from the first element to the specified index.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the backward search.</param>
        /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.</param>
        /// <returns>
        /// The zero-based index of the last occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        public int FindLastIndex(int startIndex, Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            Requires.Range(startIndex >= 0, "startIndex");
            Requires.Range(startIndex == 0 || startIndex < this.Count, "startIndex");
            return _root.FindLastIndex(startIndex, match);
        }

        /// <summary>
        /// Searches for an element that matches the conditions defined by the specified
        /// predicate, and returns the zero-based index of the last occurrence within
        /// the range of elements in the <see cref="ImmutableList{T}"/> that contains
        /// the specified number of elements and ends at the specified index.
        /// </summary>
        /// <param name="startIndex">The zero-based starting index of the backward search.</param>
        /// <param name="count">The number of elements in the section to search.</param>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
        /// to search for.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of an element that matches the
        /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
        /// </returns>
        public int FindLastIndex(int startIndex, int count, Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            Requires.Range(startIndex >= 0, "startIndex");
            Requires.Range(count <= this.Count, "count");
            Requires.Range(startIndex - count + 1 >= 0, "startIndex");

            return _root.FindLastIndex(startIndex, count, match);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// first occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
        /// that starts at the specified index and contains the specified number of elements.
        /// </summary>
        /// <param name="item">
        /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="index">
        /// The zero-based starting index of the search. 0 (zero) is valid in an empty
        /// list.
        /// </param>
        /// <param name="count">
        /// The number of elements in the section to search.
        /// </param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of <paramref name="item"/> within the range of
        /// elements in the <see cref="ImmutableList{T}"/> that starts at <paramref name="index"/> and
        /// contains <paramref name="count"/> number of elements, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public int IndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer)
        {
            return _root.IndexOf(item, index, count, equalityComparer);
        }

        /// <summary>
        /// Searches for the specified object and returns the zero-based index of the
        /// last occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
        /// that contains the specified number of elements and ends at the specified
        /// index.
        /// </summary>
        /// <param name="item">
        /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="index">The zero-based starting index of the backward search.</param>
        /// <param name="count">The number of elements in the section to search.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>
        /// The zero-based index of the last occurrence of <paramref name="item"/> within the range of elements
        /// in the <see cref="ImmutableList{T}"/> that contains <paramref name="count"/> number of elements
        /// and ends at <paramref name="index"/>, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public int LastIndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer)
        {
            return _root.LastIndexOf(item, index, count, equalityComparer);
        }

        /// <summary>
        /// Determines whether every element in the <see cref="ImmutableList{T}"/>
        /// matches the conditions defined by the specified predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions to check against
        /// the elements.
        /// </param>
        /// <returns>
        /// true if every element in the <see cref="ImmutableList{T}"/> matches the
        /// conditions defined by the specified predicate; otherwise, false. If the list
        /// has no elements, the return value is true.
        /// </returns>
        public bool TrueForAll(Predicate<T> match)
        {
            Requires.NotNull(match, "match");
            return _root.TrueForAll(match);
        }

        #endregion

        #region IImmutableList<T> Methods

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        public bool Contains(T value)
        {
            Contract.Ensures(!this.IsEmpty || !Contract.Result<bool>());
            return this.IndexOf(value) >= 0;
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        public int IndexOf(T value)
        {
            return this.IndexOf(value, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.Add(T value)
        {
            return this.Add(value);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.AddRange(IEnumerable<T> items)
        {
            return this.AddRange(items);
        }

        /// <summary>
        /// Inserts the specified value at the specified index.
        /// </summary>
        /// <param name="index">The index at which to insert the value.</param>
        /// <param name="item">The element to add.</param>
        /// <returns>The new immutable list.</returns>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.Insert(int index, T item)
        {
            return this.Insert(index, item);
        }

        /// <summary>
        /// Inserts the specified value at the specified index.
        /// </summary>
        /// <param name="index">The index at which to insert the value.</param>
        /// <param name="items">The elements to add.</param>
        /// <returns>The new immutable list.</returns>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.InsertRange(int index, IEnumerable<T> items)
        {
            return this.InsertRange(index, items);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.Remove(T value, IEqualityComparer<T> equalityComparer)
        {
            return this.Remove(value, equalityComparer);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.RemoveAll(Predicate<T> match)
        {
            return this.RemoveAll(match);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.RemoveRange(IEnumerable<T> items, IEqualityComparer<T> equalityComparer)
        {
            return this.RemoveRange(items, equalityComparer);
        }

        /// <summary>
        /// See the <see cref="IImmutableList{T}"/> interface.
        /// </summary>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.RemoveRange(int index, int count)
        {
            return this.RemoveRange(index, count);
        }

        /// <summary>
        /// Removes the element at the specified index.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <returns>A new list with the elements removed.</returns>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.RemoveAt(int index)
        {
            return this.RemoveAt(index);
        }

        /// <summary>
        /// Replaces an element in the list at a given position with the specified element.
        /// </summary>
        /// <param name="index">The position in the list of the element to replace.</param>
        /// <param name="value">The element to replace the old element with.</param>
        /// <returns>The new list.</returns>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.SetItem(int index, T value)
        {
            return this.SetItem(index, value);
        }

        /// <summary>
        /// Replaces an element in the list with the specified element.
        /// </summary>
        /// <param name="oldValue">The element to replace.</param>
        /// <param name="newValue">The element to replace the old element with.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// </param>
        /// <returns>The new list.</returns>
        /// <exception cref="ArgumentException">Thrown when the old value does not exist in the list.</exception>
        [ExcludeFromCodeCoverage]
        IImmutableList<T> IImmutableList<T>.Replace(T oldValue, T newValue, IEqualityComparer<T> equalityComparer)
        {
            return this.Replace(oldValue, newValue, equalityComparer);
        }

        #endregion

        #region IEnumerable<T> Members

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
        /// </returns>
        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        #endregion

        #region IList<T> Members

        /// <summary>
        /// Inserts the specified index.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <param name="item">The item.</param>
        /// <exception cref="System.NotSupportedException"></exception>
        void IList<T>.Insert(int index, T item)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Removes the value at the specified index.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <exception cref="System.NotSupportedException"></exception>
        void IList<T>.RemoveAt(int index)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Gets or sets the value at the specified index.
        /// </summary>
        T IList<T>.this[int index]
        {
            get { return this[index]; }
            set { throw new NotSupportedException(); }
        }

        #endregion

        #region ICollection<T> Members

        /// <summary>
        /// Adds the specified item.
        /// </summary>
        /// <param name="item">The item.</param>
        /// <exception cref="System.NotImplementedException"></exception>
        void ICollection<T>.Add(T item)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Clears this instance.
        /// </summary>
        /// <exception cref="System.NotSupportedException"></exception>
        void ICollection<T>.Clear()
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="ICollection{T}"/> is read-only.
        /// </summary>
        /// <returns>true if the <see cref="ICollection{T}"/> is read-only; otherwise, false.
        ///   </returns>
        bool ICollection<T>.IsReadOnly
        {
            get { return true; }
        }

        /// <summary>
        /// Removes the specified item.
        /// </summary>
        /// <param name="item">The item.</param>
        /// <returns></returns>
        /// <exception cref="System.NotSupportedException"></exception>
        bool ICollection<T>.Remove(T item)
        {
            throw new NotSupportedException();
        }

        #endregion

        #region ICollection Methods

        /// <summary>
        /// See the <see cref="ICollection"/> interface.
        /// </summary>
        void System.Collections.ICollection.CopyTo(Array array, int arrayIndex)
        {
            _root.CopyTo(array, arrayIndex);
        }

        #endregion

        #region IList members

        /// <summary>
        /// Adds an item to the <see cref="IList"/>.
        /// </summary>
        /// <param name="value">The object to add to the <see cref="IList"/>.</param>
        /// <returns>
        /// The position into which the new element was inserted, or -1 to indicate that the item was not inserted into the collection,
        /// </returns>
        /// <exception cref="System.NotImplementedException"></exception>
        int IList.Add(object value)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Removes the <see cref="IList"/> item at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the item to remove.</param>
        /// <exception cref="System.NotSupportedException"></exception>
        void IList.RemoveAt(int index)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Clears this instance.
        /// </summary>
        /// <exception cref="System.NotImplementedException"></exception>
        void IList.Clear()
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Determines whether the <see cref="IList"/> contains a specific value.
        /// </summary>
        /// <param name="value">The object to locate in the <see cref="IList"/>.</param>
        /// <returns>
        /// true if the <see cref="object"/> is found in the <see cref="IList"/>; otherwise, false.
        /// </returns>
        /// <exception cref="System.NotImplementedException"></exception>
        bool IList.Contains(object value)
        {
            return this.Contains((T)value);
        }

        /// <summary>
        /// Determines the index of a specific item in the <see cref="IList"/>.
        /// </summary>
        /// <param name="value">The object to locate in the <see cref="IList"/>.</param>
        /// <returns>
        /// The index of <paramref name="value"/> if found in the list; otherwise, -1.
        /// </returns>
        /// <exception cref="System.NotImplementedException"></exception>
        int IList.IndexOf(object value)
        {
            return this.IndexOf((T)value);
        }

        /// <summary>
        /// Inserts an item to the <see cref="IList"/> at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index at which <paramref name="value"/> should be inserted.</param>
        /// <param name="value">The object to insert into the <see cref="IList"/>.</param>
        /// <exception cref="System.NotImplementedException"></exception>
        void IList.Insert(int index, object value)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="IList"/> has a fixed size.
        /// </summary>
        /// <returns>true if the <see cref="IList"/> has a fixed size; otherwise, false.</returns>
        /// <exception cref="System.NotImplementedException"></exception>
        bool IList.IsFixedSize
        {
            get { return true; }
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="ICollection{T}"/> is read-only.
        /// </summary>
        /// <returns>true if the <see cref="ICollection{T}"/> is read-only; otherwise, false.
        ///   </returns>
        /// <exception cref="System.NotImplementedException"></exception>
        bool IList.IsReadOnly
        {
            get { return true; }
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="IList"/>.
        /// </summary>
        /// <param name="value">The object to remove from the <see cref="IList"/>.</param>
        /// <exception cref="System.NotImplementedException"></exception>
        void IList.Remove(object value)
        {
            throw new NotSupportedException();
        }

        /// <summary>
        /// Gets or sets the <see cref="System.Object"/> at the specified index.
        /// </summary>
        /// <value>
        /// The <see cref="System.Object"/>.
        /// </value>
        /// <param name="index">The index.</param>
        /// <returns></returns>
        /// <exception cref="System.NotImplementedException"></exception>
        object IList.this[int index]
        {
            get { return this[index]; }
            set { throw new NotSupportedException(); }
        }

        #endregion

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
        /// </returns>
        /// <remarks>
        /// CAUTION: when this enumerator is actually used as a valuetype (not boxed) do NOT copy it by assigning to a second variable 
        /// or by passing it to another method.  When this enumerator is disposed of it returns a mutable reference type stack to a resource pool,
        /// and if the value type enumerator is copied (which can easily happen unintentionally if you pass the value around) there is a risk
        /// that a stack that has already been returned to the resource pool may still be in use by one of the enumerator copies, leading to data
        /// corruption and/or exceptions.
        /// </remarks>
        public Enumerator GetEnumerator()
        {
            return new Enumerator(_root);
        }

        // --- START of EXTENSION to MICROSOFT code ---
        // --- Methods added for TypeCobol ---

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <param name="startIndex">The index of the first element to enumerate.</param>
        /// <param name="count">The number of elements in this collection.</param>
        /// <param name="reversed"><c>true</c> if the list should be enumerated in reverse order.</param>
        public IEnumerator<T> GetEnumerator(int startIndex, int count, bool reversed)
        {
            return _root.GetEnumerator(startIndex, count, reversed);
        }

        /// <summary>
        /// Searches for the specified object around the initial index of the object 
        /// in the first version of the immutable list.
        /// This method is useful only when the two following conditions are true :
        /// - the searched item is present only once in the list
        /// - the probability that the searched item did not move far from its initial position is very high
        /// </summary>
        /// <param name="item">
        /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
        /// can be null for reference types.
        /// </param>
        /// <param name="initialIndex">
        /// The zero-based index of the serached object in the first version of the immutable list.
        /// </param>
        /// <returns>
        /// The zero-based index of the first occurrence of <paramref name="item"/> within the range of
        /// elements in the <see cref="ImmutableList{T}"/> that starts at <paramref name="index"/> and
        /// contains <paramref name="count"/> number of elements, if found; otherwise, -1.
        /// </returns>
        [Pure]
        public int IndexOf(object item, int initialIndex)
        {
            return _root.IndexOf((T)item, initialIndex);
        }

        // --- END of EXTENSION to MICROSOFT code ---

        /// <summary>
        /// Returns the root <see cref="Node"/> of the list
        /// </summary>
        internal Node Root
        {
            get
            {
                return _root;
            }
        }

        /// <summary>
        /// Creates a new sorted set wrapper for a node tree.
        /// </summary>
        /// <param name="root">The root of the collection.</param>
        /// <returns>The immutable sorted set instance.</returns>
        [Pure]
        private static ImmutableList<T> WrapNode(Node root)
        {
            return root.IsEmpty
                ? ImmutableList<T>.Empty
                : new ImmutableList<T>(root);
        }

        /// <summary>
        /// Attempts to discover an <see cref="ImmutableList{T}"/> instance beneath some enumerable sequence
        /// if one exists.
        /// </summary>
        /// <param name="sequence">The sequence that may have come from an immutable list.</param>
        /// <param name="other">Receives the concrete <see cref="ImmutableList{T}"/> typed value if one can be found.</param>
        /// <returns><c>true</c> if the cast was successful; <c>false</c> otherwise.</returns>
        private static bool TryCastToImmutableList(IEnumerable<T> sequence, out ImmutableList<T> other)
        {
            other = sequence as ImmutableList<T>;
            if (other != null)
            {
                return true;
            }

            var builder = sequence as Builder;
            if (builder != null)
            {
                other = builder.ToImmutable();
                return true;
            }

            return false;
        }

        /// <summary>
        /// Creates a wrapping collection type around a root node.
        /// </summary>
        /// <param name="root">The root node to wrap.</param>
        /// <returns>A wrapping collection type for the new tree.</returns>
        [Pure]
        private ImmutableList<T> Wrap(Node root)
        {
            if (root != _root)
            {
                return root.IsEmpty ? this.Clear() : new ImmutableList<T>(root);
            }
            else
            {
                return this;
            }
        }

        /// <summary>
        /// Creates an immutable list with the contents from a sequence of elements.
        /// </summary>
        /// <param name="items">The sequence of elements from which to create the list.</param>
        /// <returns>The immutable list.</returns>
        [Pure]
        private ImmutableList<T> FillFromEmpty(IEnumerable<T> items)
        {
            Debug.Assert(this.IsEmpty);

            // If the items being added actually come from an ImmutableList<T>
            // then there is no value in reconstructing it.
            ImmutableList<T> other;
            if (TryCastToImmutableList(items, out other))
            {
                return other;
            }

            // Rather than build up the immutable structure in the incremental way,
            // build it in such a way as to generate minimal garbage, by assembling
            // the immutable binary tree from leaf to root.  This requires
            // that we know the length of the item sequence in advance, and can
            // index into that sequence like a list, so the one possible piece of 
            // garbage produced is a temporary array to store the list while
            // we build the tree.
            var list = items.AsOrderedCollection();
            if (list.Count == 0)
            {
                return this;
            }

            Node root = Node.NodeTreeFromList(list, 0, list.Count);
            return new ImmutableList<T>(root);
        }

        /// <summary>
        /// Enumerates the contents of a binary tree.
        /// </summary>
        /// <remarks>
        /// This struct can and should be kept in exact sync with the other binary tree enumerators: 
        /// <see cref="ImmutableList{T}.Enumerator"/>, <see cref="ImmutableSortedDictionary{TKey, TValue}.Enumerator"/>, and <see cref="ImmutableSortedSet{T}.Enumerator"/>.
        /// 
        /// CAUTION: when this enumerator is actually used as a valuetype (not boxed) do NOT copy it by assigning to a second variable 
        /// or by passing it to another method.  When this enumerator is disposed of it returns a mutable reference type stack to a resource pool,
        /// and if the value type enumerator is copied (which can easily happen unintentionally if you pass the value around) there is a risk
        /// that a stack that has already been returned to the resource pool may still be in use by one of the enumerator copies, leading to data
        /// corruption and/or exceptions.
        /// </remarks>
        [EditorBrowsable(EditorBrowsableState.Advanced)]
        public struct Enumerator : IEnumerator<T>, ISecurePooledObjectUser, IStrongEnumerator<T>
        {
            /// <summary>
            /// The resource pool of reusable mutable stacks for purposes of enumeration.
            /// </summary>
            /// <remarks>
            /// We utilize this resource pool to make "allocation free" enumeration achievable.
            /// </remarks>
            private static readonly SecureObjectPool<Stack<RefAsValueType<Node>>, Enumerator> s_EnumeratingStacks =
                new SecureObjectPool<Stack<RefAsValueType<Node>>, Enumerator>();

            /// <summary>
            /// The builder being enumerated, if applicable.
            /// </summary>
            private readonly Builder _builder;

            /// <summary>
            /// A unique ID for this instance of this enumerator.
            /// Used to protect pooled objects from use after they are recycled.
            /// </summary>
            private readonly int _poolUserId;

            /// <summary>
            /// The starting index of the collection at which to begin enumeration.
            /// </summary>
            private readonly int _startIndex;

            /// <summary>
            /// The number of elements to include in the enumeration.
            /// </summary>
            private readonly int _count;

            /// <summary>
            /// The number of elements left in the enumeration.
            /// </summary>
            private int _remainingCount;

            /// <summary>
            /// A value indicating whether this enumerator walks in reverse order.
            /// </summary>
            private bool _reversed;

            /// <summary>
            /// The set being enumerated.
            /// </summary>
            private Node _root;

            /// <summary>
            /// The stack to use for enumerating the binary tree.
            /// </summary>
            private SecurePooledObject<Stack<RefAsValueType<Node>>> _stack;

            /// <summary>
            /// The node currently selected.
            /// </summary>
            private Node _current;

            /// <summary>
            /// The version of the builder (when applicable) that is being enumerated.
            /// </summary>
            private int _enumeratingBuilderVersion;

            /// <summary>
            /// Initializes an <see cref="Enumerator"/> structure.
            /// </summary>
            /// <param name="root">The root of the set to be enumerated.</param>
            /// <param name="builder">The builder, if applicable.</param>
            /// <param name="startIndex">The index of the first element to enumerate.</param>
            /// <param name="count">The number of elements in this collection.</param>
            /// <param name="reversed"><c>true</c> if the list should be enumerated in reverse order.</param>
            internal Enumerator(Node root, Builder builder = null, int startIndex = -1, int count = -1, bool reversed = false)
            {
                Requires.NotNull(root, "root");
                Requires.Range(startIndex >= -1, "startIndex");
                Requires.Range(count >= -1, "count");
                Requires.Argument(reversed || count == -1 || (startIndex == -1 ? 0 : startIndex) + count <= root.Count);
                Requires.Argument(!reversed || count == -1 || (startIndex == -1 ? root.Count - 1 : startIndex) - count + 1 >= 0);

                _root = root;
                _builder = builder;
                _current = null;
                _startIndex = startIndex >= 0 ? startIndex : (reversed ? root.Count - 1 : 0);
                _count = count == -1 ? root.Count : count;
                _remainingCount = _count;
                _reversed = reversed;
                _enumeratingBuilderVersion = builder != null ? builder.Version : -1;
                _poolUserId = SecureObjectPool.NewId();
                _stack = null;
                if (_count > 0)
                {
                    if (!s_EnumeratingStacks.TryTake(this, out _stack))
                    {
                        _stack = s_EnumeratingStacks.PrepNew(this, new Stack<RefAsValueType<Node>>(root.Height));
                    }

                    this.ResetStack();
                }
            }

            /// <inheritdoc/>
            int ISecurePooledObjectUser.PoolUserId
            {
                get { return _poolUserId; }
            }

            /// <summary>
            /// The current element.
            /// </summary>
            public T Current
            {
                get
                {
                    this.ThrowIfDisposed();
                    if (_current != null)
                    {
                        return _current.Value;
                    }

                    throw new InvalidOperationException();
                }
            }

            /// <summary>
            /// The current element.
            /// </summary>
            object System.Collections.IEnumerator.Current
            {
                get { return this.Current; }
            }

            /// <summary>
            /// Disposes of this enumerator and returns the stack reference to the resource pool.
            /// </summary>
            public void Dispose()
            {
                _root = null;
                _current = null;
                Stack<RefAsValueType<Node>> stack;
                if (_stack != null && _stack.TryUse(ref this, out stack))
                {
                    stack.ClearFastWhenEmpty();
                    s_EnumeratingStacks.TryAdd(this, _stack);
                }

                _stack = null;
            }

            /// <summary>
            /// Advances enumeration to the next element.
            /// </summary>
            /// <returns>A value indicating whether there is another element in the enumeration.</returns>
            public bool MoveNext()
            {
                this.ThrowIfDisposed();
                this.ThrowIfChanged();

                if (_stack != null)
                {
                    var stack = _stack.Use(ref this);
                    if (_remainingCount > 0 && stack.Count > 0)
                    {
                        Node n = stack.Pop().Value;
                        _current = n;
                        this.PushNext(this.NextBranch(n));
                        _remainingCount--;
                        return true;
                    }
                }

                _current = null;
                return false;
            }

            /// <summary>
            /// Restarts enumeration.
            /// </summary>
            public void Reset()
            {
                this.ThrowIfDisposed();

                _enumeratingBuilderVersion = _builder != null ? _builder.Version : -1;
                _remainingCount = _count;
                if (_stack != null)
                {
                    this.ResetStack();
                }
            }

            /// <summary>Resets the stack used for enumeration.</summary>
            private void ResetStack()
            {
                var stack = _stack.Use(ref this);
                stack.ClearFastWhenEmpty();

                var node = _root;
                var skipNodes = _reversed ? _root.Count - _startIndex - 1 : _startIndex;
                while (!node.IsEmpty && skipNodes != this.PreviousBranch(node).Count)
                {
                    if (skipNodes < this.PreviousBranch(node).Count)
                    {
                        stack.Push(new RefAsValueType<Node>(node));
                        node = this.PreviousBranch(node);
                    }
                    else
                    {
                        skipNodes -= this.PreviousBranch(node).Count + 1;
                        node = this.NextBranch(node);
                    }
                }

                if (!node.IsEmpty)
                {
                    stack.Push(new RefAsValueType<Node>(node));
                }
            }

            /// <summary>
            /// Obtains the right branch of the given node (or the left, if walking in reverse).
            /// </summary>
            private Node NextBranch(Node node)
            {
                return _reversed ? node.Left : node.Right;
            }

            /// <summary>
            /// Obtains the left branch of the given node (or the right, if walking in reverse).
            /// </summary>
            private Node PreviousBranch(Node node)
            {
                return _reversed ? node.Right : node.Left;
            }

            /// <summary>
            /// Throws an <see cref="ObjectDisposedException"/> if this enumerator has been disposed.
            /// </summary>
            private void ThrowIfDisposed()
            {
                Contract.Ensures(_root != null);
                Contract.EnsuresOnThrow<ObjectDisposedException>(_root == null);

                // Since this is a struct, copies might not have been marked as disposed.
                // But the stack we share across those copies would know.
                // This trick only works when we have a non-null stack.
                // For enumerators of empty collections, there isn't any natural
                // way to know when a copy of the struct has been disposed of.

                if (_root == null || (_stack != null && !_stack.IsOwned(ref this)))
                {
                    Requires.FailObjectDisposed(this);
                }
            }

            /// <summary>
            /// Throws an exception if the underlying builder's contents have been changed since enumeration started.
            /// </summary>
            /// <exception cref="System.InvalidOperationException">Thrown if the collection has changed.</exception>
            private void ThrowIfChanged()
            {
                if (_builder != null && _builder.Version != _enumeratingBuilderVersion)
                {
                    throw new InvalidOperationException("Collection modified during enumeration");
                }
            }

            /// <summary>
            /// Pushes this node and all its Left descendants onto the stack.
            /// </summary>
            /// <param name="node">The starting node to push onto the stack.</param>
            private void PushNext(Node node)
            {
                Requires.NotNull(node, "node");
                if (!node.IsEmpty)
                {
                    var stack = _stack.Use(ref this);
                    while (!node.IsEmpty)
                    {
                        stack.Push(new RefAsValueType<Node>(node));
                        node = this.PreviousBranch(node);
                    }
                }
            }
        }

        /// <summary>
        /// A node in the AVL tree storing this set.
        /// </summary>
        [DebuggerDisplay("{_key}")]
        internal sealed class Node : IBinaryTree<T>, IEnumerable<T>
        {
            /// <summary>
            /// The default empty node.
            /// </summary>
            internal static readonly Node EmptyNode = new Node();

            /// <summary>
            /// The key associated with this node.
            /// </summary>
            private T _key;

            /// <summary>
            /// A value indicating whether this node has been frozen (made immutable).
            /// </summary>
            /// <remarks>
            /// Nodes must be frozen before ever being observed by a wrapping collection type
            /// to protect collections from further mutations.
            /// </remarks>
            private bool _frozen;

            /// <summary>
            /// The depth of the tree beneath this node.
            /// </summary>
            private byte _height; // AVL tree max height <= ~1.44 * log2(maxNodes + 2)

            /// <summary>
            /// The number of elements contained by this subtree starting at this node.
            /// </summary>
            /// <remarks>
            /// If this node would benefit from saving 4 bytes, we could have only a few nodes 
            /// scattered throughout the graph actually record the count of nodes beneath them.
            /// Those without the count could query their descendants, which would often short-circuit
            /// when they hit a node that *does* include a count field.
            /// </remarks>
            private int _count;

            /// <summary>
            /// The left tree.
            /// </summary>
            private Node _left;

            /// <summary>
            /// The right tree.
            /// </summary>
            private Node _right;

            /// <summary>
            /// Initializes a new instance of the <see cref="ImmutableList{T}.Node"/> class
            /// that is pre-frozen.
            /// </summary>
            private Node()
            {
                Contract.Ensures(this.IsEmpty);
                _frozen = true; // the empty node is *always* frozen.
            }

            /// <summary>
            /// Initializes a new instance of the <see cref="ImmutableList{T}.Node"/> class
            /// that is not yet frozen.
            /// </summary>
            /// <param name="key">The value stored by this node.</param>
            /// <param name="left">The left branch.</param>
            /// <param name="right">The right branch.</param>
            /// <param name="frozen">Whether this node is prefrozen.</param>
            private Node(T key, Node left, Node right, bool frozen = false)
            {
                Requires.NotNull(left, "left");
                Requires.NotNull(right, "right");
                Debug.Assert(!frozen || (left._frozen && right._frozen));
                Contract.Ensures(!this.IsEmpty);

                _key = key;
                _left = left;
                _right = right;
                _height = checked((byte)(1 + Math.Max(left._height, right._height)));
                _count = 1 + left._count + right._count;
                _frozen = frozen;
            }

            /// <summary>
            /// Gets a value indicating whether this instance is empty.
            /// </summary>
            /// <value>
            /// <c>true</c> if this instance is empty; otherwise, <c>false</c>.
            /// </value>
            public bool IsEmpty
            {
                get
                {
                    Contract.Ensures(Contract.Result<bool>() == (_left == null));
                    return _left == null;
                }
            }

            /// <summary>
            /// Gets the height of the tree beneath this node.
            /// </summary>
            public int Height
            {
                get { return _height; }
            }

            /// <summary>
            /// Gets the left branch of this node.
            /// </summary>
            public Node Left { get { return _left; } }

            /// <summary>
            /// Gets the left branch of this node.
            /// </summary>
            IBinaryTree IBinaryTree.Left
            {
                get { return _left; }
            }

            /// <summary>
            /// Gets the right branch of this node.
            /// </summary>
            public Node Right { get { return _right; } }

            /// <summary>
            /// Gets the right branch of this node.
            /// </summary>
            IBinaryTree IBinaryTree.Right
            {
                get { return _right; }
            }

            /// <summary>
            /// Gets the left branch of this node.
            /// </summary>
            IBinaryTree<T> IBinaryTree<T>.Left
            {
                get { return _left; }
            }

            /// <summary>
            /// Gets the right branch of this node.
            /// </summary>
            IBinaryTree<T> IBinaryTree<T>.Right
            {
                get { return _right; }
            }

            /// <summary>
            /// Gets the value represented by the current node.
            /// </summary>
            public T Value
            {
                get { return _key; }
            }

            /// <summary>
            /// Gets the number of elements contained by this subtree starting at this node.
            /// </summary>
            public int Count
            {
                get
                {
                    Contract.Ensures(Contract.Result<int>() == _count);
                    return _count;
                }
            }

            /// <summary>
            /// Gets the key.
            /// </summary>
            internal T Key
            {
                get { return _key; }
            }

            /// <summary>
            /// Gets the element of the set at the given index.
            /// </summary>
            /// <param name="index">The 0-based index of the element in the set to return.</param>
            /// <returns>The element at the given position.</returns>
            internal T this[int index]
            {
                get
                {
                    Requires.Range(index >= 0 && index < this.Count, "index");

                    if (index < _left._count)
                    {
                        return _left[index];
                    }

                    if (index > _left._count)
                    {
                        return _right[index - _left._count - 1];
                    }

                    return _key;
                }
            }

            #region IEnumerable<T> Members

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            public Enumerator GetEnumerator()
            {
                return new Enumerator(this);
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            [ExcludeFromCodeCoverage] // internal, never called, but here for interface implementation
            IEnumerator<T> IEnumerable<T>.GetEnumerator()
            {
                return this.GetEnumerator();
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            [ExcludeFromCodeCoverage] // internal, never called, but here for interface implementation
            IEnumerator IEnumerable.GetEnumerator()
            {
                return this.GetEnumerator();
            }

            #endregion

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <param name="builder">The builder, if applicable.</param>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            internal Enumerator GetEnumerator(Builder builder)
            {
                return new Enumerator(this, builder);
            }

            // --- START of EXTENSION to MICROSOFT code ---
            // --- Methods added for TypeCobol ---

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <param name="startIndex">The index of the first element to enumerate.</param>
            /// <param name="count">The number of elements in this collection.</param>
            /// <param name="reversed"><c>true</c> if the list should be enumerated in reverse order.</param>
            internal Enumerator GetEnumerator(int startIndex, int count, bool reversed)
            {
                return new Enumerator(this, null, startIndex, count, reversed);
            }

            /// <summary>
            /// Searches for the specified object around the initial index of the object 
            /// in the first version of the immutable list.
            /// This method is useful only when the two following conditions are true :
            /// - the searched item is present only once in the list
            /// - the probability that the searched item did not move far from its initial position is very high
            /// </summary>
            /// <param name="item">
            /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="initialIndex">
            /// The zero-based index of the serached object in the first version of the immutable list.
            /// </param>
            /// <returns>
            /// The zero-based index of the first occurrence of <paramref name="item"/> within the range of
            /// elements in the <see cref="ImmutableList{T}"/> that starts at <paramref name="index"/> and
            /// contains <paramref name="count"/> number of elements, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int IndexOf(T item, int initialIndex)
            {
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

                // Second try : check if less than 'proximitySpanSize' elements were added before the item
                int elementsCountAfterInitialIndex = this.Count - initialIndex - 1;
                if (elementsCountAfterInitialIndex > 0)
                {
                    Enumerator enumerator = GetEnumerator(initialIndex + 1, elementsCountAfterInitialIndex, false);
                    for (int i = 1; i <= Math.Min(elementsCountAfterInitialIndex, proximitySpanSize); i++)
                    {
                        enumerator.MoveNext();
                        if (equalityComparer.Equals(item, enumerator.Current))
                        {
                            return initialIndex + i;
                        }
                    }
                }

                // Third try : check if less than 'proximitySpanSize' elements were removed before the item
                int elementsCountBeforeInitialIndex = initialIndex;
                if (elementsCountBeforeInitialIndex > 0)
                {
                    Enumerator enumerator = GetEnumerator(initialIndex - 1, elementsCountBeforeInitialIndex, true);
                    for (int i = 1; i <= Math.Min(elementsCountBeforeInitialIndex, proximitySpanSize); i++)
                    {
                        enumerator.MoveNext();
                        if (equalityComparer.Equals(item, enumerator.Current))
                        {
                            return initialIndex - i;
                        }
                    }
                }

                // If the list was small, we already know that the serached item is not in the list
                if (this.Count <= (2 * proximitySpanSize + 1))
                {
                    return -1;
                }

                // Last try : we can do no better than to scan the entire list
                // - from the beginning if the line was initially located in the first half
                if (initialIndex <= (this.Count / 2))
                {
                    return this.IndexOf(item, equalityComparer);
                }
                else
                {
                    return this.LastIndexOf(item, this.Count - 1, this.Count, equalityComparer);
                }
            }

            // --- END of EXTENSION to MICROSOFT code ---

            /// <summary>
            /// Creates a node tree that contains the contents of a list.
            /// </summary>
            /// <param name="items">An indexable list with the contents that the new node tree should contain.</param>
            /// <param name="start">The starting index within <paramref name="items"/> that should be captured by the node tree.</param>
            /// <param name="length">The number of elements from <paramref name="items"/> that should be captured by the node tree.</param>
            /// <returns>The root of the created node tree.</returns>
            [Pure]
            internal static Node NodeTreeFromList(IOrderedCollection<T> items, int start, int length)
            {
                Requires.NotNull(items, "items");
                Requires.Range(start >= 0, "start");
                Requires.Range(length >= 0, "length");

                if (length == 0)
                {
                    return EmptyNode;
                }

                int rightCount = (length - 1) / 2;
                int leftCount = (length - 1) - rightCount;
                Node left = NodeTreeFromList(items, start, leftCount);
                Node right = NodeTreeFromList(items, start + leftCount + 1, rightCount);
                return new Node(items[start + leftCount], left, right, true);
            }

            /// <summary>
            /// Adds the specified key to the tree.
            /// </summary>
            /// <param name="key">The key.</param>
            /// <returns>The new tree.</returns>
            internal Node Add(T key)
            {
                return this.Insert(_count, key);
            }

            /// <summary>
            /// Adds a value at a given index to this node.
            /// </summary>
            /// <param name="index">The location for the new value.</param>
            /// <param name="key">The value to add.</param>
            /// <returns>The new tree.</returns>
            internal Node Insert(int index, T key)
            {
                Requires.Range(index >= 0 && index <= this.Count, "index");

                if (this.IsEmpty)
                {
                    return new Node(key, this, this);
                }
                else
                {
                    Node result;
                    if (index <= _left._count)
                    {
                        var newLeft = _left.Insert(index, key);
                        result = this.Mutate(left: newLeft);
                    }
                    else
                    {
                        var newRight = _right.Insert(index - _left._count - 1, key);
                        result = this.Mutate(right: newRight);
                    }

                    return MakeBalanced(result);
                }
            }

            /// <summary>
            /// Adds the specified keys to the tree.
            /// </summary>
            /// <param name="keys">The keys.</param>
            /// <returns>The new tree.</returns>
            internal Node AddRange(IEnumerable<T> keys)
            {
                return this.InsertRange(_count, keys);
            }

            /// <summary>
            /// Adds a collection of values at a given index to this node.
            /// </summary>
            /// <param name="index">The location for the new values.</param>
            /// <param name="keys">The values to add.</param>
            /// <returns>The new tree.</returns>
            internal Node InsertRange(int index, IEnumerable<T> keys)
            {
                Requires.Range(index >= 0 && index <= this.Count, "index");
                Requires.NotNull(keys, "keys");

                if (this.IsEmpty)
                {
                    ImmutableList<T> other;
                    if (TryCastToImmutableList(keys, out other))
                    {
                        return other._root;
                    }

                    var list = keys.AsOrderedCollection();
                    return Node.NodeTreeFromList(list, 0, list.Count);
                }
                else
                {
                    Node result;
                    if (index <= _left._count)
                    {
                        var newLeft = _left.InsertRange(index, keys);
                        result = this.Mutate(left: newLeft);
                    }
                    else
                    {
                        var newRight = _right.InsertRange(index - _left._count - 1, keys);
                        result = this.Mutate(right: newRight);
                    }

                    return BalanceNode(result);
                }
            }

            /// <summary>
            /// Removes a value at a given index to this node.
            /// </summary>
            /// <param name="index">The location for the new value.</param>
            /// <returns>The new tree.</returns>
            internal Node RemoveAt(int index)
            {
                Requires.Range(index >= 0 && index < this.Count, "index");

                Node result = this;
                if (index == _left._count)
                {
                    // We have a match. If this is a leaf, just remove it 
                    // by returning Empty.  If we have only one child,
                    // replace the node with the child.
                    if (_right.IsEmpty && _left.IsEmpty)
                    {
                        result = EmptyNode;
                    }
                    else if (_right.IsEmpty && !_left.IsEmpty)
                    {
                        result = _left;
                    }
                    else if (!_right.IsEmpty && _left.IsEmpty)
                    {
                        result = _right;
                    }
                    else
                    {
                        // We have two children. Remove the next-highest node and replace
                        // this node with it.
                        var successor = _right;
                        while (!successor._left.IsEmpty)
                        {
                            successor = successor._left;
                        }

                        var newRight = _right.RemoveAt(0);
                        result = successor.Mutate(left: _left, right: newRight);
                    }
                }
                else if (index < _left._count)
                {
                    var newLeft = _left.RemoveAt(index);
                    result = this.Mutate(left: newLeft);
                }
                else
                {
                    var newRight = _right.RemoveAt(index - _left._count - 1);
                    result = this.Mutate(right: newRight);
                }

                return result.IsEmpty ? result : MakeBalanced(result);
            }

            /// <summary>
            /// Removes all the elements that match the conditions defined by the specified
            /// predicate.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
            /// to remove.
            /// </param>
            /// <returns>
            /// The new node tree.
            /// </returns>
            internal Node RemoveAll(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Contract.Ensures(Contract.Result<Node>() != null);

                var result = this;
                int index = 0;
                foreach (var item in this)
                {
                    if (match(item))
                    {
                        result = result.RemoveAt(index);
                    }
                    else
                    {
                        index++;
                    }
                }

                return result;
            }

            /// <summary>
            /// Replaces a value at a given index.
            /// </summary>
            /// <param name="index">The location for the new value.</param>
            /// <param name="value">The new value for the node.</param>
            /// <returns>The new tree.</returns>
            internal Node ReplaceAt(int index, T value)
            {
                Requires.Range(index >= 0 && index < this.Count, "index");

                Node result = this;
                if (index == _left._count)
                {
                    // We have a match. 
                    result = this.Mutate(value);
                }
                else if (index < _left._count)
                {
                    var newLeft = _left.ReplaceAt(index, value);
                    result = this.Mutate(left: newLeft);
                }
                else
                {
                    var newRight = _right.ReplaceAt(index - _left._count - 1, value);
                    result = this.Mutate(right: newRight);
                }

                return result;
            }

            /// <summary>
            /// Reverses the order of the elements in the entire <see cref="ImmutableList{T}"/>.
            /// </summary>
            /// <returns>The reversed list.</returns>
            internal Node Reverse()
            {
                Contract.Ensures(Contract.Result<Node>() != null);
                return this.Reverse(0, this.Count);
            }

            /// <summary>
            /// Reverses the order of the elements in the specified range.
            /// </summary>
            /// <param name="index">The zero-based starting index of the range to reverse.</param>
            /// <param name="count">The number of elements in the range to reverse.</param> 
            /// <returns>The reversed list.</returns>
            internal Node Reverse(int index, int count)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Range(index + count <= this.Count, "index");

                Node result = this;
                int start = index;
                int end = index + count - 1;
                while (start < end)
                {
                    T a = result[start];
                    T b = result[end];
                    result = result
                        .ReplaceAt(end, a)
                        .ReplaceAt(start, b);
                    start++;
                    end--;
                }

                return result;
            }

            /// <summary>
            /// Sorts the elements in the entire <see cref="ImmutableList{T}"/> using
            /// the default comparer.
            /// </summary>
            internal Node Sort()
            {
                Contract.Ensures(Contract.Result<Node>() != null);
                return this.Sort(Comparer<T>.Default);
            }

            /// <summary>
            /// Sorts the elements in the entire <see cref="ImmutableList{T}"/> using
            /// the specified <see cref="Comparison{T}"/>.
            /// </summary>
            /// <param name="comparison">
            /// The <see cref="Comparison{T}"/> to use when comparing elements.
            /// </param>
            /// <returns>The sorted list.</returns>
            internal Node Sort(Comparison<T> comparison)
            {
                Requires.NotNull(comparison, "comparison");
                Contract.Ensures(Contract.Result<Node>() != null);

                // PERF: Eventually this might be reimplemented in a way that does not require allocating an array.
                var array = new T[this.Count];
                this.CopyTo(array);
                Array.Sort(array, comparison);
                return NodeTreeFromList(array.AsOrderedCollection(), 0, this.Count);
            }

            /// <summary>
            /// Sorts the elements in the entire <see cref="ImmutableList{T}"/> using
            /// the specified comparer.
            /// </summary>
            /// <param name="comparer">
            /// The <see cref="IComparer{T}"/> implementation to use when comparing
            /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
            /// </param>
            /// <returns>The sorted list.</returns>
            internal Node Sort(IComparer<T> comparer)
            {
                Requires.NotNull(comparer, "comparer");
                Contract.Ensures(Contract.Result<Node>() != null);
                return this.Sort(0, this.Count, comparer);
            }

            /// <summary>
            /// Sorts the elements in a range of elements in <see cref="ImmutableList{T}"/>
            /// using the specified comparer.
            /// </summary>
            /// <param name="index">
            /// The zero-based starting index of the range to sort.
            /// </param>
            /// <param name="count">
            /// The length of the range to sort.
            /// </param>
            /// <param name="comparer">
            /// The <see cref="IComparer{T}"/> implementation to use when comparing
            /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
            /// </param>
            /// <returns>The sorted list.</returns>
            internal Node Sort(int index, int count, IComparer<T> comparer)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Argument(index + count <= this.Count);
                Requires.NotNull(comparer, "comparer");

                // PERF: Eventually this might be reimplemented in a way that does not require allocating an array.
                var array = new T[this.Count];
                this.CopyTo(array);
                Array.Sort(array, index, count, comparer);
                return NodeTreeFromList(array.AsOrderedCollection(), 0, this.Count);
            }

            /// <summary>
            /// Searches a range of elements in the sorted <see cref="ImmutableList{T}"/>
            /// for an element using the specified comparer and returns the zero-based index
            /// of the element.
            /// </summary>
            /// <param name="index">The zero-based starting index of the range to search.</param>
            /// <param name="count"> The length of the range to search.</param>
            /// <param name="item">The object to locate. The value can be null for reference types.</param>
            /// <param name="comparer">
            /// The <see cref="IComparer{T}"/> implementation to use when comparing
            /// elements, or null to use the default comparer <see cref="Comparer{T}.Default"/>.
            /// </param>
            /// <returns>
            /// The zero-based index of item in the sorted <see cref="ImmutableList{T}"/>,
            /// if item is found; otherwise, a negative number that is the bitwise complement
            /// of the index of the next element that is larger than item or, if there is
            /// no larger element, the bitwise complement of <see cref="ImmutableList{T}.Count"/>.
            /// </returns>
            /// <exception cref="ArgumentOutOfRangeException">
            /// <paramref name="index"/> is less than 0.-or-<paramref name="count"/> is less than 0.
            /// </exception>
            /// <exception cref="ArgumentException">
            /// <paramref name="index"/> and <paramref name="count"/> do not denote a valid range in the <see cref="ImmutableList{T}"/>.
            /// </exception>
            /// <exception cref="InvalidOperationException">
            /// <paramref name="comparer"/> is null, and the default comparer <see cref="Comparer{T}.Default"/>
            /// cannot find an implementation of the <see cref="IComparable{T}"/> generic interface
            /// or the <see cref="IComparable"/> interface for type <typeparamref name="T"/>.
            /// </exception>
            internal int BinarySearch(int index, int count, T item, IComparer<T> comparer)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                comparer = comparer ?? Comparer<T>.Default;

                if (this.IsEmpty || count <= 0)
                {
                    return ~index;
                }

                // If this node is not within range, defer to either branch as appropriate.
                int thisNodeIndex = _left.Count; // this is only the index within the AVL tree, treating this node as root rather than a member of a larger tree.
                if (index + count <= thisNodeIndex)
                {
                    return _left.BinarySearch(index, count, item, comparer);
                }
                else if (index > thisNodeIndex)
                {
                    int result = _right.BinarySearch(index - thisNodeIndex - 1, count, item, comparer);
                    int offset = thisNodeIndex + 1;
                    return result < 0 ? result - offset : result + offset;
                }

                // We're definitely in the caller's designated range now. 
                // Any possible match will be a descendant of this node (or this immediate one).
                // Some descendants may not be in range, but if we hit any it means no match was found,
                // and a negative response would come back from the above code to the below code.
                int compare = comparer.Compare(item, _key);
                if (compare == 0)
                {
                    return thisNodeIndex;
                }
                else if (compare > 0)
                {
                    int adjustedCount = count - (thisNodeIndex - index) - 1;
                    int result = adjustedCount < 0 ? -1 : _right.BinarySearch(0, adjustedCount, item, comparer);
                    int offset = thisNodeIndex + 1;
                    return result < 0 ? result - offset : result + offset;
                }
                else
                {
                    if (index == thisNodeIndex)
                    {
                        // We can't go any further left.
                        return ~index;
                    }

                    int result = _left.BinarySearch(index, count, item, comparer);
                    //return result < 0 ? result - thisNodeIndex : result + thisNodeIndex;
                    return result;
                }
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// first occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
            /// that starts at the specified index and contains the specified number of elements.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="equalityComparer">The equality comparer to use for testing the match of two elements.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of <paramref name="item"/> within the entire
            /// <see cref="ImmutableList{T}"/>, if found; otherwise, -1.
            /// </returns>
            [Pure]
            internal int IndexOf(T item, IEqualityComparer<T> equalityComparer)
            {
                return this.IndexOf(item, 0, this.Count, equalityComparer);
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// first occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
            /// that starts at the specified index and contains the specified number of elements.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="index">
            /// The zero-based starting index of the search. 0 (zero) is valid in an empty
            /// list.
            /// </param>
            /// <param name="count">
            /// The number of elements in the section to search.
            /// </param>
            /// <param name="equalityComparer">The equality comparer to use for testing the match of two elements.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of <paramref name="item"/> within the range of
            /// elements in the <see cref="ImmutableList{T}"/> that starts at <paramref name="index"/> and
            /// contains <paramref name="count"/> number of elements, if found; otherwise, -1.
            /// </returns>
            [Pure]
            internal int IndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Range(count <= this.Count, "count");
                Requires.Range(index + count <= this.Count, "count");
                Requires.NotNull(equalityComparer, "equalityComparer");

                using (var enumerator = new Enumerator(this, startIndex: index, count: count))
                {
                    while (enumerator.MoveNext())
                    {
                        if (equalityComparer.Equals(item, enumerator.Current))
                        {
                            return index;
                        }

                        index++;
                    }
                }

                return -1;
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// last occurrence within the range of elements in the <see cref="ImmutableList{T}"/>
            /// that contains the specified number of elements and ends at the specified
            /// index.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="index">The zero-based starting index of the backward search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <param name="equalityComparer">The equality comparer to use for testing the match of two elements.</param>
            /// <returns>
            /// The zero-based index of the last occurrence of <paramref name="item"/> within the range of elements
            /// in the <see cref="ImmutableList{T}"/> that contains <paramref name="count"/> number of elements
            /// and ends at <paramref name="index"/>, if found; otherwise, -1.
            /// </returns>
            [Pure]
            internal int LastIndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer)
            {
                Requires.NotNull(equalityComparer, "ValueComparer");
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0 && count <= this.Count, "count");
                Requires.Argument(index - count + 1 >= 0);

                using (var enumerator = new Enumerator(this, startIndex: index, count: count, reversed: true))
                {
                    while (enumerator.MoveNext())
                    {
                        if (equalityComparer.Equals(item, enumerator.Current))
                        {
                            return index;
                        }

                        index--;
                    }
                }

                return -1;
            }

            /// <summary>
            /// Copies the entire <see cref="ImmutableList{T}"/> to a compatible one-dimensional
            /// array, starting at the beginning of the target array.
            /// </summary>
            /// <param name="array">
            /// The one-dimensional <see cref="Array"/> that is the destination of the elements
            /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
            /// zero-based indexing.
            /// </param>
            internal void CopyTo(T[] array)
            {
                Requires.NotNull(array, "array");
                Requires.Argument(array.Length >= this.Count);

                int index = 0;
                foreach (var element in this)
                {
                    array[index++] = element;
                }
            }

            /// <summary>
            /// Copies the entire <see cref="ImmutableList{T}"/> to a compatible one-dimensional
            /// array, starting at the specified index of the target array.
            /// </summary>
            /// <param name="array">
            /// The one-dimensional <see cref="Array"/> that is the destination of the elements
            /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
            /// zero-based indexing.
            /// </param>
            /// <param name="arrayIndex">
            /// The zero-based index in <paramref name="array"/> at which copying begins.
            /// </param>
            internal void CopyTo(T[] array, int arrayIndex)
            {
                Requires.NotNull(array, "array");
                Requires.Range(arrayIndex >= 0, "arrayIndex");
                Requires.Range(arrayIndex <= array.Length, "arrayIndex");
                Requires.Argument(arrayIndex + this.Count <= array.Length);

                foreach (var element in this)
                {
                    array[arrayIndex++] = element;
                }
            }

            /// <summary>
            /// Copies a range of elements from the <see cref="ImmutableList{T}"/> to
            /// a compatible one-dimensional array, starting at the specified index of the
            /// target array.
            /// </summary>
            /// <param name="index">
            /// The zero-based index in the source <see cref="ImmutableList{T}"/> at
            /// which copying begins.
            /// </param>
            /// <param name="array">
            /// The one-dimensional <see cref="Array"/> that is the destination of the elements
            /// copied from <see cref="ImmutableList{T}"/>. The <see cref="Array"/> must have
            /// zero-based indexing.
            /// </param>
            /// <param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param>
            /// <param name="count">The number of elements to copy.</param>
            internal void CopyTo(int index, T[] array, int arrayIndex, int count)
            {
                Requires.NotNull(array, "array");
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Range(index + count <= this.Count, "count");
                Requires.Range(arrayIndex >= 0, "arrayIndex");
                Requires.Range(arrayIndex + count <= array.Length, "arrayIndex");

                using (var enumerator = new Enumerator(this, startIndex: index, count: count))
                {
                    while (enumerator.MoveNext())
                    {
                        array[arrayIndex++] = enumerator.Current;
                    }
                }
            }

            /// <summary>
            /// Copies the elements of the <see cref="ICollection"/> to an <see cref="Array"/>, starting at a particular <see cref="Array"/> index.
            /// </summary>
            /// <param name="array">The one-dimensional <see cref="Array"/> that is the destination of the elements copied from <see cref="ICollection"/>. The <see cref="Array"/> must have zero-based indexing.</param>
            /// <param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param>
            internal void CopyTo(Array array, int arrayIndex)
            {
                Requires.NotNull(array, "array");
                Requires.Range(arrayIndex >= 0, "arrayIndex");
                Requires.Range(array.Length >= arrayIndex + this.Count, "arrayIndex");

                foreach (var element in this)
                {
                    array.SetValue(element, arrayIndex++);
                }
            }

            /// <summary>
            /// Converts the elements in the current <see cref="ImmutableList{T}"/> to
            /// another type, and returns a list containing the converted elements.
            /// </summary>
            /// <param name="converter">
            /// A <see cref="Func{T, TResult}"/> delegate that converts each element from
            /// one type to another type.
            /// </param>
            /// <typeparam name="TOutput">
            /// The type of the elements of the target array.
            /// </typeparam>
            /// <returns>
            /// A node tree with the transformed list.
            /// </returns>
            internal ImmutableList<TOutput>.Node ConvertAll<TOutput>(Func<T, TOutput> converter)
            {
                var root = ImmutableList<TOutput>.Node.EmptyNode;

                if (this.IsEmpty)
                {
                    return root;
                }

                return root.AddRange(Enumerable.Select(this, converter));
            }

            /// <summary>
            /// Determines whether every element in the <see cref="ImmutableList{T}"/>
            /// matches the conditions defined by the specified predicate.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions to check against
            /// the elements.
            /// </param>
            /// <returns>
            /// true if every element in the <see cref="ImmutableList{T}"/> matches the
            /// conditions defined by the specified predicate; otherwise, false. If the list
            /// has no elements, the return value is true.
            /// </returns>
            internal bool TrueForAll(Predicate<T> match)
            {
                foreach (var item in this)
                {
                    if (!match(item))
                    {
                        return false;
                    }
                }

                return true;
            }

            /// <summary>
            /// Determines whether the <see cref="ImmutableList{T}"/> contains elements
            /// that match the conditions defined by the specified predicate.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
            /// to search for.
            /// </param>
            /// <returns>
            /// true if the <see cref="ImmutableList{T}"/> contains one or more elements
            /// that match the conditions defined by the specified predicate; otherwise,
            /// false.
            /// </returns>
            internal bool Exists(Predicate<T> match)
            {
                Requires.NotNull(match, "match");

                foreach (T item in this)
                {
                    if (match(item))
                    {
                        return true;
                    }
                }

                return false;
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the first occurrence within the entire <see cref="ImmutableList{T}"/>.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The first element that matches the conditions defined by the specified predicate,
            /// if found; otherwise, the default value for type <typeparamref name="T"/>.
            /// </returns>
            internal T Find(Predicate<T> match)
            {
                Requires.NotNull(match, "match");

                foreach (var item in this)
                {
                    if (match(item))
                    {
                        return item;
                    }
                }

                return default(T);
            }

            /// <summary>
            /// Retrieves all the elements that match the conditions defined by the specified
            /// predicate.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
            /// to search for.
            /// </param>
            /// <returns>
            /// A <see cref="ImmutableList{T}"/> containing all the elements that match
            /// the conditions defined by the specified predicate, if found; otherwise, an
            /// empty <see cref="ImmutableList{T}"/>.
            /// </returns>
            internal ImmutableList<T> FindAll(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Contract.Ensures(Contract.Result<ImmutableList<T>>() != null);

                if (this.IsEmpty)
                {
                    return ImmutableList<T>.Empty;
                }

                List<T> list = null;
                foreach (var item in this)
                {
                    if (match(item))
                    {
                        if (list == null)
                        {
                            list = new List<T>();
                        }

                        list.Add(item);
                    }
                }

                return list != null ?
                    ImmutableList.CreateRange(list) :
                    ImmutableList<T>.Empty;
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the first occurrence within
            /// the entire <see cref="ImmutableList{T}"/>.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The zero-based index of the first occurrence of an element that matches the
            /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
            /// </returns>
            internal int FindIndex(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Contract.Ensures(Contract.Result<int>() >= -1);

                return this.FindIndex(0, _count, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the first occurrence within
            /// the range of elements in the <see cref="ImmutableList{T}"/> that extends
            /// from the specified index to the last element.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the search.</param>
            /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element to search for.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of an element that matches the
            /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
            /// </returns>
            internal int FindIndex(int startIndex, Predicate<T> match)
            {
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(startIndex <= this.Count, "startIndex");
                Requires.NotNull(match, "match");

                return this.FindIndex(startIndex, this.Count - startIndex, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the first occurrence within
            /// the range of elements in the <see cref="ImmutableList{T}"/> that starts
            /// at the specified index and contains the specified number of elements.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element to search for.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of an element that matches the
            /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
            /// </returns>
            internal int FindIndex(int startIndex, int count, Predicate<T> match)
            {
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(count >= 0, "count");
                Requires.Argument(startIndex + count <= this.Count);
                Requires.NotNull(match, "match");

                using (var enumerator = new Enumerator(this, startIndex: startIndex, count: count))
                {
                    int index = startIndex;
                    while (enumerator.MoveNext())
                    {
                        if (match(enumerator.Current))
                        {
                            return index;
                        }

                        index++;
                    }
                }

                return -1;
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the last occurrence within the entire <see cref="ImmutableList{T}"/>.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The last element that matches the conditions defined by the specified predicate,
            /// if found; otherwise, the default value for type <typeparamref name="T"/>.
            /// </returns>
            internal T FindLast(Predicate<T> match)
            {
                Requires.NotNull(match, "match");

                using (var enumerator = new Enumerator(this, reversed: true))
                {
                    while (enumerator.MoveNext())
                    {
                        if (match(enumerator.Current))
                        {
                            return enumerator.Current;
                        }
                    }
                }

                return default(T);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the last occurrence within
            /// the entire <see cref="ImmutableList{T}"/>.
            /// </summary>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The zero-based index of the last occurrence of an element that matches the
            /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
            /// </returns>
            internal int FindLastIndex(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Contract.Ensures(Contract.Result<int>() >= -1);

                if (this.IsEmpty)
                {
                    return -1;
                }

                return this.FindLastIndex(this.Count - 1, this.Count, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the last occurrence within
            /// the range of elements in the <see cref="ImmutableList{T}"/> that extends
            /// from the first element to the specified index.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <param name="match">The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
            /// to search for.</param>
            /// <returns>
            /// The zero-based index of the last occurrence of an element that matches the
            /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
            /// </returns>
            internal int FindLastIndex(int startIndex, Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(startIndex == 0 || startIndex < this.Count, "startIndex");

                if (this.IsEmpty)
                {
                    return -1;
                }

                return this.FindLastIndex(startIndex, startIndex + 1, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the last occurrence within
            /// the range of elements in the <see cref="ImmutableList{T}"/> that contains
            /// the specified number of elements and ends at the specified index.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <param name="match">
            /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The zero-based index of the last occurrence of an element that matches the
            /// conditions defined by <paramref name="match"/>, if found; otherwise, -1.
            /// </returns>
            internal int FindLastIndex(int startIndex, int count, Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(count <= this.Count, "count");
                Requires.Argument(startIndex - count + 1 >= 0);

                using (var enumerator = new Enumerator(this, startIndex: startIndex, count: count, reversed: true))
                {
                    int index = startIndex;
                    while (enumerator.MoveNext())
                    {
                        if (match(enumerator.Current))
                        {
                            return index;
                        }

                        index--;
                    }
                }

                return -1;
            }

            /// <summary>
            /// Freezes this node and all descendant nodes so that any mutations require a new instance of the nodes.
            /// </summary>
            internal void Freeze()
            {
                // If this node is frozen, all its descendants must already be frozen.
                if (!_frozen)
                {
                    _left.Freeze();
                    _right.Freeze();
                    _frozen = true;
                }
            }

            #region Tree balancing methods

            /// <summary>
            /// AVL rotate left operation.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>The rotated tree.</returns>
            private static Node RotateLeft(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                Contract.Ensures(Contract.Result<Node>() != null);

                if (tree._right.IsEmpty)
                {
                    return tree;
                }

                var right = tree._right;
                return right.Mutate(left: tree.Mutate(right: right._left));
            }

            /// <summary>
            /// AVL rotate right operation.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>The rotated tree.</returns>
            private static Node RotateRight(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                Contract.Ensures(Contract.Result<Node>() != null);

                if (tree._left.IsEmpty)
                {
                    return tree;
                }

                var left = tree._left;
                return left.Mutate(right: tree.Mutate(left: left._right));
            }

            /// <summary>
            /// AVL rotate double-left operation.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>The rotated tree.</returns>
            private static Node DoubleLeft(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                Contract.Ensures(Contract.Result<Node>() != null);

                if (tree._right.IsEmpty)
                {
                    return tree;
                }

                Node rotatedRightChild = tree.Mutate(right: RotateRight(tree._right));
                return RotateLeft(rotatedRightChild);
            }

            /// <summary>
            /// AVL rotate double-right operation.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>The rotated tree.</returns>
            private static Node DoubleRight(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                Contract.Ensures(Contract.Result<Node>() != null);

                if (tree._left.IsEmpty)
                {
                    return tree;
                }

                Node rotatedLeftChild = tree.Mutate(left: RotateLeft(tree._left));
                return RotateRight(rotatedLeftChild);
            }

            /// <summary>
            /// Returns a value indicating whether the tree is in balance.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>0 if the tree is in balance, a positive integer if the right side is heavy, or a negative integer if the left side is heavy.</returns>
            [Pure]
            private static int Balance(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);

                return tree._right._height - tree._left._height;
            }

            /// <summary>
            /// Determines whether the specified tree is right heavy.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>
            /// <c>true</c> if [is right heavy] [the specified tree]; otherwise, <c>false</c>.
            /// </returns>
            [Pure]
            private static bool IsRightHeavy(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                return Balance(tree) >= 2;
            }

            /// <summary>
            /// Determines whether the specified tree is left heavy.
            /// </summary>
            [Pure]
            private static bool IsLeftHeavy(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                return Balance(tree) <= -2;
            }

            /// <summary>
            /// Balances the specified tree.
            /// </summary>
            /// <param name="tree">The tree.</param>
            /// <returns>A balanced tree.</returns>
            [Pure]
            private static Node MakeBalanced(Node tree)
            {
                Requires.NotNull(tree, "tree");
                Debug.Assert(!tree.IsEmpty);
                Contract.Ensures(Contract.Result<Node>() != null);

                if (IsRightHeavy(tree))
                {
                    return Balance(tree._right) < 0 ? DoubleLeft(tree) : RotateLeft(tree);
                }

                if (IsLeftHeavy(tree))
                {
                    return Balance(tree._left) > 0 ? DoubleRight(tree) : RotateRight(tree);
                }

                return tree;
            }

            /// <summary>
            /// Balance the specified node.  Allows for a large imbalance between left and
            /// right nodes, but assumes left and right nodes are individually balanced.
            /// </summary>
            /// <param name="node">The node.</param>
            /// <returns>A balanced node</returns>
            private static Node BalanceNode(Node node)
            {
                while (IsRightHeavy(node) || IsLeftHeavy(node))
                {
                    if (IsRightHeavy(node))
                    {
                        node = Balance(node._right) < 0 ? DoubleLeft(node) : RotateLeft(node);
                        node.Mutate(left: BalanceNode(node._left));
                    }
                    else
                    {
                        node = Balance(node._left) > 0 ? DoubleRight(node) : RotateRight(node);
                        node.Mutate(right: BalanceNode(node._right));
                    }
                }

                return node;
            }

            #endregion

            /// <summary>
            /// Creates a node mutation, either by mutating this node (if not yet frozen) or by creating a clone of this node
            /// with the described changes.
            /// </summary>
            /// <param name="left">The left branch of the mutated node.</param>
            /// <param name="right">The right branch of the mutated node.</param>
            /// <returns>The mutated (or created) node.</returns>
            private Node Mutate(Node left = null, Node right = null)
            {
                if (_frozen)
                {
                    return new Node(_key, left ?? _left, right ?? _right);
                }
                else
                {
                    if (left != null)
                    {
                        _left = left;
                    }

                    if (right != null)
                    {
                        _right = right;
                    }

                    _height = checked((byte)(1 + Math.Max(_left._height, _right._height)));
                    _count = 1 + _left._count + _right._count;
                    return this;
                }
            }

            /// <summary>
            /// Creates a node mutation, either by mutating this node (if not yet frozen) or by creating a clone of this node
            /// with the described changes.
            /// </summary>
            /// <param name="value">The new value for this node.</param>
            /// <returns>The mutated (or created) node.</returns>
            private Node Mutate(T value)
            {
                if (_frozen)
                {
                    return new Node(value, _left, _right);
                }
                else
                {
                    _key = value;
                    return this;
                }
            }
        }

        /// <summary>
        /// A list that mutates with little or no memory allocations,
        /// can produce and/or build on immutable list instances very efficiently.
        /// </summary>
        /// <remarks>
        /// <para>
        /// While <see cref="ImmutableList{T}.AddRange"/> and other bulk change methods
        /// already provide fast bulk change operations on the collection, this class allows
        /// multiple combinations of changes to be made to a set with equal efficiency.
        /// </para>
        /// <para>
        /// Instance members of this class are <em>not</em> thread-safe.
        /// </para>
        /// </remarks>
        [SuppressMessage("Microsoft.Design", "CA1034:NestedTypesShouldNotBeVisible", Justification = "Ignored")]
        [SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix", Justification = "Ignored")]
        [DebuggerDisplay("Count = {Count}")]
        [DebuggerTypeProxy(typeof(ImmutableListBuilderDebuggerProxy<>))]
        public sealed class Builder : IList<T>, IList, IOrderedCollection<T>, IImmutableListQueries<T>, IReadOnlyList<T>
        {
            /// <summary>
            /// The binary tree used to store the contents of the list.  Contents are typically not entirely frozen.
            /// </summary>
            private Node _root = Node.EmptyNode;

            /// <summary>
            /// Caches an immutable instance that represents the current state of the collection.
            /// </summary>
            /// <value>Null if no immutable view has been created for the current version.</value>
            private ImmutableList<T> _immutable;

            /// <summary>
            /// A number that increments every time the builder changes its contents.
            /// </summary>
            private int _version;

            /// <summary>
            /// The object callers may use to synchronize access to this collection.
            /// </summary>
            private object _syncRoot;

            /// <summary>
            /// Initializes a new instance of the <see cref="Builder"/> class.
            /// </summary>
            /// <param name="list">A list to act as the basis for a new list.</param>
            internal Builder(ImmutableList<T> list)
            {
                Requires.NotNull(list, "list");
                _root = list._root;
                _immutable = list;
            }

            #region IList<T> Properties

            /// <summary>
            /// Gets the number of elements in this list.
            /// </summary>
            public int Count
            {
                get { return this.Root.Count; }
            }

            /// <summary>
            /// Gets a value indicating whether this instance is read-only.
            /// </summary>
            /// <value>Always <c>false</c>.</value>
            bool ICollection<T>.IsReadOnly
            {
                get { return false; }
            }

            #endregion

            /// <summary>
            /// Gets the current version of the contents of this builder.
            /// </summary>
            internal int Version
            {
                get { return _version; }
            }

            /// <summary>
            /// Gets or sets the root node that represents the data in this collection.
            /// </summary>
            internal Node Root
            {
                get
                {
                    return _root;
                }

                private set
                {
                    // We *always* increment the version number because some mutations
                    // may not create a new value of root, although the existing root
                    // instance may have mutated.
                    _version++;

                    if (_root != value)
                    {
                        _root = value;

                        // Clear any cached value for the immutable view since it is now invalidated.
                        _immutable = null;
                    }
                }
            }

            #region Indexers

            /// <summary>
            /// Gets or sets the value for a given index into the list.
            /// </summary>
            /// <param name="index">The index of the desired element.</param>
            /// <returns>The value at the specified index.</returns>
            [SuppressMessage("Microsoft.Usage", "CA2233:OperationsShouldNotOverflow", MessageId = "index+1", Justification = "There is no chance of this overflowing")]
            public T this[int index]
            {
                get
                {
                    return this.Root[index];
                }

                set
                {
                    this.Root = this.Root.ReplaceAt(index, value);
                }
            }

            /// <summary>
            /// Gets the element in the collection at a given index.
            /// </summary>
            T IOrderedCollection<T>.this[int index]
            {
                get
                {
                    return this[index];
                }
            }

            #endregion

            #region IList<T> Methods

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public int IndexOf(T item)
            {
                return this.Root.IndexOf(item, EqualityComparer<T>.Default);
            }

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public void Insert(int index, T item)
            {
                this.Root = this.Root.Insert(index, item);
            }

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public void RemoveAt(int index)
            {
                this.Root = this.Root.RemoveAt(index);
            }

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public void Add(T item)
            {
                this.Root = this.Root.Add(item);
            }

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public void Clear()
            {
                this.Root = ImmutableList<T>.Node.EmptyNode;
            }

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public bool Contains(T item)
            {
                return this.IndexOf(item) >= 0;
            }

            /// <summary>
            /// See <see cref="IList{T}"/>
            /// </summary>
            public bool Remove(T item)
            {
                int index = this.IndexOf(item);
                if (index < 0)
                {
                    return false;
                }

                this.Root = this.Root.RemoveAt(index);
                return true;
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            public ImmutableList<T>.Enumerator GetEnumerator()
            {
                return this.Root.GetEnumerator(this);
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            IEnumerator<T> IEnumerable<T>.GetEnumerator()
            {
                return this.GetEnumerator();
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            IEnumerator IEnumerable.GetEnumerator()
            {
                return this.GetEnumerator();
            }

            #endregion

            // --- START of EXTENSION to MICROSOFT code ---
            // --- Methods added for TypeCobol ---

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <param name="startIndex">The index of the first element to enumerate.</param>
            /// <param name="count">The number of elements in this collection.</param>
            /// <param name="reversed"><c>true</c> if the list should be enumerated in reverse order.</param>
            public IEnumerator<T> GetEnumerator(int startIndex, int count, bool reversed)
            {
                return this.Root.GetEnumerator(startIndex, count, reversed);
            }

            /// <summary>
            /// Searches for the specified object around the initial index of the object 
            /// in the first version of the immutable list.
            /// This method is useful only when the two following conditions are true :
            /// - the searched item is present only once in the list
            /// - the probability that the searched item did not move far from its initial position is very high
            /// </summary>
            /// <param name="item">
            /// The object to locate in the <see cref="ImmutableList{T}"/>. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="initialIndex">
            /// The zero-based index of the serached object in the first version of the immutable list.
            /// </param>
            /// <returns>
            /// The zero-based index of the first occurrence of <paramref name="item"/> within the range of
            /// elements in the <see cref="ImmutableList{T}"/> that starts at <paramref name="index"/> and
            /// contains <paramref name="count"/> number of elements, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int IndexOf(object item, int initialIndex)
            {
                return this.Root.IndexOf((T)item, initialIndex);
            }

            // --- END of EXTENSION to MICROSOFT code ---

            #region IImmutableListQueries<T> methods

            /// <summary>
            /// Performs the specified action on each element of the list.
            /// </summary>
            /// <param name="action">The System.Action&lt;T&gt; delegate to perform on each element of the list.</param>
            public void ForEach(Action<T> action)
            {
                Requires.NotNull(action, "action");

                foreach (T item in this)
                {
                    action(item);
                }
            }

            /// <summary>
            /// Copies the entire ImmutableList&lt;T&gt; to a compatible one-dimensional
            /// array, starting at the beginning of the target array.
            /// </summary>
            /// <param name="array">
            /// The one-dimensional System.Array that is the destination of the elements
            /// copied from ImmutableList&lt;T&gt;. The System.Array must have
            /// zero-based indexing.
            /// </param>
            public void CopyTo(T[] array)
            {
                Requires.NotNull(array, "array");
                Requires.Range(array.Length >= this.Count, "array");
                _root.CopyTo(array);
            }

            /// <summary>
            /// Copies the entire ImmutableList&lt;T&gt; to a compatible one-dimensional
            /// array, starting at the specified index of the target array.
            /// </summary>
            /// <param name="array">
            /// The one-dimensional System.Array that is the destination of the elements
            /// copied from ImmutableList&lt;T&gt;. The System.Array must have
            /// zero-based indexing.
            /// </param>
            /// <param name="arrayIndex">
            /// The zero-based index in array at which copying begins.
            /// </param>
            public void CopyTo(T[] array, int arrayIndex)
            {
                Requires.NotNull(array, "array");
                Requires.Range(array.Length >= arrayIndex + this.Count, "arrayIndex");
                _root.CopyTo(array, arrayIndex);
            }

            /// <summary>
            /// Copies a range of elements from the ImmutableList&lt;T&gt; to
            /// a compatible one-dimensional array, starting at the specified index of the
            /// target array.
            /// </summary>
            /// <param name="index">
            /// The zero-based index in the source ImmutableList&lt;T&gt; at
            /// which copying begins.
            /// </param>
            /// <param name="array">
            /// The one-dimensional System.Array that is the destination of the elements
            /// copied from ImmutableList&lt;T&gt;. The System.Array must have
            /// zero-based indexing.
            /// </param>
            /// <param name="arrayIndex">The zero-based index in array at which copying begins.</param>
            /// <param name="count">The number of elements to copy.</param>
            public void CopyTo(int index, T[] array, int arrayIndex, int count)
            {
                _root.CopyTo(index, array, arrayIndex, count);
            }

            /// <summary>
            /// Creates a shallow copy of a range of elements in the source ImmutableList&lt;T&gt;.
            /// </summary>
            /// <param name="index">
            /// The zero-based ImmutableList&lt;T&gt; index at which the range
            /// starts.
            /// </param>
            /// <param name="count">
            /// The number of elements in the range.
            /// </param>
            /// <returns>
            /// A shallow copy of a range of elements in the source ImmutableList&lt;T&gt;.
            /// </returns>
            public ImmutableList<T> GetRange(int index, int count)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Range(index + count <= this.Count, "count");
                return ImmutableList<T>.WrapNode(Node.NodeTreeFromList(this, index, count));
            }

            /// <summary>
            /// Converts the elements in the current ImmutableList&lt;T&gt; to
            /// another type, and returns a list containing the converted elements.
            /// </summary>
            /// <param name="converter">
            /// A System.Converter&lt;TInput,TOutput&gt; delegate that converts each element from
            /// one type to another type.
            /// </param>
            /// <typeparam name="TOutput">
            /// The type of the elements of the target array.
            /// </typeparam>
            /// <returns>
            /// A ImmutableList&lt;T&gt; of the target type containing the converted
            /// elements from the current ImmutableList&lt;T&gt;.
            /// </returns>
            public ImmutableList<TOutput> ConvertAll<TOutput>(Func<T, TOutput> converter)
            {
                Requires.NotNull(converter, "converter");
                return ImmutableList<TOutput>.WrapNode(_root.ConvertAll(converter));
            }

            /// <summary>
            /// Determines whether the ImmutableList&lt;T&gt; contains elements
            /// that match the conditions defined by the specified predicate.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the elements
            /// to search for.
            /// </param>
            /// <returns>
            /// true if the ImmutableList&lt;T&gt; contains one or more elements
            /// that match the conditions defined by the specified predicate; otherwise,
            /// false.
            /// </returns>
            public bool Exists(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.Exists(match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the first occurrence within the entire ImmutableList&lt;T&gt;.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The first element that matches the conditions defined by the specified predicate,
            /// if found; otherwise, the default value for type T.
            /// </returns>
            public T Find(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.Find(match);
            }

            /// <summary>
            /// Retrieves all the elements that match the conditions defined by the specified
            /// predicate.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the elements
            /// to search for.
            /// </param>
            /// <returns>
            /// A ImmutableList&lt;T&gt; containing all the elements that match
            /// the conditions defined by the specified predicate, if found; otherwise, an
            /// empty ImmutableList&lt;T&gt;.
            /// </returns>
            public ImmutableList<T> FindAll(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.FindAll(match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the first occurrence within
            /// the entire ImmutableList&lt;T&gt;.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The zero-based index of the first occurrence of an element that matches the
            /// conditions defined by match, if found; otherwise, -1.
            /// </returns>
            public int FindIndex(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.FindIndex(match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the first occurrence within
            /// the range of elements in the ImmutableList&lt;T&gt; that extends
            /// from the specified index to the last element.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the search.</param>
            /// <param name="match">The System.Predicate&lt;T&gt; delegate that defines the conditions of the element to search for.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of an element that matches the
            /// conditions defined by match, if found; otherwise, -1.
            /// </returns>
            public int FindIndex(int startIndex, Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(startIndex <= this.Count, "startIndex");
                return _root.FindIndex(startIndex, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the first occurrence within
            /// the range of elements in the ImmutableList&lt;T&gt; that starts
            /// at the specified index and contains the specified number of elements.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <param name="match">The System.Predicate&lt;T&gt; delegate that defines the conditions of the element to search for.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of an element that matches the
            /// conditions defined by match, if found; otherwise, -1.
            /// </returns>
            public int FindIndex(int startIndex, int count, Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(count >= 0, "count");
                Requires.Range(startIndex + count <= this.Count, "count");

                return _root.FindIndex(startIndex, count, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the last occurrence within the entire ImmutableList&lt;T&gt;.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The last element that matches the conditions defined by the specified predicate,
            /// if found; otherwise, the default value for type T.
            /// </returns>
            public T FindLast(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.FindLast(match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the last occurrence within
            /// the entire ImmutableList&lt;T&gt;.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The zero-based index of the last occurrence of an element that matches the
            /// conditions defined by match, if found; otherwise, -1.
            /// </returns>
            public int FindLastIndex(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.FindLastIndex(match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the last occurrence within
            /// the range of elements in the ImmutableList&lt;T&gt; that extends
            /// from the first element to the specified index.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <param name="match">The System.Predicate&lt;T&gt; delegate that defines the conditions of the element
            /// to search for.</param>
            /// <returns>
            /// The zero-based index of the last occurrence of an element that matches the
            /// conditions defined by match, if found; otherwise, -1.
            /// </returns>
            public int FindLastIndex(int startIndex, Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(startIndex == 0 || startIndex < this.Count, "startIndex");
                return _root.FindLastIndex(startIndex, match);
            }

            /// <summary>
            /// Searches for an element that matches the conditions defined by the specified
            /// predicate, and returns the zero-based index of the last occurrence within
            /// the range of elements in the ImmutableList&lt;T&gt; that contains
            /// the specified number of elements and ends at the specified index.
            /// </summary>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the element
            /// to search for.
            /// </param>
            /// <returns>
            /// The zero-based index of the last occurrence of an element that matches the
            /// conditions defined by match, if found; otherwise, -1.
            /// </returns>
            public int FindLastIndex(int startIndex, int count, Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                Requires.Range(startIndex >= 0, "startIndex");
                Requires.Range(count <= this.Count, "count");
                Requires.Range(startIndex - count + 1 >= 0, "startIndex");

                return _root.FindLastIndex(startIndex, count, match);
            }

            /* UPDATE to MICROSOFT code : conflicts with method added for TypeCobol 

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// first occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that extends from the specified index to the last element.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="index">
            /// The zero-based starting index of the search. 0 (zero) is valid in an empty
            /// list.
            /// </param>
            /// <returns>
            /// The zero-based index of the first occurrence of item within the range of
            /// elements in the ImmutableList&lt;T&gt; that extends from index
            /// to the last element, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int IndexOf(T item, int index)
            {
                return _root.IndexOf(item, index, this.Count - index, EqualityComparer<T>.Default);
            }
            */

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// first occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that starts at the specified index and contains the specified number of elements.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="index">
            /// The zero-based starting index of the search. 0 (zero) is valid in an empty
            /// list.
            /// </param>
            /// <param name="count">
            /// The number of elements in the section to search.
            /// </param>
            /// <returns>
            /// The zero-based index of the first occurrence of item within the range of
            /// elements in the ImmutableList&lt;T&gt; that starts at index and
            /// contains count number of elements, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int IndexOf(T item, int index, int count)
            {
                return _root.IndexOf(item, index, count, EqualityComparer<T>.Default);
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// first occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that starts at the specified index and contains the specified number of elements.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="index">
            /// The zero-based starting index of the search. 0 (zero) is valid in an empty
            /// list.
            /// </param>
            /// <param name="count">
            /// The number of elements in the section to search.
            /// </param>
            /// <param name="equalityComparer">The equality comparer to use in the search.</param>
            /// <returns>
            /// The zero-based index of the first occurrence of item within the range of
            /// elements in the ImmutableList&lt;T&gt; that starts at index and
            /// contains count number of elements, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int IndexOf(T item, int index, int count, IEqualityComparer<T> equalityComparer)
            {
                Requires.NotNull(equalityComparer, "equalityComparer");

                return _root.IndexOf(item, index, count, equalityComparer);
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// last occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that contains the specified number of elements and ends at the specified
            /// index.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <returns>
            /// The zero-based index of the last occurrence of item within the range of elements
            /// in the ImmutableList&lt;T&gt; that contains count number of elements
            /// and ends at index, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int LastIndexOf(T item)
            {
                if (this.Count == 0)
                {
                    return -1;
                }

                return _root.LastIndexOf(item, this.Count - 1, this.Count, EqualityComparer<T>.Default);
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// last occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that contains the specified number of elements and ends at the specified
            /// index.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <returns>
            /// The zero-based index of the last occurrence of item within the range of elements
            /// in the ImmutableList&lt;T&gt; that contains count number of elements
            /// and ends at index, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int LastIndexOf(T item, int startIndex)
            {
                if (this.Count == 0 && startIndex == 0)
                {
                    return -1;
                }

                return _root.LastIndexOf(item, startIndex, startIndex + 1, EqualityComparer<T>.Default);
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// last occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that contains the specified number of elements and ends at the specified
            /// index.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <returns>
            /// The zero-based index of the last occurrence of item within the range of elements
            /// in the ImmutableList&lt;T&gt; that contains count number of elements
            /// and ends at index, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int LastIndexOf(T item, int startIndex, int count)
            {
                return _root.LastIndexOf(item, startIndex, count, EqualityComparer<T>.Default);
            }

            /// <summary>
            /// Searches for the specified object and returns the zero-based index of the
            /// last occurrence within the range of elements in the ImmutableList&lt;T&gt;
            /// that contains the specified number of elements and ends at the specified
            /// index.
            /// </summary>
            /// <param name="item">
            /// The object to locate in the ImmutableList&lt;T&gt;. The value
            /// can be null for reference types.
            /// </param>
            /// <param name="startIndex">The zero-based starting index of the backward search.</param>
            /// <param name="count">The number of elements in the section to search.</param>
            /// <param name="equalityComparer">The equality comparer to use in the search.</param>
            /// <returns>
            /// The zero-based index of the last occurrence of item within the range of elements
            /// in the ImmutableList&lt;T&gt; that contains count number of elements
            /// and ends at index, if found; otherwise, -1.
            /// </returns>
            [Pure]
            public int LastIndexOf(T item, int startIndex, int count, IEqualityComparer<T> equalityComparer)
            {
                return _root.LastIndexOf(item, startIndex, count, equalityComparer);
            }

            /// <summary>
            /// Determines whether every element in the ImmutableList&lt;T&gt;
            /// matches the conditions defined by the specified predicate.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions to check against
            /// the elements.
            /// </param>
            /// <returns>
            /// true if every element in the ImmutableList&lt;T&gt; matches the
            /// conditions defined by the specified predicate; otherwise, false. If the list
            /// has no elements, the return value is true.
            /// </returns>
            public bool TrueForAll(Predicate<T> match)
            {
                Requires.NotNull(match, "match");
                return _root.TrueForAll(match);
            }

            #endregion

            #region Public methods

            /// <summary>
            /// Adds the elements of a sequence to the end of this collection.
            /// </summary>
            /// <param name="items">
            /// The sequence whose elements should be appended to this collection.
            /// The sequence itself cannot be null, but it can contain elements that are
            /// null, if type <typeparamref name="T"/> is a reference type.
            /// </param>
            public void AddRange(IEnumerable<T> items)
            {
                Requires.NotNull(items, "items");

                this.Root = this.Root.AddRange(items);
            }

            /// <summary>
            /// Inserts the elements of a collection into the ImmutableList&lt;T&gt;
            /// at the specified index.
            /// </summary>
            /// <param name="index">
            /// The zero-based index at which the new elements should be inserted.
            /// </param>
            /// <param name="items">
            /// The collection whose elements should be inserted into the ImmutableList&lt;T&gt;.
            /// The collection itself cannot be null, but it can contain elements that are
            /// null, if type T is a reference type.
            /// </param>
            public void InsertRange(int index, IEnumerable<T> items)
            {
                Requires.Range(index >= 0 && index <= this.Count, "index");
                Requires.NotNull(items, "items");

                this.Root = this.Root.InsertRange(index, items);
            }

            /// <summary>
            /// Removes all the elements that match the conditions defined by the specified
            /// predicate.
            /// </summary>
            /// <param name="match">
            /// The System.Predicate&lt;T&gt; delegate that defines the conditions of the elements
            /// to remove.
            /// </param>
            /// <returns>
            /// The number of elements removed from the ImmutableList&lt;T&gt;
            /// </returns>
            public int RemoveAll(Predicate<T> match)
            {
                Requires.NotNull(match, "match");

                int count = this.Count;
                this.Root = this.Root.RemoveAll(match);
                return count - this.Count;
            }

            /// <summary>
            /// Reverses the order of the elements in the entire ImmutableList&lt;T&gt;.
            /// </summary>
            public void Reverse()
            {
                this.Reverse(0, this.Count);
            }

            /// <summary>
            /// Reverses the order of the elements in the specified range.
            /// </summary>
            /// <param name="index">The zero-based starting index of the range to reverse.</param>
            /// <param name="count">The number of elements in the range to reverse.</param> 
            public void Reverse(int index, int count)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Range(index + count <= this.Count, "count");

                this.Root = this.Root.Reverse(index, count);
            }

            /// <summary>
            /// Sorts the elements in the entire ImmutableList&lt;T&gt; using
            /// the default comparer.
            /// </summary>
            public void Sort()
            {
                this.Root = this.Root.Sort();
            }

            /// <summary>
            /// Sorts the elements in the entire ImmutableList&lt;T&gt; using
            /// the specified System.Comparison&lt;T&gt;.
            /// </summary>
            /// <param name="comparison">
            /// The System.Comparison&lt;T&gt; to use when comparing elements.
            /// </param>
            public void Sort(Comparison<T> comparison)
            {
                Requires.NotNull(comparison, "comparison");
                this.Root = this.Root.Sort(comparison);
            }

            /// <summary>
            /// Sorts the elements in the entire ImmutableList&lt;T&gt; using
            /// the specified comparer.
            /// </summary>
            /// <param name="comparer">
            /// The System.Collections.Generic.IComparer&lt;T&gt; implementation to use when comparing
            /// elements, or null to use the default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default.
            /// </param>
            public void Sort(IComparer<T> comparer)
            {
                Requires.NotNull(comparer, "comparer");
                this.Root = this.Root.Sort(comparer);
            }

            /// <summary>
            /// Sorts the elements in a range of elements in ImmutableList&lt;T&gt;
            /// using the specified comparer.
            /// </summary>
            /// <param name="index">
            /// The zero-based starting index of the range to sort.
            /// </param>
            /// <param name="count">
            /// The length of the range to sort.
            /// </param>
            /// <param name="comparer">
            /// The System.Collections.Generic.IComparer&lt;T&gt; implementation to use when comparing
            /// elements, or null to use the default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default.
            /// </param>
            public void Sort(int index, int count, IComparer<T> comparer)
            {
                Requires.Range(index >= 0, "index");
                Requires.Range(count >= 0, "count");
                Requires.Range(index + count <= this.Count, "count");
                Requires.NotNull(comparer, "comparer");
                this.Root = this.Root.Sort(index, count, comparer);
            }

            /// <summary>
            /// Searches the entire sorted System.Collections.Generic.List&lt;T&gt; for an element
            /// using the default comparer and returns the zero-based index of the element.
            /// </summary>
            /// <param name="item">The object to locate. The value can be null for reference types.</param>
            /// <returns>
            /// The zero-based index of item in the sorted System.Collections.Generic.List&lt;T&gt;,
            /// if item is found; otherwise, a negative number that is the bitwise complement
            /// of the index of the next element that is larger than item or, if there is
            /// no larger element, the bitwise complement of System.Collections.Generic.List&lt;T&gt;.Count.
            /// </returns>
            /// <exception cref="InvalidOperationException">
            /// The default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default cannot
            /// find an implementation of the System.IComparable&lt;T&gt; generic interface or
            /// the System.IComparable interface for type T.
            /// </exception>
            public int BinarySearch(T item)
            {
                return this.BinarySearch(item, null);
            }

            /// <summary>
            ///  Searches the entire sorted System.Collections.Generic.List&lt;T&gt; for an element
            ///  using the specified comparer and returns the zero-based index of the element.
            /// </summary>
            /// <param name="item">The object to locate. The value can be null for reference types.</param>
            /// <param name="comparer">
            /// The System.Collections.Generic.IComparer&lt;T&gt; implementation to use when comparing
            /// elements.-or-null to use the default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default.
            /// </param>
            /// <returns>
            /// The zero-based index of item in the sorted System.Collections.Generic.List&lt;T&gt;,
            /// if item is found; otherwise, a negative number that is the bitwise complement
            /// of the index of the next element that is larger than item or, if there is
            /// no larger element, the bitwise complement of System.Collections.Generic.List&lt;T&gt;.Count.
            /// </returns>
            /// <exception cref="InvalidOperationException">
            /// comparer is null, and the default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default
            /// cannot find an implementation of the System.IComparable&lt;T&gt; generic interface
            /// or the System.IComparable interface for type T.
            /// </exception>
            public int BinarySearch(T item, IComparer<T> comparer)
            {
                return this.BinarySearch(0, this.Count, item, comparer);
            }

            /// <summary>
            /// Searches a range of elements in the sorted System.Collections.Generic.List&lt;T&gt;
            /// for an element using the specified comparer and returns the zero-based index
            /// of the element.
            /// </summary>
            /// <param name="index">The zero-based starting index of the range to search.</param>
            /// <param name="count"> The length of the range to search.</param>
            /// <param name="item">The object to locate. The value can be null for reference types.</param>
            /// <param name="comparer">
            /// The System.Collections.Generic.IComparer&lt;T&gt; implementation to use when comparing
            /// elements, or null to use the default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default.
            /// </param>
            /// <returns>
            /// The zero-based index of item in the sorted System.Collections.Generic.List&lt;T&gt;,
            /// if item is found; otherwise, a negative number that is the bitwise complement
            /// of the index of the next element that is larger than item or, if there is
            /// no larger element, the bitwise complement of System.Collections.Generic.List&lt;T&gt;.Count.
            /// </returns>
            /// <exception cref="ArgumentOutOfRangeException">
            /// index is less than 0.-or-count is less than 0.
            /// </exception>
            /// <exception cref="ArgumentException">
            /// index and count do not denote a valid range in the System.Collections.Generic.List&lt;T&gt;.
            /// </exception>
            /// <exception cref="InvalidOperationException">
            /// comparer is null, and the default comparer System.Collections.Generic.Comparer&lt;T&gt;.Default
            /// cannot find an implementation of the System.IComparable&lt;T&gt; generic interface
            /// or the System.IComparable interface for type T.
            /// </exception>
            public int BinarySearch(int index, int count, T item, IComparer<T> comparer)
            {
                return this.Root.BinarySearch(index, count, item, comparer);
            }

            /// <summary>
            /// Creates an immutable list based on the contents of this instance.
            /// </summary>
            /// <returns>An immutable list.</returns>
            /// <remarks>
            /// This method is an O(n) operation, and approaches O(1) time as the number of
            /// actual mutations to the set since the last call to this method approaches 0.
            /// </remarks>
            public ImmutableList<T> ToImmutable()
            {
                // Creating an instance of ImmutableList<T> with our root node automatically freezes our tree,
                // ensuring that the returned instance is immutable.  Any further mutations made to this builder
                // will clone (and unfreeze) the spine of modified nodes until the next time this method is invoked.
                if (_immutable == null)
                {
                    _immutable = ImmutableList<T>.WrapNode(this.Root);
                }

                return _immutable;
            }

            #endregion

            #region IList members

            /// <summary>
            /// Adds an item to the <see cref="IList"/>.
            /// </summary>
            /// <param name="value">The object to add to the <see cref="IList"/>.</param>
            /// <returns>
            /// The position into which the new element was inserted, or -1 to indicate that the item was not inserted into the collection,
            /// </returns>
            /// <exception cref="System.NotImplementedException"></exception>
            int IList.Add(object value)
            {
                this.Add((T)value);
                return this.Count - 1;
            }

            /// <summary>
            /// Clears this instance.
            /// </summary>
            /// <exception cref="System.NotImplementedException"></exception>
            void IList.Clear()
            {
                this.Clear();
            }

            /// <summary>
            /// Determines whether the <see cref="IList"/> contains a specific value.
            /// </summary>
            /// <param name="value">The object to locate in the <see cref="IList"/>.</param>
            /// <returns>
            /// true if the <see cref="object"/> is found in the <see cref="IList"/>; otherwise, false.
            /// </returns>
            /// <exception cref="System.NotImplementedException"></exception>
            bool IList.Contains(object value)
            {
                return this.Contains((T)value);
            }

            /// <summary>
            /// Determines the index of a specific item in the <see cref="IList"/>.
            /// </summary>
            /// <param name="value">The object to locate in the <see cref="IList"/>.</param>
            /// <returns>
            /// The index of <paramref name="value"/> if found in the list; otherwise, -1.
            /// </returns>
            /// <exception cref="System.NotImplementedException"></exception>
            int IList.IndexOf(object value)
            {
                return this.IndexOf((T)value);
            }

            /// <summary>
            /// Inserts an item to the <see cref="IList"/> at the specified index.
            /// </summary>
            /// <param name="index">The zero-based index at which <paramref name="value"/> should be inserted.</param>
            /// <param name="value">The object to insert into the <see cref="IList"/>.</param>
            /// <exception cref="System.NotImplementedException"></exception>
            void IList.Insert(int index, object value)
            {
                this.Insert(index, (T)value);
            }

            /// <summary>
            /// Gets a value indicating whether the <see cref="IList"/> has a fixed size.
            /// </summary>
            /// <returns>true if the <see cref="IList"/> has a fixed size; otherwise, false.</returns>
            /// <exception cref="System.NotImplementedException"></exception>
            bool IList.IsFixedSize
            {
                get { return false; }
            }

            /// <summary>
            /// Gets a value indicating whether the <see cref="ICollection{T}"/> is read-only.
            /// </summary>
            /// <returns>true if the <see cref="ICollection{T}"/> is read-only; otherwise, false.
            ///   </returns>
            /// <exception cref="System.NotImplementedException"></exception>
            bool IList.IsReadOnly
            {
                get { return false; }
            }

            /// <summary>
            /// Removes the first occurrence of a specific object from the <see cref="IList"/>.
            /// </summary>
            /// <param name="value">The object to remove from the <see cref="IList"/>.</param>
            /// <exception cref="System.NotImplementedException"></exception>
            void IList.Remove(object value)
            {
                this.Remove((T)value);
            }

            /// <summary>
            /// Gets or sets the <see cref="System.Object"/> at the specified index.
            /// </summary>
            /// <value>
            /// The <see cref="System.Object"/>.
            /// </value>
            /// <param name="index">The index.</param>
            /// <returns></returns>
            /// <exception cref="System.NotImplementedException"></exception>
            object IList.this[int index]
            {
                get { return this[index]; }
                set { this[index] = (T)value; }
            }

            #endregion

            #region ICollection members

            /// <summary>
            /// Copies the elements of the <see cref="ICollection"/> to an <see cref="Array"/>, starting at a particular <see cref="Array"/> index.
            /// </summary>
            /// <param name="array">The one-dimensional <see cref="Array"/> that is the destination of the elements copied from <see cref="ICollection"/>. The <see cref="Array"/> must have zero-based indexing.</param>
            /// <param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param>
            /// <exception cref="System.NotImplementedException"></exception>
            void ICollection.CopyTo(Array array, int arrayIndex)
            {
                this.Root.CopyTo(array, arrayIndex);
            }

            /// <summary>
            /// Gets a value indicating whether access to the <see cref="ICollection"/> is synchronized (thread safe).
            /// </summary>
            /// <returns>true if access to the <see cref="ICollection"/> is synchronized (thread safe); otherwise, false.</returns>
            /// <exception cref="System.NotImplementedException"></exception>
            [DebuggerBrowsable(DebuggerBrowsableState.Never)]
            bool ICollection.IsSynchronized
            {
                get { return false; }
            }

            /// <summary>
            /// Gets an object that can be used to synchronize access to the <see cref="ICollection"/>.
            /// </summary>
            /// <returns>An object that can be used to synchronize access to the <see cref="ICollection"/>.</returns>
            /// <exception cref="System.NotImplementedException"></exception>
            [DebuggerBrowsable(DebuggerBrowsableState.Never)]
            object ICollection.SyncRoot
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
            #endregion
        }
    }

    /// <summary>
    /// A simple view of the immutable list that the debugger can show to the developer.
    /// </summary>
    internal class ImmutableListDebuggerProxy<T>
    {
        /// <summary>
        /// The collection to be enumerated.
        /// </summary>
        private readonly ImmutableList<T> _list;

        /// <summary>
        /// The simple view of the collection.
        /// </summary>
        private T[] _cachedContents;

        /// <summary>   
        /// Initializes a new instance of the <see cref="ImmutableListDebuggerProxy{T}"/> class.
        /// </summary>
        /// <param name="list">The list to display in the debugger</param>
        public ImmutableListDebuggerProxy(ImmutableList<T> list)
        {
            Requires.NotNull(list, "list");
            _list = list;
        }

        /// <summary>
        /// Gets a simple debugger-viewable list.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public T[] Contents
        {
            get
            {
                if (_cachedContents == null)
                {
                    _cachedContents = _list.ToArray(_list.Count);
                }

                return _cachedContents;
            }
        }
    }

    /// <summary>
    /// A simple view of the immutable list that the debugger can show to the developer.
    /// </summary>
    internal class ImmutableListBuilderDebuggerProxy<T>
    {
        /// <summary>
        /// The collection to be enumerated.
        /// </summary>
        private readonly ImmutableList<T>.Builder _list;

        /// <summary>
        /// The simple view of the collection.
        /// </summary>
        private T[] _cachedContents;

        /// <summary>   
        /// Initializes a new instance of the <see cref="ImmutableListBuilderDebuggerProxy{T}"/> class.
        /// </summary>
        /// <param name="builder">The list to display in the debugger</param>
        public ImmutableListBuilderDebuggerProxy(ImmutableList<T>.Builder builder)
        {
            Requires.NotNull(builder, "builder");
            _list = builder;
        }

        /// <summary>
        /// Gets a simple debugger-viewable list.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public T[] Contents
        {
            get
            {
                if (_cachedContents == null)
                {
                    _cachedContents = _list.ToArray(_list.Count);
                }

                return _cachedContents;
            }
        }
    }

    /// <summary>
    /// Common runtime checks that throw <see cref="ArgumentException"/> upon failure.
    /// </summary>
    internal static class Requires
    {
        /// <summary>
        /// Throws an exception if the specified parameter's value is null.
        /// </summary>
        /// <typeparam name="T">The type of the parameter.</typeparam>
        /// <param name="value">The value of the argument.</param>
        /// <param name="parameterName">The name of the parameter to include in any thrown exception.</param>
        /// <exception cref="ArgumentNullException">Thrown if <paramref name="value"/> is <c>null</c></exception>
        [DebuggerStepThrough]
        public static void NotNull<T>(T value, string parameterName)
            where T : class // ensures value-types aren't passed to a null checking method
        {
            if (value == null)
            {
                FailArgumentNullException(parameterName);
            }
        }

        /// <summary>
        /// Throws an exception if the specified parameter's value is null.  It passes through the specified value back as a return value.
        /// </summary>
        /// <typeparam name="T">The type of the parameter.</typeparam>
        /// <param name="value">The value of the argument.</param>
        /// <param name="parameterName">The name of the parameter to include in any thrown exception.</param>
        /// <returns>The value of the parameter.</returns>
        /// <exception cref="ArgumentNullException">Thrown if <paramref name="value"/> is <c>null</c></exception>
        [DebuggerStepThrough]
        public static T NotNullPassthrough<T>(T value, string parameterName)
            where T : class // ensures value-types aren't passed to a null checking method
        {
            NotNull(value, parameterName);
            return value;
        }

        /// <summary>
        /// Throws an exception if the specified parameter's value is null.
        /// </summary>
        /// <typeparam name="T">The type of the parameter.</typeparam>
        /// <param name="value">The value of the argument.</param>
        /// <param name="parameterName">The name of the parameter to include in any thrown exception.</param>
        /// <exception cref="ArgumentNullException">Thrown if <paramref name="value"/> is <c>null</c></exception>
        /// <remarks>
        /// This method exists for callers who themselves only know the type as a generic parameter which
        /// may or may not be a class, but certainly cannot be null.
        /// </remarks>
        [DebuggerStepThrough]
        public static void NotNullAllowStructs<T>(T value, string parameterName)
        {
            if (null == value)
            {
                FailArgumentNullException(parameterName);
            }
        }

        /// <summary>
        /// Throws an <see cref="ArgumentNullException"/>.
        /// </summary>
        /// <param name="parameterName">The name of the parameter that was null.</param>
        [DebuggerStepThrough]
        private static void FailArgumentNullException(string parameterName)
        {
            // Separating out this throwing operation helps with inlining of the caller
            throw new ArgumentNullException(parameterName);
        }

        /// <summary>
        /// Throws an <see cref="ArgumentOutOfRangeException"/> if a condition does not evaluate to true.
        /// </summary>
        [DebuggerStepThrough]
        public static void Range(bool condition, string parameterName, string message = null)
        {
            if (!condition)
            {
                FailRange(parameterName, message);
            }
        }

        /// <summary>
        /// Throws an <see cref="ArgumentOutOfRangeException"/>.
        /// </summary>
        [DebuggerStepThrough]
        public static void FailRange(string parameterName, string message = null)
        {
            if (string.IsNullOrEmpty(message))
            {
                throw new ArgumentOutOfRangeException(parameterName);
            }
            else
            {
                throw new ArgumentOutOfRangeException(parameterName, message);
            }
        }

        /// <summary>
        /// Throws an <see cref="ArgumentException"/> if a condition does not evaluate to true.
        /// </summary>
        [DebuggerStepThrough]
        public static void Argument(bool condition, string parameterName, string message)
        {
            if (!condition)
            {
                throw new ArgumentException(message, parameterName);
            }
        }

        /// <summary>
        /// Throws an <see cref="ArgumentException"/> if a condition does not evaluate to true.
        /// </summary>
        [SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly")]
        [DebuggerStepThrough]
        public static void Argument(bool condition)
        {
            if (!condition)
            {
                throw new ArgumentException();
            }
        }

        /// <summary>
        /// Throws an <see cref="ObjectDisposedException"/> for a disposed object.
        /// </summary>
        /// <typeparam name="TDisposed">Specifies the type of the disposed object.</typeparam>
        /// <param name="disposed">The disposed object.</param>
        [DebuggerStepThrough]
        [MethodImpl(MethodImplOptions.NoInlining)] // inlining this on .NET < 4.5.2 on x64 causes InvalidProgramException. 
        public static void FailObjectDisposed<TDisposed>(TDisposed disposed)
        {
            // separating out this throwing helps with inlining of the caller, especially
            // due to the retrieval of the type's name
            throw new ObjectDisposedException(disposed.GetType().FullName);
        }
    }

    /// <summary>
    /// Extension methods for immutable types.
    /// </summary>
    internal static class ImmutableExtensions
    {
        /// <summary>
        /// Tries to divine the number of elements in a sequence without actually enumerating each element.
        /// </summary>
        /// <typeparam name="T">The type of elements in the sequence.</typeparam>
        /// <param name="sequence">The enumerable source.</param>
        /// <param name="count">Receives the number of elements in the enumeration, if it could be determined.</param>
        /// <returns><c>true</c> if the count could be determined; <c>false</c> otherwise.</returns>
        internal static bool TryGetCount<T>(this IEnumerable<T> sequence, out int count)
        {
            return TryGetCount<T>((IEnumerable)sequence, out count);
        }
        
        /// <summary>
        /// Tries to divine the number of elements in a sequence without actually enumerating each element.
        /// </summary>
        /// <typeparam name="T">The type of elements in the sequence.</typeparam>
        /// <param name="sequence">The enumerable source.</param>
        /// <param name="count">Receives the number of elements in the enumeration, if it could be determined.</param>
        /// <returns><c>true</c> if the count could be determined; <c>false</c> otherwise.</returns>
        internal static bool TryGetCount<T>(this IEnumerable sequence, out int count)
        {
            var collection = sequence as ICollection;
            if (collection != null)
            {
                count = collection.Count;
                return true;
            }

            var collectionOfT = sequence as ICollection<T>;
            if (collectionOfT != null)
            {
                count = collectionOfT.Count;
                return true;
            }

            var readOnlyCollection = sequence as IReadOnlyCollection<T>;
            if (readOnlyCollection != null)
            {
                count = readOnlyCollection.Count;
                return true;
            }

            count = 0;
            return false;
        }
        
        /// <summary>
        /// Gets the number of elements in the specified sequence,
        /// while guaranteeing that the sequence is only enumerated once
        /// in total by this method and the caller.
        /// </summary>
        /// <typeparam name="T">The type of element in the collection.</typeparam>
        /// <param name="sequence">The sequence.</param>
        /// <returns>The number of elements in the sequence.</returns>
        internal static int GetCount<T>(ref IEnumerable<T> sequence)
        {
            int count;
            if (!sequence.TryGetCount(out count))
            {
                // We cannot predict the length of the sequence. We must walk the entire sequence
                // to find the count. But avoid our caller also having to enumerate by capturing
                // the enumeration in a snapshot and passing that back to the caller.
                var list = sequence.ToList();
                count = list.Count;
                sequence = list;
            }

            return count;
        }

        /// <summary>
        /// Gets a copy of a sequence as an array.
        /// </summary>
        /// <typeparam name="T">The type of element.</typeparam>
        /// <param name="sequence">The sequence to be copied.</param>
        /// <param name="count">The number of elements in the sequence.</param>
        /// <returns>The array.</returns>
        /// <remarks>
        /// This is more efficient than the <see cref="Enumerable.ToArray{TSource}"/> extension method
        /// because that only tries to cast the sequence to <see cref="ICollection{T}"/> to determine
        /// the count before it falls back to reallocating arrays as it enumerates.
        /// </remarks>
        internal static T[] ToArray<T>(this IEnumerable<T> sequence, int count)
        {
            Requires.NotNull(sequence, "sequence");
            Requires.Range(count >= 0, "count");

            T[] array = new T[count];
            int i = 0;
            foreach (var item in sequence)
            {
                Requires.Argument(i < count);
                array[i++] = item;
            }

            Requires.Argument(i == count);
            return array;
        }

        /// <summary>
        /// Provides a known wrapper around a sequence of elements that provides the number of elements
        /// and an indexer into its contents.
        /// </summary>
        /// <typeparam name="T">The type of elements in the collection.</typeparam>
        /// <param name="sequence">The collection.</param>
        /// <returns>An ordered collection.  May not be thread-safe.  Never null.</returns>
        internal static IOrderedCollection<T> AsOrderedCollection<T>(this IEnumerable<T> sequence)
        {
            Requires.NotNull(sequence, "sequence");
            Contract.Ensures(Contract.Result<IOrderedCollection<T>>() != null);

            var orderedCollection = sequence as IOrderedCollection<T>;
            if (orderedCollection != null)
            {
                return orderedCollection;
            }

            var listOfT = sequence as IList<T>;
            if (listOfT != null)
            {
                return new ListOfTWrapper<T>(listOfT);
            }

            // It would be great if SortedSet<T> and SortedDictionary<T> provided indexers into their collections,
            // but since they don't we have to clone them to an array.
            return new FallbackWrapper<T>(sequence);
        }

        /// <summary>
        /// Clears the specified stack.  For empty stacks, it avoids the call to <see cref="Stack{T}.Clear"/>, which
        /// avoids a call into the runtime's implementation of <see cref="Array.Clear"/>, helping performance,
        /// in particular around inlining.  <see cref="Stack{T}.Count"/> typically gets inlined by today's JIT, while
        /// <see cref="Stack{T}.Clear"/> and <see cref="Array.Clear"/> typically don't.
        /// </summary>
        /// <typeparam name="T">Specifies the type of data in the stack to be cleared.</typeparam>
        /// <param name="stack">The stack to clear.</param>
        internal static void ClearFastWhenEmpty<T>(this Stack<T> stack)
        {
            if (stack.Count > 0)
            {
                stack.Clear();
            }
        }

        /// <summary>
        /// Gets a disposable enumerable that can be used as the source for a C# foreach loop
        /// that will not box the enumerator if it is of a particular type.
        /// </summary>
        /// <typeparam name="T">The type of value to be enumerated.</typeparam>
        /// <typeparam name="TEnumerator">The type of the Enumerator struct.</typeparam>
        /// <param name="enumerable">The collection to be enumerated.</param>
        /// <returns>A struct that enumerates the collection.</returns>
        internal static DisposableEnumeratorAdapter<T, TEnumerator> GetEnumerableDisposable<T, TEnumerator>(this IEnumerable<T> enumerable)
            where TEnumerator : struct, IStrongEnumerator<T>, IEnumerator<T>
        {
            Requires.NotNull(enumerable, "enumerable");

            var strongEnumerable = enumerable as IStrongEnumerable<T, TEnumerator>;
            if (strongEnumerable != null)
            {
                return new DisposableEnumeratorAdapter<T, TEnumerator>(strongEnumerable.GetEnumerator());
            }
            else
            {
                // Consider for future: we could add more special cases for common
                // mutable collection types like List<T>+Enumerator and such.
                return new DisposableEnumeratorAdapter<T, TEnumerator>(enumerable.GetEnumerator());
            }
        }
        
        /// <summary>
        /// Wraps a <see cref="IList{T}"/> as an ordered collection.
        /// </summary>
        /// <typeparam name="T">The type of element in the collection.</typeparam>
        private class ListOfTWrapper<T> : IOrderedCollection<T>
        {
            /// <summary>
            /// The list being exposed.
            /// </summary>
            private readonly IList<T> _collection;

            /// <summary>
            /// Initializes a new instance of the <see cref="ListOfTWrapper{T}"/> class.
            /// </summary>
            /// <param name="collection">The collection.</param>
            internal ListOfTWrapper(IList<T> collection)
            {
                Requires.NotNull(collection, "collection");
                _collection = collection;
            }

            /// <summary>
            /// Gets the count.
            /// </summary>
            public int Count
            {
                get { return _collection.Count; }
            }

            /// <summary>
            /// Gets the <typeparamref name="T"/> at the specified index.
            /// </summary>
            public T this[int index]
            {
                get { return _collection[index]; }
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            public IEnumerator<T> GetEnumerator()
            {
                return _collection.GetEnumerator();
            }

            /// <summary>
            /// Returns an enumerator that iterates through a collection.
            /// </summary>
            /// <returns>
            /// An <see cref="IEnumerator"/> object that can be used to iterate through the collection.
            /// </returns>
            IEnumerator IEnumerable.GetEnumerator()
            {
                return this.GetEnumerator();
            }
        }

        /// <summary>
        /// Wraps any <see cref="IEnumerable{T}"/> as an ordered, indexable list.
        /// </summary>
        /// <typeparam name="T">The type of element in the collection.</typeparam>
        private class FallbackWrapper<T> : IOrderedCollection<T>
        {
            /// <summary>
            /// The original sequence.
            /// </summary>
            private readonly IEnumerable<T> _sequence;

            /// <summary>
            /// The list-ified sequence.
            /// </summary>
            private IList<T> _collection;

            /// <summary>
            /// Initializes a new instance of the <see cref="FallbackWrapper{T}"/> class.
            /// </summary>
            /// <param name="sequence">The sequence.</param>
            internal FallbackWrapper(IEnumerable<T> sequence)
            {
                Requires.NotNull(sequence, "sequence");
                _sequence = sequence;
            }

            /// <summary>
            /// Gets the count.
            /// </summary>
            public int Count
            {
                get
                {
                    if (_collection == null)
                    {
                        int count;
                        if (_sequence.TryGetCount(out count))
                        {
                            return count;
                        }

                        _collection = _sequence.ToArray();
                    }

                    return _collection.Count;
                }
            }

            /// <summary>
            /// Gets the <typeparamref name="T"/> at the specified index.
            /// </summary>
            public T this[int index]
            {
                get
                {
                    if (_collection == null)
                    {
                        _collection = _sequence.ToArray();
                    }

                    return _collection[index];
                }
            }

            /// <summary>
            /// Returns an enumerator that iterates through the collection.
            /// </summary>
            /// <returns>
            /// A <see cref="IEnumerator{T}"/> that can be used to iterate through the collection.
            /// </returns>
            public IEnumerator<T> GetEnumerator()
            {
                return _sequence.GetEnumerator();
            }

            /// <summary>
            /// Returns an enumerator that iterates through a collection.
            /// </summary>
            /// <returns>
            /// An <see cref="IEnumerator"/> object that can be used to iterate through the collection.
            /// </returns>
            [ExcludeFromCodeCoverage]
            IEnumerator IEnumerable.GetEnumerator()
            {
                return this.GetEnumerator();
            }
        }
    }

    /// <summary>
    /// An adapter that allows a single foreach loop in C# to avoid
    /// boxing an enumerator when possible, but fall back to boxing when necessary.
    /// </summary>
    /// <typeparam name="T">The type of value to be enumerated.</typeparam>
    /// <typeparam name="TEnumerator">The type of the enumerator struct.</typeparam>
    internal struct DisposableEnumeratorAdapter<T, TEnumerator> : IDisposable
        where TEnumerator : struct, IEnumerator<T>
    {
        /// <summary>
        /// The enumerator object to use if not null.
        /// </summary>
        private readonly IEnumerator<T> _enumeratorObject;

        /// <summary>
        /// The enumerator struct to use if <see cref="_enumeratorObject"/> is <c>null</c>.
        /// </summary>
        /// <remarks>
        /// This field must NOT be readonly because the field's value is a struct and must be able to mutate
        /// in-place. A readonly keyword would cause any mutation to take place in a copy rather than the field.
        /// </remarks>
        private TEnumerator _enumeratorStruct;

        /// <summary>
        /// Initializes a new instance of the <see cref="DisposableEnumeratorAdapter{T, TEnumerator}"/> struct
        /// for enumerating over a strongly typed struct enumerator.
        /// </summary>
        /// <param name="enumerator">The initialized enumerator struct.</param>
        internal DisposableEnumeratorAdapter(TEnumerator enumerator)
        {
            _enumeratorStruct = enumerator;
            _enumeratorObject = null;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="DisposableEnumeratorAdapter{T, TEnumerator}"/> struct
        /// for enumerating over a (boxed) <see cref="IEnumerable{T}"/> enumerator.
        /// </summary>
        /// <param name="enumerator">The initialized enumerator object.</param>
        internal DisposableEnumeratorAdapter(IEnumerator<T> enumerator)
        {
            _enumeratorStruct = default(TEnumerator);
            _enumeratorObject = enumerator;
        }

        /// <summary>
        /// Gets the current enumerated value.
        /// </summary>
        public T Current
        {
            get { return _enumeratorObject != null ? _enumeratorObject.Current : _enumeratorStruct.Current; }
        }

        /// <summary>
        /// Moves to the next value.
        /// </summary>
        public bool MoveNext()
        {
            return _enumeratorObject != null ? _enumeratorObject.MoveNext() : _enumeratorStruct.MoveNext();
        }

        /// <summary>
        /// Disposes the underlying enumerator.
        /// </summary>
        public void Dispose()
        {
            if (_enumeratorObject != null)
            {
                _enumeratorObject.Dispose();
            }
            else
            {
                _enumeratorStruct.Dispose();
            }
        }

        /// <summary>
        /// Returns a copy of this struct. 
        /// </summary>
        /// <remarks>
        /// This member is here so that it can be used in C# foreach loops.
        /// </remarks>
        public DisposableEnumeratorAdapter<T, TEnumerator> GetEnumerator()
        {
            return this;
        }
    }

    /// <summary>
    /// Object pooling utilities.
    /// </summary>
    internal class SecureObjectPool
    {
        /// <summary>
        /// The ever-incrementing (and wrap-on-overflow) integer for owner id's.
        /// </summary>
        private static int s_poolUserIdCounter;

        /// <summary>
        /// The ID reserved for unassigned objects.
        /// </summary>
        internal const int UnassignedId = -1;

        /// <summary>
        /// Returns a new ID.
        /// </summary>
        internal static int NewId()
        {
            int result;
            do
            {
                result = Interlocked.Increment(ref s_poolUserIdCounter);
            }
            while (result == UnassignedId);

            return result;
        }
    }

    internal class SecureObjectPool<T, TCaller>
        where TCaller : ISecurePooledObjectUser
    {
        public void TryAdd(TCaller caller, SecurePooledObject<T> item)
        {
            // Only allow the caller to recycle this object if it is the current owner.
            if (caller.PoolUserId == item.Owner)
            {
                item.Owner = SecureObjectPool.UnassignedId;
                AllocFreeConcurrentStack<SecurePooledObject<T>>.TryAdd(item);
            }
        }

        public bool TryTake(TCaller caller, out SecurePooledObject<T> item)
        {
            if (caller.PoolUserId != SecureObjectPool.UnassignedId && AllocFreeConcurrentStack<SecurePooledObject<T>>.TryTake(out item))
            {
                item.Owner = caller.PoolUserId;
                return true;
            }
            else
            {
                item = null;
                return false;
            }
        }

        public SecurePooledObject<T> PrepNew(TCaller caller, T newValue)
        {
            Requires.NotNullAllowStructs(newValue, "newValue");
            var pooledObject = new SecurePooledObject<T>(newValue);
            pooledObject.Owner = caller.PoolUserId;
            return pooledObject;
        }
    }

    internal interface ISecurePooledObjectUser
    {
        int PoolUserId { get; }
    }

    internal class SecurePooledObject<T>
    {
        private readonly T _value;
        private int _owner;

        internal SecurePooledObject(T newValue)
        {
            Requires.NotNullAllowStructs(newValue, "newValue");
            _value = newValue;
        }

        /// <summary>
        /// Gets or sets the current owner of this recyclable object.
        /// </summary>
        internal int Owner
        {
            get { return _owner; }
            set { _owner = value; }
        }

        /// <summary>
        /// Returns the recyclable value if it hasn't been reclaimed already.
        /// </summary>
        /// <typeparam name="TCaller">The type of renter of the object.</typeparam>
        /// <param name="caller">The renter of the object.</param>
        /// <returns>The rented object.</returns>
        /// <exception cref="ObjectDisposedException">Thrown if <paramref name="caller"/> is no longer the renter of the value.</exception>
        internal T Use<TCaller>(ref TCaller caller)
            where TCaller : struct, ISecurePooledObjectUser
        {
            if (!IsOwned(ref caller))
                Requires.FailObjectDisposed(caller);
            return _value;
        }

        internal bool TryUse<TCaller>(ref TCaller caller, out T value)
            where TCaller : struct, ISecurePooledObjectUser
        {
            if (IsOwned(ref caller))
            {
                value = _value;
                return true;
            }
            else
            {
                value = default(T);
                return false;
            }
        }
#if NET45
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal bool IsOwned<TCaller>(ref TCaller caller)
            where TCaller : struct, ISecurePooledObjectUser
        {
            return caller.PoolUserId == _owner;
        }
    }

    internal static class AllocFreeConcurrentStack<T>
    {
        private const int MaxSize = 35;

        [ThreadStatic]
        private static Stack<RefAsValueType<T>> t_stack;

        public static void TryAdd(T item)
        {
            Stack<RefAsValueType<T>> localStack = t_stack; // cache in a local to avoid unnecessary TLS hits on repeated accesses
            if (localStack == null)
            {
                t_stack = localStack = new Stack<RefAsValueType<T>>(MaxSize);
            }

            // Just in case we're in a scenario where an object is continually requested on one thread
            // and returned on another, avoid unbounded growth of the stack.
            if (localStack.Count < MaxSize)
            {
                localStack.Push(new RefAsValueType<T>(item));
            }
        }

        public static bool TryTake(out T item)
        {
            Stack<RefAsValueType<T>> localStack = t_stack; // cache in a local to avoid unnecessary TLS hits on repeated accesses
            if (localStack != null && localStack.Count > 0)
            {
                item = localStack.Pop().Value;
                return true;
            }

            item = default(T);
            return false;
        }
    }

    /// <summary>
    /// A simple struct we wrap reference types inside when storing in arrays to
    /// bypass the CLR's covariant checks when writing to arrays.
    /// </summary>
    /// <remarks>
    /// We use <see cref="RefAsValueType{T}"/> as a wrapper to avoid paying the cost of covariant checks whenever
    /// the underlying array that the <see cref="Stack{T}"/> class uses is written to. 
    /// We've recognized this as a perf win in ETL traces for these stack frames:
    /// clr!JIT_Stelem_Ref
    ///   clr!ArrayStoreCheck
    ///     clr!ObjIsInstanceOf
    /// </remarks>
    [DebuggerDisplay("{Value,nq}")]
    internal struct RefAsValueType<T>
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="RefAsValueType{T}"/> struct.
        /// </summary>
        internal RefAsValueType(T value)
        {
            this.Value = value;
        }

        /// <summary>
        /// The value.
        /// </summary>
        internal T Value;
    }
}