using System;
using System.Collections;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel {

	public interface MemoryArea {
		/// <summary>Address of the start of the area</summary>
		int Offset { get; }
		/// <summary>Area length (in bytes)</summary>
		int Length { get; }
	}
	public interface COBOLMemoryArea: MemoryArea {
		/// <summary>1-based address of the first byte</summary>
		int Start  { get; }
		/// <summary>1-based address of the last byte</summary>
		int End    { get; }
	}

	public class TableElementInMemory: COBOLMemoryArea {
		public int Offset { get; private set; }
		public int Length { get; private set; }
		public int Index  { get; private set; }
		public int Start  { get { return Offset+1; } }
		public int End    { get { return Offset+Length; } }

		public TableElementInMemory(int index, int length, int offset=0) {
			Offset = offset;
			Index  = index;
			Length = length;
		}
	}

	public class TableInMemory: COBOLMemoryArea, IList<TableElementInMemory> {
		public int Offset { get; set; }
		public int Length { get { return UnitLength * MaxOccurences.Count; } }
		public int UnitLength    { get; private set; }
		public Occurences MaxOccurences { get; private set; }
		public int Start  { get { return Offset+1; } }
		public int End    { get { return Offset+Length; } }

		/// <summary>Naive size computation</summary>
		/// <param name="picture">Picture, for length</param>
		/// <param name="type">Data type, for unit size</param>
		/// <param name="offset">Offset relative to level-01 parent</param>
		/// <returns>A size for the data that will be correct in simple cases, but wrong in real-world cases.</returns>
		public TableInMemory(string picture, DataType type, int offset, Occurences max) {
			Offset = offset;
			MaxOccurences = max;
			UnitLength = 1;//TODO compute from type
			if (picture != null) {
				var betweenparentheses = picture.Split("()".ToCharArray());
				if (betweenparentheses.Length > 1)
					UnitLength = int.Parse(betweenparentheses[1]);
			}
		}



		public IEnumerator<TableElementInMemory> GetEnumerator() {
			for(int c = 0; c < Count; c++)
				yield return new TableElementInMemory(c, UnitLength, Offset + c*UnitLength);
		}
		IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
		public int  Count      { get { return MaxOccurences.Count; } }
		public bool IsReadOnly { get { return true; } }
		public void Add(TableElementInMemory item)               { throw new NotSupportedException(); }
		public void Insert(int index, TableElementInMemory item) { throw new NotSupportedException(); }
		public bool Remove(TableElementInMemory item)            { throw new NotSupportedException(); }
		public void RemoveAt(int index)                          { throw new NotSupportedException(); }
		public void Clear()                                      { throw new NotSupportedException(); }
		public bool Contains(TableElementInMemory item)          { throw new NotSupportedException(); }
		public int IndexOf(TableElementInMemory item)            { throw new NotSupportedException(); }
		public void CopyTo(TableElementInMemory[] array, int index) {
			if (array == null) throw new ArgumentNullException();
			if (index < 0) throw new ArgumentOutOfRangeException();
			if (array.Length < index+Count) throw new ArgumentException();
			for(int c = 0; c < Count; c++) array[index+c] = this[c];
		}
		public TableElementInMemory this[int index] {
			get {
				if (index < 0 || (MaxOccurences.isBounded && index >= MaxOccurences.Count))
					throw new IndexOutOfRangeException(index+" outside of [0;"+MaxOccurences+"[");
				return new TableElementInMemory(index, UnitLength, Offset + index*UnitLength);
			}
			set { throw new NotSupportedException(); }
		}
	}

	public interface Occurences {
		bool isBounded { get; }
		int Count { get; }
	}
	public class Bounded: Occurences {
		public bool isBounded { get { return true; } }
		public int Count { get; private set; }
		public Bounded(int size) { Count = size; }
	}
	public class Unique: Bounded {
		public Unique(): base(1) { }
	}
	public class Unbounded: Occurences {
		public bool isBounded { get { return false; } }
		public int Count { get { throw new ArgumentOutOfRangeException("Unbounded"); } }
	}
}
