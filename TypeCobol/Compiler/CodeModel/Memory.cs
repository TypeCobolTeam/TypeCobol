using System;
using System.Collections;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel {

	public interface MemoryArea {
		/// <summary>Offset (in bytes) relative to toplevel item</summary>
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

	public class DataInMemory: COBOLMemoryArea {
		public virtual int Offset { get; set; }
		public virtual int Length { get; private set; }
		public virtual int Start  { get { return Offset+1; } }
		public virtual int End    { get { return Offset+Length; } }

		public DataInMemory(int length, int offset=0) {
			Length = length;
			Offset = offset;
		}
	}

	public class TableElementInMemory: DataInMemory {
		/// <summary>Index of the elements in the table</summary>
		public virtual int Index  { get; private set; }

		public TableElementInMemory(int index, int length, int offset=0)
			: base(length, offset) {
			Index  = index;
		}
	}

	public class TableInMemory: DataInMemory, IList<TableElementInMemory> {
		/// <summary>Length (in bytes) of the table</summary>
		public override int Length { get { return UnitLength * MaxOccurences.Count; } }
		/// <summary>Length (in bytes) of one element in the table (all elements have the same length)</summary>
		public int UnitLength    { get; private set; }
		/// <summary>Maximum number of elements in the table</summary>
		public Occurences MaxOccurences { get; private set; }

		public TableInMemory(int length, int offset, Occurences max)
			: base(-1, offset) {
			MaxOccurences = max;
			UnitLength = length;
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
					throw new ArgumentOutOfRangeException(index+" outside of [0;"+MaxOccurences+"[");
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
		public int Count { get { throw new IndexOutOfRangeException("Unbounded"); } }
	}
}
