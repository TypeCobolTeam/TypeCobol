namespace TypeCobol.Tools {

	using System.Collections.Generic;
	using System.Security.Cryptography;
	using System.Text;

public interface Hashable {
	string Hash { get; }
}

public class Hash {

	public static string CreateSHA256(string text) {
		byte[] input = Encoding.UTF8.GetBytes(text.TrimEnd('\0'));
		byte[] hash = new SHA256Managed().ComputeHash(input);
		var result = new StringBuilder();
		foreach (byte b in hash)
			result.Append(System.String.Format("{0:x2}", b));
		return result.ToString();
	}
}

public class UIDStore {
	public const int DEFAULT_MAX_SIZE  = 30;
	public const int DEFAULT_MAX_ITEMS = 99;
	public const string DEFAULT_SEPARATOR = "-";
	/// <summary>Maximum size of generated names</summary>
	public int MaxSize { get; private set; }
	/// <summary>Maximum number of items of the same original name</summary>
	public int MaxItems { get; private set; }
	/// <summary>Original names larger than that will be truncated in generated names</summary>
	public int TruncatedSize { get; private set; }
	/// <summary>Number of digits of index suffix</summary>
	public int IndexSize { get; private set; }
	/// <summary>Separator between truncated name and index suffix</summary>
	public string Separator { get; private set; }
	/// <summary>Lookup table of names <-> UID</summary>
	private Dictionary<string,List<string>> Names = new Dictionary<string,List<string>>();

	public UIDStore(int maxsize = DEFAULT_MAX_SIZE, int maxitems = DEFAULT_MAX_ITEMS, string separator = DEFAULT_SEPARATOR) {
		this.MaxSize = maxsize;
		this.MaxItems = maxitems;
		this.Separator = separator;
		this.IndexSize = maxitems.ToString().Length;
		this.TruncatedSize = maxsize - IndexSize - Separator.Length;
	}

	public virtual string FromOriginal(string name) {
		List<string> names;
		Names.TryGetValue(name, out names);
		if (names == null) names = new List<string>();
		var uid = Create(name, names.Count+1);
		names.Add(uid);
		Names[name] = names;
		return uid;
	}
	private string Create(string name, int index) {
		string uid = name.Substring(0, System.Math.Min(TruncatedSize, name.Length));
		return uid + Separator + index.ToString("D"+IndexSize);
	}
	public virtual string FromGenerated(string uid) {
		foreach(var kv in Names)
			foreach(var v in kv.Value)
				if (v.Equals(uid)) return kv.Key;
		return null;
	}
}

}