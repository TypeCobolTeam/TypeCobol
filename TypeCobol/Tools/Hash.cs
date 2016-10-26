namespace TypeCobol.Tools {
	
	using System.Security.Cryptography;
	using System.Text;

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

}