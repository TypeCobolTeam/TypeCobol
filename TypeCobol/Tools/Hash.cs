using System;

namespace TypeCobol.Tools
{

    using System.Collections.Generic;
    using System.Security.Cryptography;
    using System.Text;
    using System.Text.RegularExpressions;
    using Compiler.Diagnostics;
    using Compiler.Nodes;
    using Compiler.Parser;
    using System.Linq;

    public interface Hashable
    {
        string Hash { get; }
    }

    public class Hash
    {
        /// <summary>
        /// Calculate the Shortcut name of a COBOl Program name.
        /// </summary>
        /// <param name="name">The name to calculate the shortcut.</param>
        /// <returns>The Shortcut name</returns>
        public static string CalculateCobolProgramNameShortcut(string name)
        {
            return name.Substring(0, Math.Min(val1: name.Length, val2: 8));
        }

        public static string CreateSHA256(string text)
        {
            byte[] input = Encoding.UTF8.GetBytes(text.TrimEnd('\0'));
            byte[] hash = new SHA256Managed().ComputeHash(input);
            var result = new StringBuilder();
            foreach (byte b in hash)
                result.Append(System.String.Format("{0:x2}", b));
            return result.ToString();
        }

        /// <summary>
        /// Method to create a hash of a given lenght. You can add a node to increment a dictionary of hash and signature in order to avoid duplicate
        /// </summary>
        /// <param name="text"> The text to hash</param>
        /// <param name="size"> The size of the hash to return max value is 64</param>
        /// <param name="node"> The node in which we are going to store the hashes (we use the maximum level node ie. SourceFile)</param>
        /// <returns></returns>
        public static string CreateCOBOLNameHash(string text, int size = 8, Node node = null)
        {
            text = text.ToLowerInvariant();
            string hash = CreateSHA256(text);
            Regex hashRegex = new Regex(@"^[a-z\s,]*$");
            int index = 0;
            if (size > hash.Length)
                size = hash.Length;

            for (int i = 0; i < hash.Length; i++)
            {
                if(hashRegex.IsMatch(hash[i].ToString()))
                {
                    index = i;
                    break;
                }
            }
            
            string result = hash.Substring(index, System.Math.Min(size, hash.Length - index));

            if(result.Length < size)
                result += hash.Substring(0, size - result.Length);

            if (node?.Root != null)
            {
                var existingNameForComputedHash = node.Root.GeneratedCobolHashes.SingleOrDefault(p => p.Value == result).Key;
                if (existingNameForComputedHash == null)
                {
                    //No conflict, add the new hash
                    node.Root.GeneratedCobolHashes.Add(text, result);
                }
                else if (existingNameForComputedHash != text)
                {
                    //Hash conflict, same hash has been computed for two different names !
                    DiagnosticUtils.AddError(node, "Duplicated hash detected. Please contact TypeCobol support team.", MessageCode.ImplementationError);
                }
            }

            return result;
        }
    }

    public class UIDStore
    {
        public const int DEFAULT_MAX_SIZE = 30;
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
        private Dictionary<string, List<string>> Names = new Dictionary<string, List<string>>();

        public UIDStore(int maxsize = DEFAULT_MAX_SIZE, int maxitems = DEFAULT_MAX_ITEMS, string separator = DEFAULT_SEPARATOR)
        {
            this.MaxSize = maxsize;
            this.MaxItems = maxitems;
            this.Separator = separator;
            this.IndexSize = maxitems.ToString().Length;
            this.TruncatedSize = maxsize - IndexSize - Separator.Length;
        }

        public virtual string FromOriginal(string name)
        {
            List<string> names;
            Names.TryGetValue(name, out names);
            if (names == null) names = new List<string>();
            var uid = Create(name, names.Count + 1);
            names.Add(uid);
            Names[name] = names;
            return uid;
        }
        private string Create(string name, int index)
        {
            string uid = name.Substring(0, System.Math.Min(TruncatedSize, name.Length));
            return uid + Separator + index.ToString("D" + IndexSize);
        }
        public virtual string FromGenerated(string uid)
        {
            foreach (var kv in Names)
                foreach (var v in kv.Value)
                    if (v.Equals(uid)) return kv.Key;
            return null;
        }
    }

}