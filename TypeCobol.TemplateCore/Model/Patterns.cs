using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A List of Patterns.
    /// </summary>
    public class Patterns : AttributedEntity, IList<Pattern>
    {
        /// <summary>
        /// The List of Patterns.
        /// </summary>
        public IList<Pattern> PatternList
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Patterns()
        {
            PatternList = new List<Pattern>();
        }

        public Pattern this[int index]
        {
            get
            {
                return PatternList[index];
            }
            set => throw new NotImplementedException();
        }

        public bool IsReadOnly => throw new NotImplementedException();

        public void Add(Pattern item)
        {
            PatternList.Add(item);
        }

        public bool Contains(Pattern item)
        {
            return PatternList.Contains(item);
        }

        public void CopyTo(Pattern[] array, int arrayIndex)
        {
            PatternList.CopyTo(array, arrayIndex);
        }

        public int IndexOf(Pattern item)
        {
            return PatternList.IndexOf(item);
        }

        public void Insert(int index, Pattern item)
        {
            PatternList.Insert(index, item);
        }

        public bool Remove(Pattern item)
        {
            return PatternList.Remove(item);
        }

        public void RemoveAt(int index)
        {
            PatternList.RemoveAt(index);
        }

        IEnumerator<Pattern> IEnumerable<Pattern>.GetEnumerator()
        {
            return PatternList.GetEnumerator();
        }
    }
}
