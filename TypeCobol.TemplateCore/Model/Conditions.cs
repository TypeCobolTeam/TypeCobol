using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// A List of Conditions.
    /// </summary>
    public class Conditions : AttributedEntity, IList<Condition>
    {
        /// <summary>
        /// The List of conditions.
        /// </summary>
        public IList<Condition> ConditionList
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Conditions()
        {
            ConditionList = new List<Condition>();
        }

        public Condition this[int index]
        {   get
            {
                return ConditionList[index];
            }
            set => throw new NotImplementedException();
        }

        public bool IsReadOnly
        {
            get
            {
                return ConditionList.IsReadOnly;
            }
        }

        public int Count
        {
            get
            {
                return ConditionList.Count;
            }
        }

        public void Add(Condition item)
        {
            ConditionList.Add(item);
        }

        public bool Contains(Condition item)
        {
            return ConditionList.Contains(item);
        }

        public void CopyTo(Condition[] array, int arrayIndex)
        {
            ConditionList.CopyTo(array, arrayIndex);
        }

        public int IndexOf(Condition item)
        {
            return ConditionList.IndexOf(item);
        }

        public void Insert(int index, Condition item)
        {
            ConditionList.Insert(index, item);
        }

        public bool Remove(Condition item)
        {
            return ConditionList.Remove(item);
        }

        public void RemoveAt(int index)
        {
            ConditionList.RemoveAt(index);
        }

        IEnumerator<Condition> IEnumerable<Condition>.GetEnumerator()
        {
            return ConditionList.GetEnumerator();
        }

        public void Clear()
        {
            ConditionList.Clear();
        }

        public IEnumerator GetEnumerator()
        {
            return ConditionList.GetEnumerator();
        }
    }
}
