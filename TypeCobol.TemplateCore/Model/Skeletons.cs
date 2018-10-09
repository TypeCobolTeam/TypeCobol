using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.Model
{
    /// <summary>
    /// The Skeletons model.
    /// </summary>
    public class Skeletons : AttributedEntity, IList<Skeleton>
    {
        /// <summary>
        /// The List of all single skeletons.
        /// </summary>
        public List<Skeleton> SkeletonList
        {
            get;
            private set;
        }

        /// <summary>
        /// Empty constructor.
        /// </summary>
        public Skeletons()
        {
            SkeletonList = new List<Skeleton>();
        }
        /// <summary>
        /// Add a single skeleton to the model.
        /// </summary>
        /// <param name="skeleton"></param>
        public void Add(Skeleton skeleton)
        {
            SkeletonList.Add(skeleton);
        }

        public int IndexOf(Skeleton item)
        {
            return SkeletonList.IndexOf(item);
        }

        public void Insert(int index, Skeleton item)
        {
            SkeletonList.Insert(index, item);
        }

        public void RemoveAt(int index)
        {
            SkeletonList.RemoveAt(index);
        }

        public bool Contains(Skeleton item)
        {
            return SkeletonList.Contains(item);
        }

        public void CopyTo(Skeleton[] array, int arrayIndex)
        {
            SkeletonList.CopyTo(array, arrayIndex);
        }

        public bool Remove(Skeleton item)
        {
            return SkeletonList.Remove(item);
        }

        IEnumerator<Skeleton> IEnumerable<Skeleton>.GetEnumerator()
        {
            return SkeletonList.GetEnumerator();
        }

        public bool IsReadOnly
        {
            get
            {
                return false;
            }
        }

        Skeleton IList<Skeleton>.this[int index]
        {
            get
            {
                return SkeletonList[index];
            }
            set => throw new NotImplementedException();
        }

        /// <summary>
        /// Skeleton List indexer.
        /// </summary>
        /// <param name="i">The index of the request skeleton.</param>
        /// <returns>The Skeleton at the given index position.</returns>
        public Skeleton this[int i]
        {
            get
            {
                return SkeletonList[i];
            }
        }
    }
}
