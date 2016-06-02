using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    public partial class DataDefinitionNode : TypeDefinition, ICloneable
    {
        public DataDefinitionNode() 
        {
            this.Subordinates = new List<DataDefinitionNode>();
        }

        public object Clone()
        {
            var clone = this.MemberwiseClone() as DataDescriptionEntry;
            var subs = new List<DataDescriptionEntry>();
            foreach (var sub in Subordinates)
            {
                var csub = sub.Clone() as DataDescriptionEntry;
                csub.TopLevel = clone;
                subs.Add(csub);
            }
            clone.Subordinates = subs;
            return clone;
        }

        public QualifiedName QualifiedName
        {
            get
            {
                List<DataName> names = new List<DataName>();
                var current = TopLevel;
                while (current != null)
                {
                    names.Add(current.DataName);
                    current = current.TopLevel;
                }
                names.Reverse();
                return new SyntacticQualifiedName(Name, names);
            }
        }

        /// <summary>
        /// Type declaration.
        /// TODO: find a data representation, Object is bad!
        /// </summary>
        public Object Type { get; set; }

        /// <summary>
        /// Name of the top-level symbol (for group elements).
        /// </summary>
        public DataDescriptionEntry TopLevel { get; set; }

        /// <param name="generation">0 for this, 1 for TopLevel, 2 for TopLevel.Toplevel, ...</param>
        /// <returns>Appropriate TopLevel item, or null if generation <0 or generation too high.</returns>
        public DataDescriptionEntry GetAncestor(int generation)
        {
            if (generation < 0) return null;
            if (generation == 0) return this;
            if (TopLevel == null) return null;
            return TopLevel.GetAncestor(generation - 1);
        }

        public int Generation
        {
            get
            {
                int generation = 0;
                var parent = TopLevel;
                while (parent != null)
                {
                    generation++;
                    parent = parent.TopLevel;
                }
                return generation;
            }
        }

        /// <summary>
        /// Token (used for position tracking).
        /// </summary>
        public Token Token { get; set; }

        public DataType DataType { get; set; }
        public bool IsBuiltInType { get { return DataType == DataType.Unknown && Picture.StartsWith("TYPE:"); } }

        public COBOLMemoryArea MemoryArea { get; set; }
        public Occurences Occurences
        {
            get
            {
                if (IsTableOccurence)
                {
                    if (NoMaxOccurencesCount) return new Unbounded();
                    else return new Bounded(MaxOccurencesCount);
                }
                else return new Unique();
            }
        }

        public bool IsGroup
        {
            get { return Picture == null; }
            private set { IsGroup = value; }
        }
        public ICollection<DataDescriptionEntry> Subordinates { get; private set; }

        // [TYPECOBOL]
        public virtual bool IsTypeDefinition { get; set; }
        public bool IsTypeDefinitionPart
        {
            get { return GetTypeDefinition() != null; }
        }
        private DataDescriptionEntry GetTypeDefinition()
        {
            var parent = this;
            while (parent != null)
            {
                if (parent.IsTypeDefinition) return parent;
                parent = parent.TopLevel;
            }
            return null;
        }
        // [/TYPECOBOL]
        
        public override string ToString()
        {
            var str = new System.Text.StringBuilder();
            // [TYPECOBOL]
            if (IsTypeDefinition) str.Append("TYPEDEF ");
            // [/TYPECOBOL]
            if (IsFiller) str.Append("<filler>");
            else if (Name == null) str.Append("?");
            str.Append(Name);
            if (IsTableOccurence)
            {
                str.Append('[');
                if (OccursDependingOn != null)
                    str.Append(OccursDependingOn).Append("∈[");
                str.Append(MinOccurencesCount);
                if (MaxOccurencesCount != MinOccurencesCount)
                    if (NoMaxOccurencesCount) str.Append(";∞");
                    else str.Append(';').Append(MaxOccurencesCount);
                if (OccursDependingOn != null)
                    str.Append(']');
                str.Append(']');
            }
            str.Append(" {").Append(LevelNumber).Append("} ");
            if (IsGroup)
            {
                str.Append("GROUP(").Append(Subordinates.Count).Append(") [ ");
                foreach (var sub in Subordinates) str.Append(sub.Name).Append(" ");
                str.Append("]");
            }
            if (DataType != DataType.Unknown) str.Append(DataType);
            var parent = TopLevel;
            while (parent != null)
            {
                str.Append(" <of> ").Append(parent.Name);
                parent = parent.TopLevel;
            }
            //if (InitialValue != null) str.Append('(').Append(InitialValue).Append(')');
            return str.ToString();
        }
    }
}
