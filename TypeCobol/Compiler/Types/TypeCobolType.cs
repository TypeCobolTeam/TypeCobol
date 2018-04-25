﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// A Cobol Type
    /// </summary>
    public class TypeCobolType : ISemanticData
    {
        /// <summary>
        /// Type tags
        /// </summary>
        public enum Tags
        {
            Usage,//This is a type which is only defined by its usage.
            Picture,
            Array,
            Pointer,
            Record,
            Program,
            Function,
            Typedef,
            Renames
        }

        /// <summary>
        /// Type's flags
        /// </summary>
        public enum Flag
        {
            Strong = 0x01 << 0,
            Weak = 0x01 << 1,
            Strict = 0x01 << 2
        }

        /// <summary>
        /// Usage associated to this type
        /// </summary>
        public enum UsageFormat
        {
            None,           //No associated Usage
            Binary = 1,
            Comp,
            Comp1,
            Comp2,
            Comp3,
            Comp4,
            Comp5,
            Display,
            Display1,
            Index,
            National,
            PackedDecimal,
            ObjectReference,
            Pointer,
            ProcedurePointer,
            FunctionPointer
        }

        /// <summary>
        /// Getter on type tag.
        /// </summary>
        public Tags Tag
        {
            get;
            internal set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="tag">TypeCobol type</param>
        /// <param name="usage">Usage format</param>
        internal TypeCobolType(Tags tag, UsageFormat usage = UsageFormat.None)
        {
            this.Tag = tag;
            this.Usage = usage;
        }

        /// <summary>
        /// Types's Flags.
        /// </summary>
        public Flag Flags
        {
            get;
            set;
        }

        public virtual UsageFormat Usage
        {
            get;
            set;
        }

        /// <summary>
        /// The Symbol associated to this type if any: This for a Program or a Function or a TYPEDEF
        /// </summary>
        public TypeCobolSymbol Symbol
        {
            get;
            set;
        }

        /// <summary>
        /// The length of this type.
        /// </summary>
        public virtual int Length
        {
            get
            {
                switch (Usage)
                {
                    //Floating-point: Specifies for internal floating -point items (single precision)
                    //(i.e float in java, or C)
                    case UsageFormat.Comp1:
                        return 4;
                    //Long floating-point: Specifies for internal  floating point items(double precision)
                    //(i.e double in java or C)
                    case UsageFormat.Comp2:
                        return 8;
                    case UsageFormat.ObjectReference:
                        return 4;
                    case UsageFormat.FunctionPointer:
                        return 4;
                    case UsageFormat.Pointer:
                        return 4;
                    case UsageFormat.ProcedurePointer:
                        return 8;
                    case UsageFormat.Index:
                        return 4;
                    default:
                        throw new ArgumentException("Invalid Usage for type length calculation : " + Usage.ToString());
                }
            }
        }

        public SemanticKinds SemanticKind
        {
            get { return SemanticKinds.Type; }
        }
    }
}
