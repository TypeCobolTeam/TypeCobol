using System.Collections.Generic;

namespace TypeCobol.Codegen.Skeletons {

    /// <summary>
    /// A Pattern <==> a Template of code generation
    /// </summary>
	public class Pattern {
		/// <summary>Pattern identifier.</summary>
		public string Name      { get; internal set; }
		/// <summary>Should code in <paramref name="Template"/> be generated more than once?</summary>
		public string Group     { get; internal set; }
		/// <summary>URI in an abstract syntax tree.</summary>
		public string Location  { get; internal set; }
		/// <summary>What to do with the sourcecode</summary>
		public string Action  { get; internal set; }
		/// <summary>Variables usable in the <paramref name="Template"/>.</summary>
		public Dictionary<string,string> Variables { get; internal set; }
		/// <summary>Code template.</summary>
		public string Template  { get; internal set; }
		/// <summary>Variable Delimiter</summary>
		internal string Delimiter = "%";
        /// <summary>
        /// True if a new line must be generated before the template
        /// </summary>
        public bool NewLine { get; internal set; }
        /// <summary>
        /// Any boolean property that this pattern must satisfy before to be applied or not.
        /// A property must be of the form var.Property        
        /// </summary>
        public string BooleanProperty { get; internal set; }

        public int? Position { get; internal set; }

		internal bool Trim = true;
		internal bool Indent = true;

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			if (Name != null) str.Append(Name);
			if (Group != null) str.Append(" group:").Append(Group);
			if (Location != null) str.Append(" location:").Append(Location);
            if (Position != null) str.Append(" at position:").Append(Position);
            if (Action != null) str.Append(" action:").Append(Action);
            str.Append(" newline:").Append(NewLine);
            if (BooleanProperty != null) str.Append(" boolean_property:").Append(BooleanProperty);
			if (Variables.Count > 0) {
				str.Append(" variables: {");
				foreach(var kv in Variables)
					str.Append(' ').Append(kv.Key).Append(':').Append(kv.Value).Append(',');
				str.Length -= 1;
				str.Append(" }");
			}
			if (Template != null) str.Append(" template: \"").Append(Template).Append('"');
			return str.ToString();
		}

        /// <summary>
        /// Eval any boolean property
        /// </summary>
        /// <param name="properties">The set of properties</param>
        /// <returns>true if there is no property or if a property is evaluated to true, fals eotherwise.</returns>
        public bool EvalBooleanProperty(Dictionary<string, object> properties)
        {
            if (BooleanProperty == null)
                return true;
            if (properties == null)
                return false;
            string[] items = BooleanProperty.Split(new char[]{'.'});
            if (items.Length != 2)
                return false;
            string key = items[0];
            string prop_name = items[1];
            object value;
            if (!properties.TryGetValue(key, out value))
                return false;
            System.Type type = value.GetType();
            try
            {
                var prop = type.GetProperty(prop_name);
                if (prop == null)
                    return false;
                if (prop.PropertyType != typeof(bool))
                    return false;//We expect a Boolean type
                return (System.Boolean)prop.GetValue(value);
            }
            catch (System.Exception)
            {
                return false;
            }
        }
	}
}
