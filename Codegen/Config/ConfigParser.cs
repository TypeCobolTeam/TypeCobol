using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen.Config {
	/// <summary><see cref="Skeleton"/> parser interface.</summary>
	interface ConfigParser {
		List<Skeleton> Parse(string path);
	}
}
