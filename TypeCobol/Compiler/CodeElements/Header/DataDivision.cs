namespace TypeCobol.Compiler.CodeElements {

/// <summary>
/// The DATA DIVISION of a COBOL source program describes, in a structured manner, all the data to be processed by the program. 
/// </summary>
public class DataDivisionHeader: CodeElement {
	public DataDivisionHeader() : base(CodeElementType.DataDivisionHeader) { }
}


public abstract class DataSectionHeader: CodeElement {
    protected DataSectionHeader(CodeElementType type): base(type) { }
}

/// <summary>
/// The FILE SECTION defines the structure of data files.
/// </summary>
public class FileSectionHeader: DataSectionHeader {
	public FileSectionHeader() : base(CodeElementType.FileSectionHeader) { }
}

/// <summary>
/// The GLOBAL-STORAGE SECTION describes data records that are shared between procedure, data shared between a program and its nested programs
/// The GLOBAL-STORAGE SECTION has to be declared between FILE SECTION and WORKING-STORAGE SECTION
/// See issue #805 on GitHub
/// </summary>
public class GlobalStorageSectionHeader : DataSectionHeader {
    public GlobalStorageSectionHeader() : base(CodeElementType.GlobalStorageSectionHeader) { }
}

/// <summary>
/// The WORKING-STORAGE SECTION describes data records that are not part of data files but are developed and processed by a program or method. 
/// The WORKING-STORAGE SECTION also describes data items whose values are assigned in the source program or method and do not change during execution of the object program.
/// </summary>
public class WorkingStorageSectionHeader: DataSectionHeader {
	public WorkingStorageSectionHeader() : base(CodeElementType.WorkingStorageSectionHeader) { }
}


/// <summary>
/// The LOCAL-STORAGE SECTION defines storage that is allocated and freed on a per-invocation basis.
/// </summary>
public class LocalStorageSectionHeader: DataSectionHeader {
	public LocalStorageSectionHeader() : base(CodeElementType.LocalStorageSectionHeader) { }
}

/// <summary>
/// The LINKAGE SECTION describes data made available from another program or method.
/// </summary>
public class LinkageSectionHeader: DataSectionHeader {
	public LinkageSectionHeader() : base(CodeElementType.LinkageSectionHeader) { }
}



} // end of namespace TypeCobol.Compiler.CodeElements
